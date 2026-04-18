library(shiny)
library(bslib)
library(dplyr)
library(readxl)
library(DT)
library(ggplot2)
library(scales)
library(tibble)

# --- URL handling helpers ----------------------------------------------------

# Check if a path is an HTTP/HTTPS URL
is_url <- function(path) {
  grepl("^https?://", path, ignore.case = TRUE)
}

# Download a URL to a temporary file and return the temp file path
download_to_temp <- function(url) {
  temp <- tempfile(fileext = ".xlsx")
  tryCatch({
    download.file(url, temp, mode = "wb", quiet = TRUE)
    temp
  }, error = function(e) {
    stop(paste("Failed to download:", url, "|", conditionMessage(e)))
  })
}

# --- App root finder (still used for local fallback) -------------------------

find_app_root <- function() {
  start_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  current <- start_dir
  
  for (i in 1:8) {
    has_app <- file.exists(file.path(current, "app.R"))
    has_stage1 <- dir.exists(file.path(current, "Stage1Filtering"))
    if (has_app && has_stage1) {
      return(current)
    }
    
    parent <- dirname(current)
    if (identical(parent, current)) {
      break
    }
    current <- parent
  }
  
  start_dir
}

app_root <- find_app_root()

# --- GitHub base URL ---------------------------------------------------------

github_base <- "https://cyrusjones159.github.io/Cockey-Investments"

# --- Sector documents (ALL paths converted to GitHub URLs) -------------------

sector_documents <- list(
  Automotive = c(
    "Stage1 Consumer Discretionary" =
      paste0(github_base, "/Stage1Filtering/ConsumerDiscretionaryAutoscreener_results.xlsx"),
    "Stage1 Industrials" =
      paste0(github_base, "/Stage1Filtering/IndustrialsScreener_results.xlsx"),
    "Automotive Consumer Discretionary" =
      paste0(github_base, "/Automotive/ConsumerDiscretionaryAutoscreener_results.xlsx"),
    "Automotive Industrials" =
      paste0(github_base, "/Automotive/IndustrialsScreener_results.xlsx")
  ),
  Financials = c(
    "Stage1 Financials Screener" =
      paste0(github_base, "/Stage1Filtering/FinancialsScreener_results.xlsx"),
    "Financial Summary Output" =
      paste0(github_base, "/Financials/financials.result.xlsx"),
    "Financial Screener" =
      paste0(github_base, "/Financials/FinancialsScreener_results.xlsx")
  ),
  Healthcare = c(
    "Stage1 Healthcare Screener" =
      paste0(github_base, "/Stage1Filtering/HCscreener_results.xlsx"),
    "Healthcare Screener" =
      paste0(github_base, "/Healthcare/HCscreener_results.xlsx")
  ),
  Media = c(
    "Stage1 Media Entertainment" =
      paste0(github_base, "/Stage1Filtering/MediaEntertainmentScreener_results.xlsx"),
    "Media Entertainment Screener" =
      paste0(github_base, "/Media/MediaEntertainmentScreener_results.xlsx"),
    "Second Media Screener" =
      paste0(github_base, "/Media/screener_results.xlsx")
  )
)

# --- Resolve local paths (still used for fallback) ---------------------------

resolve_document_path <- function(path, root_dir) {
  if (grepl("^[A-Za-z]:[/\\]", path) || startsWith(path, "/")) {
    return(path)
  }
  file.path(root_dir, path)
}

# --- Load data (NOW SUPPORTS URL DOWNLOADS) ----------------------------------

load_sector_data <- function(path, root_dir) {
  
  # If the path is a URL, download it
  if (is_url(path)) {
    resolved_path <- download_to_temp(path)
  } else {
    resolved_path <- resolve_document_path(path, root_dir)
  }
  
  read_any_table <- function(target_path) {
    readxl::read_xlsx(target_path, guess_max = 5000) |>
      as_tibble()
  }
  
  if (!file.exists(resolved_path)) {
    return(list(
      data = tibble(),
      error = paste("File not found:", resolved_path),
      resolved_path = resolved_path
    ))
  }
  
  parsed <- tryCatch(
    read_any_table(resolved_path),
    error = function(e) {
      structure(list(message = conditionMessage(e)), class = "read_error")
    }
  )
  
  if (!inherits(parsed, "read_error")) {
    return(list(
      data = parsed,
      error = NULL,
      resolved_path = resolved_path
    ))
  }
  
  list(
    data = tibble(),
    error = paste("Failed to parse:", resolved_path, "|", parsed$message),
    resolved_path = resolved_path
  )
}

# --- Default selections -------------------------------------------------------

default_sector <- "Financials"
default_document <- names(sector_documents[[default_sector]])[1]

# --- UI ----------------------------------------------------------------------

ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Cockey Investments Explorer",
  tags$style(HTML("
    .company-filter-box .shiny-options-group {
      max-height: 160px;
      overflow-y: auto;
      padding-right: 6px;
      border: 1px solid #d9d9d9;
      border-radius: 6px;
      padding: 6px 8px;
      background: #ffffff;
    }
    .company-filter-box .checkbox {
      margin-top: 0;
      margin-bottom: 0;
    }
    .company-filter-actions {
      display: flex;
      gap: 8px;
      margin-bottom: 8px;
    }
    .company-filter-actions .btn {
      flex: 1;
    }
  ")),
  sidebar = sidebar(
    width = 330,
    helpText("Choose a sector, then choose one of the Excel documents in that sector."),
    selectInput(
      "sector",
      "Sector",
      choices = names(sector_documents),
      selected = default_sector
    ),
    selectInput(
      "document",
      "Excel document",
      choices = names(sector_documents[[default_sector]]),
      selected = default_document
    ),
    tags$div(
      class = "company-filter-actions",
      actionButton("select_all_symbols", "Select all"),
      actionButton("select_none_symbols", "Select none")
    ),
    uiOutput("symbol_filter_ui"),
    uiOutput("x_axis_ui"),
    uiOutput("y_axis_ui")
  ),
  layout_columns(
    col_widths = c(7, 5),
    card(
      card_header("Selected Spreadsheet"),
      card_body(DT::DTOutput("summary_table"))
    ),
    card(
      card_header("Row Preview"),
      card_body(
        uiOutput("row_preview_ui"),
        uiOutput("detail_panel")
      )
    )
  ),
  card(
    card_header("Numeric Snapshot"),
    card_body(plotOutput("numeric_plot", height = 420))
  )
)

# --- Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  observeEvent(input$sector, {
    document_choices <- names(sector_documents[[input$sector]])
    updateSelectInput(
      session,
      "document",
      choices = document_choices,
      selected = document_choices[1]
    )
  }, ignoreInit = TRUE)
  
  selected_path <- reactive({
    req(input$sector, input$document)
    sector_documents[[input$sector]][[input$document]]
  })
  
  selected_result <- reactive({
    load_sector_data(selected_path(), app_root)
  })
  
  selected_data <- reactive({
    selected_result()$data
  })
  
  numeric_columns <- reactive({
    names(dplyr::select(selected_data(), where(is.numeric)))
  })
  
  graph_data <- reactive({
    data <- selected_data()
    numeric_cols <- numeric_columns()
    
    if (nrow(data) == 0) return(data)
    
    data <- data %>%
      filter(if_all(where(is.character), ~ is.na(.x) | !grepl(
        "historical.*revision.*change at any time",
        .x,
        ignore.case = TRUE
      )))
    
    if (length(numeric_cols) > 0) {
      data <- data %>%
        filter(if_any(all_of(numeric_cols), ~ !is.na(.x)))
    }
    
    data
  })
  
  symbol_choices <- reactive({
    data <- graph_data()
    if (nrow(data) == 0) return(character())
    
    symbol_column <- intersect(c("symbol", "ticker", "Ticker", "Symbol"), names(data))[1]
    if (is.na(symbol_column)) return(character())
    
    sort(unique(as.character(data[[symbol_column]])))
  })
  
  observeEvent(input$select_all_symbols, {
    choices <- symbol_choices()
    if (length(choices) > 0) {
      updateCheckboxGroupInput(session, "symbols", selected = choices)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$select_none_symbols, {
    updateCheckboxGroupInput(session, "symbols", selected = character())
  }, ignoreInit = TRUE)
  
  filtered_data <- reactive({
    data <- graph_data()
    choices <- symbol_choices()
    
    if (length(choices) == 0 || !isTruthy(input$symbols)) return(data)
    
    symbol_column <- intersect(c("symbol", "ticker", "Ticker", "Symbol"), names(data))[1]
    if (is.na(symbol_column)) return(data)
    
    dplyr::filter(data, .data[[symbol_column]] %in% input$symbols)
  })
  
  output$symbol_filter_ui <- renderUI({
    choices <- symbol_choices()
    if (length(choices) == 0) return(NULL)
    
    div(
      class = "company-filter-box",
      checkboxGroupInput(
        "symbols",
        "Company Filter",
        choices = choices,
        selected = choices,
        inline = FALSE
      )
    )
  })
  
  output$x_axis_ui <- renderUI({
    choices <- numeric_columns()
    if (length(choices) == 0) return(NULL)
    
    selectInput("x_axis", "X axis", choices = choices, selected = choices[1])
  })
  
  output$y_axis_ui <- renderUI({
    choices <- numeric_columns()
    if (length(choices) == 0) return(NULL)
    
    selected_y <- if (length(choices) >= 2) choices[2] else choices[1]
    selectInput("y_axis", "Y axis", choices = choices, selected = selected_y)
  })
  
  output$summary_table <- DT::renderDT({
    DT::datatable(
      filtered_data(),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$row_preview_ui <- renderUI({
    data <- filtered_data()
    if (nrow(data) == 0) return(NULL)
    
    symbol_column <- intersect(c("symbol", "ticker", "Ticker", "Symbol"), names(data))[1]
    if (is.na(symbol_column)) return(NULL)
    
    symbol_choices <- sort(unique(as.character(data[[symbol_column]])))
    
    selectizeInput(
      "preview_symbol",
      "Preview",
      choices = symbol_choices,
      selected = symbol_choices[1],
      options = list(
        placeholder = "Search or type a value",
        maxOptions = 25
      )
    )
  })
  
  output$detail_panel <- renderUI({
    data <- filtered_data()
    if (nrow(data) == 0) return(tags$p("No rows found in the selected file."))
    
    symbol_column <- intersect(c("symbol", "ticker", "Ticker", "Symbol"), names(data))[1]
    if (is.na(symbol_column)) return(tags$p("No Symbol column was found in the selected file."))
    
    selected_symbol <- input$preview_symbol
    if (is.null(selected_symbol) || !(selected_symbol %in% as.character(data[[symbol_column]]))) {
      selected_symbol <- as.character(data[[symbol_column]][1])
    }
    
    selected_row <- which(as.character(data[[symbol_column]]) == selected_symbol)[1]
    if (is.na(selected_row)) selected_row <- 1L
    
    row <- data[selected_row, , drop = FALSE]
    fields <- lapply(names(row), function(column_name) {
      if (tolower(column_name) %in% c("symbol", "ticker")) return(NULL)
      
      value <- row[[column_name]][1]
      tags$p(tags$strong(paste0(column_name, ": ")), as.character(value))
    })
    
    tagList(Filter(Negate(is.null), fields))
  })
  
  output$numeric_plot <- renderPlot({
    data <- filtered_data()
    numeric_cols <- numeric_columns()
    
    validate(need(nrow(data) > 0, "No data available for the selected file."))
    
    if (length(numeric_cols) == 0) {
      plot.new()
      text(0.5, 0.5, "The selected file has no numeric columns to chart.")
      return()
    }
    
    x_column <- if (!is.null(input$x_axis) && input$x_axis %in% numeric_cols) input$x_axis else numeric_cols[1]
    y_column <- if (!is.null(input$y_axis) && input$y_axis %in% numeric_cols) input$y_axis else {
      if (length(numeric_cols) >= 2) numeric_cols[2] else numeric_cols[1]
    }
    
    if (identical(x_column, y_column)) {
      ggplot(data, aes(x = .data[[x_column]])) +
        geom_histogram(bins = 20, fill = "#2A9D8F", color = "white") +
        scale_x_continuous(labels = dollar) +
        labs(x = x_column, y = "Count") +
        theme_minimal(base_size = 12)
    } else {
      ggplot(data, aes(x = .data[[x_column]], y = .data[[y_column]])) +
        geom_point(alpha = 0.75, color = "#1D3557") +
        scale_x_continuous(labels = dollar) +
        scale_y_continuous(labels = dollar) +
        labs(x = x_column, y = y_column) +
        theme_minimal(base_size = 12)
    }
  })
}

shinyApp(ui, server)
