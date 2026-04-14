library(shiny)
library(bslib)
library(dplyr)
library(readxl)
library(DT)
library(ggplot2)
library(scales)
library(tibble)

# Find the project root so relative file paths work even if Shiny launches from another folder.
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

# Menu of sector files that appear in the sidebar dropdown.
sector_documents <- list(
  Automotive = c(
    "Stage1 Consumer Discretionary" = "Stage1Filtering/ConsumerDiscretionaryAutoscreener_results.xlsx",
    "Stage1 Industrials" = "Stage1Filtering/IndustrialsScreener_results.xlsx",
    "Automotive Consumer Discretionary" = "Automotive/ConsumerDiscretionaryAutoscreener_results.xlsx",
    "Automotive Industrials" = "Automotive/IndustrialsScreener_results.xlsx"
  ),
  Financials = c(
    "Stage1 Financials Screener" = "Stage1Filtering/FinancialsScreener_results.xlsx",
    "Financial Summary Output" = "Financials/financials.result.xlsx",
    "Financial Screener" = "Financials/FinancialsScreener_results.xlsx"
  ),
  Healthcare = c(
    "Stage1 Healthcare Screener" = "Stage1Filtering/HCscreener_results.xlsx",
    "Healthcare Screener" = "Healthcare/HCscreener_results.xlsx"
  ),
  Media = c(
    "Stage1 Media Entertainment" = "Stage1Filtering/MediaEntertainmentScreener_results.xlsx",
    "Media Entertainment Screener" = "Media/MediaEntertainmentScreener_results.xlsx",
    "Second Media Screener" = "Media/screener_results.xlsx"
  )
)

# Convert a relative path like "Stage1Filtering/file.xlsx" to an absolute local path.
resolve_document_path <- function(path, root_dir) {
  if (grepl("^[A-Za-z]:[/\\]", path) || startsWith(path, "/")) {
    return(path)
  }
  file.path(root_dir, path)
}

# Load one spreadsheet from disk and return both data and status info for the UI.
load_sector_data <- function(path, root_dir) {
  resolved_path <- resolve_document_path(path, root_dir)

  # Read the selected spreadsheet as xlsx.
  read_any_table <- function(target_path) {
    readxl::read_xlsx(target_path, guess_max = 5000) |>
      as_tibble()
  }

  # Fail early if the selected xlsx path does not exist.
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

# Default sidebar choices when the app first loads.
default_sector <- "Financials"
default_document <- names(sector_documents[[default_sector]])[1]

# UI layout: selectors in the sidebar, data table/details/plot in the main area.
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Cockey Investments Explorer",
  tags$style(HTML("\n    .company-filter-box .shiny-options-group {\n      max-height: 160px;\n      overflow-y: auto;\n      padding-right: 6px;\n      border: 1px solid #d9d9d9;\n      border-radius: 6px;\n      padding: 6px 8px;\n      background: #ffffff;\n    }\n    .company-filter-box .checkbox {\n      margin-top: 0;\n      margin-bottom: 0;\n    }\n    .company-filter-actions {\n      display: flex;\n      gap: 8px;\n      margin-bottom: 8px;\n    }\n    .company-filter-actions .btn {\n      flex: 1;\n    }\n  ")),
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

# Server logic: react to user selections and feed outputs.
server <- function(input, output, session) {
  # When sector changes, repopulate document choices for that sector.
  observeEvent(input$sector, {
    document_choices <- names(sector_documents[[input$sector]])
    updateSelectInput(
      session,
      "document",
      choices = document_choices,
      selected = document_choices[1]
    )
  }, ignoreInit = TRUE)

  # Current file key chosen in the sidebar (relative path from sector_documents).
  selected_path <- reactive({
    req(input$sector, input$document)
    sector_documents[[input$sector]][[input$document]]
  })

  # Read selected file and carry data plus error context together.
  selected_result <- reactive({
    load_sector_data(selected_path(), app_root)
  })

  # Main dataset used by table, detail panel, and chart.
  selected_data <- reactive({
    selected_result()$data
  })

  # Symbol values drive the checkbox filter in the sidebar.
  symbol_choices <- reactive({
    data <- graph_data()

    if (nrow(data) == 0) {
      return(character())
    }

    symbol_column <- intersect(c("symbol", "ticker", "Ticker", "Symbol"), names(data))[1]

    if (is.na(symbol_column) || length(symbol_column) == 0) {
      return(character())
    }

    sort(unique(as.character(data[[symbol_column]])))
  })

  # Give the user quick controls for selecting all symbols or clearing the list.
  observeEvent(input$select_all_symbols, {
    choices <- symbol_choices()

    if (length(choices) > 0) {
      updateCheckboxGroupInput(session, "symbols", selected = choices)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$select_none_symbols, {
    updateCheckboxGroupInput(session, "symbols", selected = character())
  }, ignoreInit = TRUE)

  # Numeric columns drive the chart type and axes.
  numeric_columns <- reactive({
    names(dplyr::select(selected_data(), where(is.numeric)))
  })

  # Keep only rows that belong in the displayed analysis and chart.
  graph_data <- reactive({
    data <- selected_data()
    numeric_cols <- numeric_columns()

    if (nrow(data) == 0) {
      return(data)
    }

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

  # Filter the current dataset by the selected symbols, when that column exists.
  filtered_data <- reactive({
    data <- graph_data()
    choices <- symbol_choices()

    if (length(choices) == 0 || !isTruthy(input$symbols)) {
      return(data)
    }

    symbol_column <- intersect(c("symbol", "ticker", "Ticker", "Symbol"), names(data))[1]
    if (is.na(symbol_column) || length(symbol_column) == 0) {
      return(data)
    }

    dplyr::filter(data, .data[[symbol_column]] %in% input$symbols)
  })

  # Build the company checkbox list from the currently selected spreadsheet.
  output$symbol_filter_ui <- renderUI({
    choices <- symbol_choices()

    if (length(choices) == 0) {
      return(NULL)
    }

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

  # Build the x-axis selector from the numeric columns in the current file.
  output$x_axis_ui <- renderUI({
    choices <- numeric_columns()

    if (length(choices) == 0) {
      return(NULL)
    }

    selectInput(
      "x_axis",
      "X axis",
      choices = choices,
      selected = choices[1]
    )
  })

  # Build the y-axis selector from the numeric columns in the current file.
  output$y_axis_ui <- renderUI({
    choices <- numeric_columns()

    if (length(choices) == 0) {
      return(NULL)
    }

    selected_y <- if (length(choices) >= 2) choices[2] else choices[1]

    selectInput(
      "y_axis",
      "Y axis",
      choices = choices,
      selected = selected_y
    )
  })

  # Interactive table of the full selected dataset.
  output$summary_table <- DT::renderDT({
    DT::datatable(
      filtered_data(),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  # Quick glance panel using a Symbol selector from the current filtered rows.
  output$row_preview_ui <- renderUI({
    data <- filtered_data()

    if (nrow(data) == 0) {
      return(NULL)
    }

    symbol_column <- intersect(c("symbol", "ticker", "Ticker", "Symbol"), names(data))[1]
    if (is.na(symbol_column) || length(symbol_column) == 0) {
      return(NULL)
    }

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

  # Show the row matching the selected Symbol, defaulting to the first match.
  output$detail_panel <- renderUI({
    data <- filtered_data()

    if (nrow(data) == 0) {
      return(tags$p("No rows found in the selected file."))
    }

    symbol_column <- intersect(c("symbol", "ticker", "Ticker", "Symbol"), names(data))[1]
    if (is.na(symbol_column) || length(symbol_column) == 0) {
      return(tags$p("No Symbol column was found in the selected file."))
    }

    selected_symbol <- input$preview_symbol
    if (is.null(selected_symbol) || !(selected_symbol %in% as.character(data[[symbol_column]]))) {
      selected_symbol <- as.character(data[[symbol_column]][1])
    }

    selected_row <- which(as.character(data[[symbol_column]]) == selected_symbol)[1]
    if (is.na(selected_row) || length(selected_row) == 0) {
      selected_row <- 1L
    }

    row <- data[selected_row, , drop = FALSE]
    fields <- lapply(names(row), function(column_name) {
      if (tolower(column_name) %in% c("symbol", "ticker")) {
        return(NULL)
      }

      value <- row[[column_name]][1]
      tags$p(tags$strong(paste0(column_name, ": ")), as.character(value))
    })

    tagList(Filter(Negate(is.null), fields))
  })

  # Plot behavior:
  # - no numeric columns: text message
  # - one numeric column: histogram
  # - two or more numeric columns: scatterplot of first two
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
#Starts the ShinyApp
shinyApp(ui, server)