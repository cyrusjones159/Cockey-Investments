library(shiny)
library(bslib)
library(dplyr)
library(readxl)
library(DT)
library(ggplot2)
library(scales)
library(tibble)
library(tidyr)
library(janitor)
library(lubridate)
library(purrr)
library(quantmod)

utils::globalVariables(c(
  "dividend_yield", "Ticker", "Year", "Close", "Price_12_31", "Dividend",
  "Dividend_Total", "company_name", "symbol", "sector", "Price",
  "norm_price", "avg_price", "avg_div", "avg_norm"
))

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
      paste0(github_base, "/Automotive/IndustrialsScreener_results.xlsx"),
    "Final Automotive Screener" = 
      paste0(github_base, "/ProductionPartner/fullviewautos.result.xlsx")
  ),
  Financials = c(
    "Stage1 Financials Screener" =
      paste0(github_base, "/Stage1Filtering/FinancialsScreener_results.xlsx"),
    "Financial Summary Output" =
      paste0(github_base, "/Financials/financials.result.xlsx"),
    "Financial Screener" =
      paste0(github_base, "/Financials/FinancialsScreener_results.xlsx"),
    "Final Financials Screener" =
      paste0(github_base, "/ProductionPartner/fullviewfinancials.result.xlsx")
  ),
  Healthcare = c(
    "Stage1 Healthcare Screener" =
      paste0(github_base, "/Stage1Filtering/HCscreener_results.xlsx"),
    "Healthcare Screener" =
      paste0(github_base, "/Healthcare/HCscreener_results.xlsx"),
    "Final Healthcare Screener" =
      paste0(github_base, "/ProductionPartner/fullviewhealth.result.xlsx")
  ),
  Media = c(
    "Stage1 Media Entertainment" =
      paste0(github_base, "/Stage1Filtering/MediaEntertainmentScreener_results.xlsx"),
    "Media Entertainment Screener" =
      paste0(github_base, "/Media/MediaEntertainmentScreener_results.xlsx"),
    "Second Media Screener" =
      paste0(github_base, "/Media/screener_results.xlsx"),
    "Final Media Screener" =
      paste0(github_base, "/ProductionPartner/media.result.xlsx")
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

# --- Plot analytics helpers (ported from Plots.R) ----------------------------

select_top_x <- function(data, x = 20) {
  if (!"dividend_yield" %in% names(data)) {
    return(dplyr::slice_head(data, n = min(x, nrow(data))))
  }
  
  data |>
    arrange(desc(dividend_yield)) |>
    slice_head(n = x)
}

get_stock_data_wide <- function(tickers, company_df, years = 10) {
  from_date <- Sys.Date() - (365 * years)
  
  price_list <- list()
  dividend_list <- list()
  
  for (t in tickers) {
    fetched <- tryCatch({
      suppressWarnings(getSymbols(t, from = from_date, auto.assign = FALSE))
    }, error = function(e) NULL)
    
    if (is.null(fetched)) {
      next
    }
    
    prices_df <- data.frame(
      Date = as.Date(time(fetched)),
      Close = as.numeric(Cl(fetched)),
      Ticker = t
    )
    
    price_list[[t]] <- prices_df
    
    divs <- tryCatch(
      getDividends(t, from = from_date),
      error = function(e) NULL
    )
    
    if (!is.null(divs) && length(divs) > 0) {
      divs_df <- data.frame(
        Date = as.Date(time(divs)),
        Dividend = as.numeric(divs),
        Ticker = t
      )
      dividend_list[[t]] <- divs_df
    }
  }
  
  if (length(price_list) == 0) {
    return(list(
      wide = tibble(),
      price_long = tibble(),
      div_long = tibble()
    ))
  }
  
  prices_all <- bind_rows(price_list)
  dividends_all <- if (length(dividend_list) > 0) bind_rows(dividend_list) else tibble()
  
  prices_yearly <- prices_all |>
    mutate(Year = year(Date)) |>
    group_by(Ticker, Year) |>
    summarize(
      Price_12_31 = Close[which.max(Date)],
      .groups = "drop"
    ) |>
    pivot_wider(
      names_from = Year,
      values_from = Price_12_31,
      names_prefix = "Price_"
    )
  
  dividends_yearly <- if (nrow(dividends_all) > 0) {
    dividends_all |>
      mutate(Year = year(Date)) |>
      group_by(Ticker, Year) |>
      summarize(
        Dividend_Total = sum(Dividend, na.rm = TRUE),
        .groups = "drop"
      ) |>
      pivot_wider(
        names_from = Year,
        values_from = Dividend_Total,
        names_prefix = "Div_"
      )
  } else {
    tibble(Ticker = unique(prices_all$Ticker))
  }
  
  final <- prices_yearly |>
    left_join(dividends_yearly, by = "Ticker") |>
    left_join(company_df, by = "Ticker") |>
    select(Company = company_name, Ticker, everything())
  
  price_long <- prices_all |>
    mutate(Year = year(Date)) |>
    group_by(Ticker, Year) |>
    summarize(Price = Close[which.max(Date)], .groups = "drop")
  
  div_long <- if (nrow(dividends_all) > 0) {
    dividends_all |>
      mutate(Year = year(Date)) |>
      group_by(Ticker, Year) |>
      summarize(Dividend = sum(Dividend, na.rm = TRUE), .groups = "drop")
  } else {
    tibble(Ticker = character(), Year = integer(), Dividend = numeric())
  }
  
  list(
    wide = final,
    price_long = price_long,
    div_long = div_long
  )
}

# --- Default selections -------------------------------------------------------

default_sector <- "Financials"
default_document <- names(sector_documents[[default_sector]])[4]

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
  navset_tab(
    nav_panel(
      "Spreadsheet view",
      layout_columns(
        col_widths = c(7, 5),
        card(
          full_screen = TRUE,
          card_header("Selected Spreadsheet"),
          card_body(DT::DTOutput("summary_table"))
        ),
        card(
          full_screen = TRUE,
          card_header("Row Preview"),
          card_body(
            uiOutput("row_preview_ui"),
            uiOutput("detail_panel")
          )
        )
      )
    ),
    nav_panel(
      "Snapshot View",
      card(
        full_screen = TRUE,
        card_header("Numeric Snapshot"),
        card_body(plotOutput("numeric_plot", height = 420))
      )
    ),
    nav_panel(
      "Plot Analytics",
      card(
        full_screen = TRUE,
        card_header("Plot Analytics"),
        card_body(
          navset_pill(
            nav_panel("Sector Price", plotOutput("sector_price_plot", height = 430)),
            nav_panel("Sector Dividends", plotOutput("sector_div_plot", height = 430)),
            nav_panel("Average Year-End Price", plotOutput("avg_sector_price_plot", height = 430)),
            nav_panel("Normalized Growth", plotOutput("normalized_growth_plot", height = 430)),
            nav_panel("Average Annual Dividends", plotOutput("avg_sector_div_plot", height = 430))
          )
        )
      )
    )
  )
)

# --- Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  sector_plot_sources <- list(
    Financials = c(
      sector_documents$Financials[["Financial Screener"]]
    ),
    Automotive = c(
      sector_documents$Automotive[["Automotive Consumer Discretionary"]],
      sector_documents$Automotive[["Automotive Industrials"]]
    ),
    Healthcare = c(
      sector_documents$Healthcare[["Healthcare Screener"]]
    ),
    Media = c(
      sector_documents$Media[["Media Entertainment Screener"]]
    )
  )
  
  stock_plot_data <- reactive({
    sector_results <- lapply(names(sector_plot_sources), function(sector_name) {
      source_paths <- sector_plot_sources[[sector_name]]
      
      sector_raw <- bind_rows(lapply(source_paths, function(path) {
        loaded <- load_sector_data(path, app_root)
        if (!is.null(loaded$error)) {
          return(tibble())
        }
        janitor::clean_names(loaded$data)
      }))
      
      if (nrow(sector_raw) == 0 || !"symbol" %in% names(sector_raw)) {
        return(NULL)
      }
      
      sector_filtered <- select_top_x(sector_raw, x = 20)
      tickers <- sector_filtered |>
        pull(symbol) |>
        as.character() |>
        unique() |>
        discard(~ is.na(.x) || .x == "")
      
      if (length(tickers) == 0) {
        return(NULL)
      }
      
      company_df <- sector_filtered |>
        transmute(
          Ticker = as.character(symbol),
          company_name = if ("company_name" %in% names(sector_filtered)) as.character(company_name) else as.character(symbol)
        ) |>
        distinct(Ticker, .keep_all = TRUE)
      
      sector_data <- get_stock_data_wide(tickers, company_df)
      if (nrow(sector_data$price_long) == 0) {
        return(NULL)
      }
      
      sector_data$price_long <- sector_data$price_long |>
        mutate(sector = sector_name)
      
      sector_data$div_long <- sector_data$div_long |>
        mutate(sector = sector_name)
      
      sector_data
    })
    
    names(sector_results) <- names(sector_plot_sources)
    valid_results <- sector_results[!vapply(sector_results, is.null, logical(1))]
    
    if (length(valid_results) == 0) {
      return(list(
        by_sector = list(),
        all_prices = tibble(),
        all_divs = tibble(),
        avg_prices_yearly = tibble(),
        avg_divs_yearly = tibble(),
        avg_norm_prices = tibble()
      ))
    }
    
    all_prices <- bind_rows(lapply(valid_results, function(x) x$price_long))
    all_divs <- bind_rows(lapply(valid_results, function(x) x$div_long))
    
    avg_prices_yearly <- all_prices |>
      group_by(sector, Year) |>
      summarize(avg_price = mean(Price, na.rm = TRUE), .groups = "drop")
    
    avg_divs_yearly <- if (nrow(all_divs) > 0) {
      all_divs |>
        group_by(sector, Year) |>
        summarize(avg_div = mean(Dividend, na.rm = TRUE), .groups = "drop")
    } else {
      tibble()
    }
    
    normalized_prices <- all_prices |>
      group_by(Ticker) |>
      arrange(Year, .by_group = TRUE) |>
      mutate(norm_price = Price / first(Price)) |>
      ungroup()
    
    avg_norm_prices <- normalized_prices |>
      group_by(sector, Year) |>
      summarize(avg_norm = mean(norm_price, na.rm = TRUE), .groups = "drop")
    
    list(
      by_sector = valid_results,
      all_prices = all_prices,
      all_divs = all_divs,
      avg_prices_yearly = avg_prices_yearly,
      avg_divs_yearly = avg_divs_yearly,
      avg_norm_prices = avg_norm_prices
    )
  })
  
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
    choices <- as.character(numeric_columns())
    if (length(choices) == 0) return(NULL)
    
    selected_y <- if (!is.na(choices[2])) choices[2] else choices[1]
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
    numeric_cols <- as.character(numeric_columns())
    
    validate(need(nrow(data) > 0, "No data available for the selected file."))
    
    if (length(numeric_cols) == 0) {
      plot.new()
      text(0.5, 0.5, "The selected file has no numeric columns to chart.")
      return()
    }
    
    x_column <- if (!is.null(input$x_axis) && input$x_axis %in% numeric_cols) input$x_axis else numeric_cols[1]
    y_column <- if (!is.null(input$y_axis) && input$y_axis %in% numeric_cols) {
      input$y_axis
    } else {
      if (!is.na(numeric_cols[2])) numeric_cols[2] else numeric_cols[1]
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
  
  output$sector_price_plot <- renderPlot({
    plot_data <- stock_plot_data()
    sector_data <- plot_data$by_sector[[input$sector]]
    
    validate(need(!is.null(sector_data), "No stock history was available for the selected sector."))
    validate(need(nrow(sector_data$price_long) > 0, "No stock history was available for the selected sector."))
    
    ggplot(sector_data$price_long, aes(x = Year, y = Price, color = Ticker)) +
      geom_line(linewidth = 1.05) +
      geom_point(size = 1.6) +
      labs(
        title = paste("Year-End Price Over Time:", input$sector),
        y = "Price on Dec 31 ($)",
        x = "Year"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })
  
  output$sector_div_plot <- renderPlot({
    plot_data <- stock_plot_data()
    sector_data <- plot_data$by_sector[[input$sector]]
    
    validate(need(!is.null(sector_data), "No dividend history was available for the selected sector."))
    validate(need(nrow(sector_data$div_long) > 0, "No dividend history was available for the selected sector."))
    
    ggplot(sector_data$div_long, aes(x = Year, y = Dividend, color = Ticker)) +
      geom_line(linewidth = 1.05) +
      geom_point(size = 1.6) +
      labs(
        title = paste("Total Dividends Paid Per Year:", input$sector),
        y = "Total Dividends ($)",
        x = "Year"
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })
  
  output$avg_sector_price_plot <- renderPlot({
    avg_prices <- stock_plot_data()$avg_prices_yearly
    validate(need(nrow(avg_prices) > 0, "No sector price summary is available yet."))
    
    ggplot(avg_prices, aes(x = Year, y = avg_price, color = sector)) +
      geom_line(linewidth = 1.25) +
      geom_point(size = 2) +
      labs(
        title = "Average Year-End Stock Price by Sector",
        x = "Year",
        y = "Average Price ($)",
        color = "Sector"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
  })
  
  output$normalized_growth_plot <- renderPlot({
    avg_norm_prices <- stock_plot_data()$avg_norm_prices
    validate(need(nrow(avg_norm_prices) > 0, "No normalized growth summary is available yet."))
    
    ggplot(avg_norm_prices, aes(x = Year, y = avg_norm, color = sector)) +
      geom_line(linewidth = 1.25) +
      labs(
        title = "Normalized Stock Price Growth by Sector",
        y = "Growth (Relative to First Year)",
        x = "Year",
        color = "Sector"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
  })
  
  output$avg_sector_div_plot <- renderPlot({
    avg_divs <- stock_plot_data()$avg_divs_yearly
    validate(need(nrow(avg_divs) > 0, "No sector dividend summary is available yet."))
    
    ggplot(avg_divs, aes(x = Year, y = avg_div, color = sector)) +
      geom_line(linewidth = 1.25) +
      geom_point(size = 2) +
      labs(
        title = "Average Annual Dividends by Sector",
        x = "Year",
        y = "Average Dividends ($)",
        color = "Sector"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
  })
}

shinyApp(ui, server)