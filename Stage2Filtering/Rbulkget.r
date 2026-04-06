library(quantmod)
library(dplyr)
library(purrr)

get_stock_data <- function(tickers, years = 10) {
  from_date <- Sys.Date() - (365 * years)

  price_list <- list()
  dividend_list <- list()

  for (t in tickers) {
    # Price data
    getSymbols(t, from = from_date, auto.assign = TRUE, warnings = FALSE)
    prices <- get(t)
    prices_df <- data.frame(Date = index(prices), coredata(prices))
    prices_df$Ticker <- t
    price_list[[t]] <- prices_df

    # Dividend data
    divs <- getDividends(t, from = from_date)
    divs_df <- data.frame(Date = index(divs), Dividend = coredata(divs))
    divs_df$Ticker <- t
    dividend_list[[t]] <- divs_df
  }

  prices_all <- bind_rows(price_list)
  dividends_all <- bind_rows(dividend_list)

  return(list(prices = prices_all, dividends = dividends_all))
}

# Example usage
tickers <- c("BAC", "WFC", "JPM")
result <- get_stock_data(tickers)

head(result$prices)
head(result$dividends)
