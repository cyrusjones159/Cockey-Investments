install.packages("tidyverse")
install.packages("readxl")
install.packages("readr")
install.packages("wget")
install.packages("dplyr")
install.packages("sqldf")
install.packages("janitor")
install.packages("quantmod")
install.packages("purrr")
install.packages("lubridate")
install.packages("tidyr")
install.packages("writexl")
library(writexl)
library(lubridate)
library(tidyr)
library(quantmod)
library(purrr)
library(tidyverse)
library(readr)
library(readxl)
library(wget) #THIS REQUIRES WGET ON YOU MAC OR WINDOWS BEING INSTALLED... OTHERWISE USE R.DOWNLOAD.FILE
library(dplyr)
library(sqldf)
library(janitor)
download.file("https://cyrusjones159.github.io/Cockey-Investments/Financials/FinancialsScreener_results.xls", "c:\\users\\stritzj\\Documents\\542\\financials2.xls", mode = "wb")
mydf <- read_excel("c:/users/stritzj/Documents/542/financials3.xls")
head(mydf, 5)
str(mydf)

mydf2 <- clean_names(mydf)
head(mydf2, 5)
result <- sqldf("SELECT * FROM mydf2 WHERE company_name = 'Bank OZK'")
result2 <- sqldf("SELECT symbol, dividend_yield FROM mydf2 WHERE dividend_yield > 4 Order by dividend_yield DESC")
head(result,5)
result2
tickers <- as.vector(result2$symbol)
tickers

get_stock_data_wide <- function(tickers, company_df, years = 10) {
  from_date <- Sys.Date() - (365 * years)
  
  price_list <- list()
  dividend_list <- list()
  
  for (t in tickers) {
    
    # ---- Price Data ----
    getSymbols(t, from = from_date, auto.assign = TRUE, warnings = FALSE)
    prices <- get(t)
    
    prices_df <- data.frame(
      Date = index(prices),
      Close = as.numeric(prices[, 4]),
      Ticker = t
    )
    
    price_list[[t]] <- prices_df
    
    # ---- Dividend Data ----
    divs <- getDividends(t, from = from_date)
    divs_df <- data.frame(
      Date = index(divs),
      Dividend = as.numeric(divs),
      Ticker = t
    )
    
    dividend_list[[t]] <- divs_df
  }
  
  prices_all <- bind_rows(price_list)
  dividends_all <- bind_rows(dividend_list)
  
  # ---- Year-end price ----
  prices_yearly <- prices_all %>%
    mutate(Year = year(Date)) %>%
    group_by(Ticker, Year) %>%
    summarize(
      Price_12_31 = Close[which.max(Date)],
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = Year,
      values_from = Price_12_31,
      names_prefix = "Price_"
    )
  
  # ---- Total dividends per year ----
  dividends_yearly <- dividends_all %>%
    mutate(Year = year(Date)) %>%
    group_by(Ticker, Year) %>%
    summarize(
      Dividend_Total = sum(Dividend),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = Year,
      values_from = Dividend_Total,
      names_prefix = "Div_"
    )
  
  # ---- Merge price + dividend tables ----
  final <- prices_yearly %>%
    left_join(dividends_yearly, by = "Ticker") %>%
    left_join(company_df, by = "Ticker") %>%   # add company names
    select(Company = company_name, Ticker, everything())
  
  return(final)
}

#result3 <- get_stock_data(tickers)
company_df <- mydf2 %>% select(company_name, Ticker = symbol)
result3 <- get_stock_data_wide(tickers, company_df)
head(result3, 20)

# MUTATE THREE TO CALCULATE THE AVG DIVIDEND OVER 10 YEARS, THE STARTING PRICE, THE ENDING PRICE IN YEAR 10, AND EQUITY CHANGES.
result4 <- result3 %>%
  mutate(
    # Average dividend across all Div_* columns
    avgdividend = rowMeans(select(., starts_with("Div_")), na.rm = TRUE),
    
    # Total dividends across all 10 years
    totaldividends = rowSums(select(., starts_with("Div_")), na.rm = TRUE),
    
    # Identify price columns
    price_start = select(., starts_with("Price_"))[[1]],
    price_end   = select(., starts_with("Price_"))[[ncol(select(., starts_with("Price_")))]],
    
    # Total change in equity value
    change = price_end - price_start
  )
result4

# FINAL SELECT HAS JUST SOME OF THE FIELDS AS YOU MAY WANT TO RESTRICT TO ANY YEAR A NEGATIVE EQUITY WAS PRESENT
result5 <- result4 %>%
  select(
    Company,
    Ticker,
    avgdividend,
    totaldividends,
    price_start,
    price_end,
    change
  )
result5
print(result5)
# WRITE THE RESULTS BACK TO DISK
write_xlsx(result5, "c:/users/stritzj/Documents/542/financials.result.xlsx")
