## --- Code to get info we want --- ##
library(tidyverse)
library(readxl)
library(dplyr)
library(janitor)
library(quantmod)

#PART3 -> You need to open the file and save it as Excel97 File with a Different Name....as the Wickham plugin isnt so great(It requires old Excel Versions- SaveAs in Excel)....OR you can you wget instead of download.
#PART3A-> I am going into the downloaded files and opening them in Excel and resaving them in 97 Version with a different file name... and wala they work again.... Maybe CSV is a better call but.. no Wickham...:)
setwd("C:/...") # set directory to where files are located
financials <- read_excel("financials2.xlsx")
automotives <- read_excel("autos2.xlsx")
industrials <- read_excel("industrials2.xlsx")
healthcares <- read_excel("health2.xlsx")
medias <- read_excel("media2.xlsx")

financials <- clean_names(financials)
automotives<- clean_names(automotives)
industrials <- clean_names(industrials)
healthcares <- clean_names(healthcares)
medias <- clean_names(medias)

## ---- pick the top x number of stocks with the highest dividend yield ---- ##
select_top_x <- function(data, x = 20){
  data <- data %>%
    arrange(desc(dividend_yield)) %>% 
    slice(1:x)
}

financials_filter <- select_top_x(financials)
automotives_filter <- select_top_x(automotives)
industrials_filter <- select_top_x(industrials)
healthcares_filter <- select_top_x(healthcares)
medias_filter <- select_top_x(medias)

financial_tick <- as.vector(financials_filter$symbol)
auto_tick <- as.vector(automotives_filter$symbol)
industry_tick<- as.vector(industrials_filter$symbol)
health_tick <- as.vector(healthcares_filter$symbol)
media_tick <- as.vector(medias_filter$symbol)
all_tickers <- c(financial_tick, auto_tick, industry_tick, health_tick, media_tick)

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
  
  
  # ---- Convert to long format for plotting ----
  price_long <- prices_all %>%
    mutate(Year = year(Date)) %>%
    group_by(Ticker, Year) %>%
    summarize(Price = Close[which.max(Date)], .groups = "drop")
  
  div_long <- dividends_all %>%
    mutate(Year = year(Date)) %>%
    group_by(Ticker, Year) %>%
    summarize(Dividend = sum(Dividend), .groups = "drop")
  
  # Return a list so you can plot both
  return(list(
    wide = final,
    price_long = price_long,
    div_long = div_long
  ))
}

fina_df <- financials %>% select(company_name, Ticker = symbol)
financials_final <- get_stock_data_wide(financial_tick, fina_df)

auto_df <- automotives %>% select(company_name, Ticker = symbol)
automotives_final <- get_stock_data_wide(auto_tick, auto_df)

industry_df <- industrials %>% select(company_name, Ticker = symbol)
industrials_final <- get_stock_data_wide(industry_tick, industry_df)

health_df <- healthcares %>% select(company_name, Ticker = symbol)
healthcares_final <- get_stock_data_wide(health_tick, health_df)

media_df <- medias %>% select(company_name, Ticker = symbol)
medias_final <- get_stock_data_wide(media_tick, media_df)

sectors <- list(financials_final, automotives_final, industrials_final, healthcares_final, medias_final)

price_plots <- list()
div_plots <- list()

for (i in seq_along(sectors)){
  
  sector_prices <- sectors[[i]]$price_long
  sector_divs <- sectors[[i]]$div_long
  
  plot_prices <- ggplot(sector_prices, aes(x = Year, y = Price, color = Ticker)) + 
    geom_line(size=1.1) + 
    geom_point() + 
    labs(title = 'Year-End Price Over Time', 
         y = 'Price on Dec 31 ($)') + 
    theme_minimal()
  
  plot_divs <- ggplot(sector_divs, aes(x = Year, y = Dividend, color = Ticker)) +
    geom_line(size = 1.1) +
    geom_point() +
    labs(title = "Total Dividends Paid Per Year ",
         y = "Total Dividends ($)") + 
    theme_minimal()
  
  price_plots[[i]] <- plot_prices
  div_plots[[i]] <- plot_divs
  
  print(plot_prices)
  print(plot_divs)
}

## --- get average to make industry plots --- ##

financials_final$price_long$sector <- "financial"
industrials_final$price_long$sector <- "industrial"
automotives_final$price_long$sector <- "auto"
healthcares_final$price_long$sector <- "health"
medias_final$price_long$sector <- "media"

all_prices <- bind_rows(
  financials_final$price_long,
  industrials_final$price_long,
  automotives_final$price_long,
  healthcares_final$price_long,
  medias_final$price_long
  )

financials_final$div_long$sector <- "financial"
industrials_final$div_long$sector <- "industrial"
automotives_final$div_long$sector <- "auto"
healthcares_final$div_long$sector <- "health"
medias_final$div_long$sector <- "media"

all_divs <- bind_rows(
  financials_final$div_long,
  industrials_final$div_long,
  automotives_final$div_long,
  healthcares_final$div_long,
  medias_final$div_long
  )

avg_prices_yearly <- all_prices %>%
  group_by(sector, Year) %>%
  summarize(avg_price = mean(Price, na.rm = TRUE), .groups = "drop")

avg_divs_yearly <- all_divs %>%
  group_by(sector, Year) %>%
  summarize(avg_div = mean(Dividend, na.rm = TRUE), .groups = "drop")

ggplot(avg_prices_yearly, aes(x = Year, y = avg_price, color = sector)) +
  geom_line(size = 1.3) +
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
theme(
  plot.title = element_text(face = "bold"),
  legend.position = "bottom"
)

normalized_prices <- all_prices %>%
  group_by(Ticker) %>%
  mutate(norm_price = Price / first(Price)) %>%
  ungroup()

avg_norm_prices <- normalized_prices %>%
  group_by(sector, Year) %>%
  summarize(avg_norm = mean(norm_price, na.rm = TRUE), .groups = "drop")

ggplot(avg_norm_prices, aes(x = Year, y = avg_norm, color = sector)) +
  geom_line(size = 1.3) +
  labs(
    title = "Normalized Stock Price Growth by Sector",
    y = "Growth (Relative to First Year)"
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggplot(avg_divs_yearly, aes(x = Year, y = avg_div, color = sector)) +
  geom_line(size = 1.3) +
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
