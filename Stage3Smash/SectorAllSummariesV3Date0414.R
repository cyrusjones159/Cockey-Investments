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
#PART1-> WE USED FIDELITY INVESTMENTS STOCK SCREENER TO PARE DOWN TARGET INVESTMENTS (SEE SCREENSHOTS FOR DETAILS)
#PART2 -> PROCESS AND DOWNLOAD ALL FILES LOCALLY (SOURCE PATH-> DESTINATION PATH)
download.file("https://cyrusjones159.github.io/Cockey-Investments/Financials/FinancialsScreener_results.xls", ".\\financials2.xls", mode = "wb")
download.file("https://cyrusjones159.github.io/Cockey-Investments/Automotive/ConsumerDiscretionaryAutoscreener_results.xls", ".\\autos2.xls", mode = "wb")
download.file("https://cyrusjones159.github.io/Cockey-Investments/Automotive/IndustrialsScreener_results.xls", ".\\industrials2.xls", mode = "wb")
download.file("https://cyrusjones159.github.io/Cockey-Investments/Healthcare/HCscreener_results.xls", ".\\health2.xls", mode = "wb")
download.file("https://cyrusjones159.github.io/Cockey-Investments/Media/MediaEntertainmentScreener_results.xls", ".\\media2.xls", mode = "wb")
#PART3 CONVERT DOWNLOADED FILES TO XLS VERSION 97 THE RESULT IS THE SAME NAME WITH A READABLE FORMAT
#THIS REQUIRES OPENOFFICE IN THE CURRENT PATH - NOTE LIBREOFFICE CAN DO THE SAME THING.
convert_with_openoffice <- function(input, output_dir = ".", soffice = "soffice") {
  cmd <- c(
    "--headless",
    "--convert-to", 'xls:"MS Excel 97"',
    shQuote(normalizePath(input)),
    "--outdir", shQuote(normalizePath(output_dir))
  )
  system2(soffice, args = cmd)
}
#THIS IS MAGIC... IT WORKS
convert_with_openoffice("financials2.xls")
convert_with_openoffice("autos2.xls")
convert_with_openoffice("industrials2.xls")
convert_with_openoffice("health2.xls")
convert_with_openoffice("media2.xls")




#PART3 -> You need to open the file and save it as Excel97 File with a Different Name....as the Wickham plugin isnt so great(It requires old Excel Versions- SaveAs in Excel)....OR you can you wget instead of download.
#PART3A-> I am going into the downloaded files and opening them in Excel and resaving them in 97 Version with a different file name... and wala they work again.... Maybe CSV is a better call but.. no Wickham...:)
mydf <- read_excel(".//financials3.xls")
automydf <- read_excel("autos3.xls")
indmydf <- read_excel("industrials3.xls")
healthmydf <- read_excel("health3.xls")
mediamydf <- read_excel("media3.xls")
head(mydf, 5)
#  YOU CAN START HERE AS I CONVERTED THE FILES FOR YOU ALREADY.... IF YOU PREFER

### THE DATA FRAME COMES BACK WITH UNCLEAN NAMES CLEAN NAMES IS PART OF THE JANITOR FUNCTION LIBRARY I LOADED EARLIER.
mydf2 <- clean_names(mydf)
head(mydf2, 5)
automydf2 <-clean_names(automydf)
indmydf2 <- clean_names(indmydf)
healthmydf2 <- clean_names(healthmydf)
mediamydf2 <- clean_names(mediamydf)
#### MYDF2 SHOULD BE A DATAFRAME VERSION OF THE ORIGINAL STOCK SCREENER IT HAS LOTS OF EXTRANEOUS COLUMNS OBVIOUSLY AS WE ONLY NEED THE TICKERS....

##### THIS STEP IS JUST TO GET A FEEL FOR THE DATA SET TO GIVE YOU CONFIDENCE WE ARE NOT WILD AND CRAZY.
result <- sqldf("SELECT * FROM mydf2 WHERE company_name = 'Bank OZK'")
head(result,5)
#### IDECIDED FOR MY DATA TO FURTHER RESTRICT TO REALLY HIGH DIVIDEND YIELDS... AS THIS MAKES A 40% RETURN OVER 10 YEARS EVEN IF THE STOCK IS FLAT.SO IF YOU ARE
#### A STOCK BROKER YOU STILL GET PAID... AND NOT PROSECUTED....
result2 <- sqldf("SELECT symbol, dividend_yield FROM mydf2 WHERE dividend_yield > 4 Order by dividend_yield DESC")
autoresult2 <- sqldf("SELECT symbol, dividend_yield FROM automydf2 WHERE dividend_yield > 2 Order by dividend_yield DESC")
indresult2 <- sqldf("SELECT symbol, dividend_yield FROM indmydf2 WHERE dividend_yield > 4 Order by dividend_yield DESC")
healthresult2 <- sqldf("SELECT symbol, dividend_yield FROM healthmydf2 WHERE dividend_yield > 2 Order by dividend_yield DESC")
mediaresult2 <- sqldf("SELECT symbol, dividend_yield FROM mediamydf2 WHERE dividend_yield > 4 Order by dividend_yield DESC")


#### RESULT2 THEN IS MY FINANCIAL SELECTION.....
result2   # RETURNS THE PARED DOWN LIST
tickers <- as.vector(result2$symbol)  #THIS RETURNS JUST THE TICKER FROM THE DATA TO A DATAFRAME.
autotickers <- as.vector(autoresult2$symbol)  #THIS RETURNS JUST THE TICKER FROM THE DATA TO A DATAFRAME.
indtickers <- as.vector(indresult2$symbol)  #THIS RETURNS JUST THE TICKER FROM THE DATA TO A DATAFRAME.
healthtickers <- as.vector(healthresult2$symbol)  #THIS RETURNS JUST THE TICKER FROM THE DATA TO A DATAFRAME.
mediatickers <- as.vector(mediaresult2$symbol)  #THIS RETURNS JUST THE TICKER FROM THE DATA TO A DATAFRAME.

tickers # THIS JUST SHOWS YOU THAT YOU HAVE THE RIGHT SET OF TICKERS BEFORE WE USE IT TO THE FUNCTION CALL.


#### OKAY THIS IS THE MAIN ENGINE.....WE ARE DEFINING A FUNCTION WHICH TAKES TICKERS, A COMPANY DATAFRAME, AND NUMBER OF YEARS
#### IT RETURNS A DATAFRAME WITH EACH TICKER WITH 10 YEARS OF DIVIDENDS AND THE PRICE ON THE 31ST DAY OF DECEMBER OF THE YEAR IN QUESTION, OR YEAR END CLOSE.

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

##### THE PREVIOUS STEP WAS JUST TO GET A FEEL THAT THE PROCESS WAS WORKING... THIS STEP ACTUALLY DOES THE FULL LIST... HAD TO WALK BEFORE RUNNING

company_df <- mydf2 %>% select(company_name, Ticker = symbol) # FIRST GET THE COMPANY NAME AND TICKERS
result3 <- get_stock_data_wide(tickers, company_df)
head(result3, 20) #THIS IS THE RAW RESULT WITHOUT ANY CALCULATE FIELDS.

autocompany_df <- automydf2 %>% select(company_name, Ticker = symbol) # FIRST GET THE COMPANY NAME AND TICKERS
autoresult3 <- get_stock_data_wide(tickers, autocompany_df)
head(autoresult3, 20) #THIS IS THE RAW RESULT WITHOUT ANY CALCULATE FIELDS

healthcompany_df <- healthmydf2 %>% select(company_name, Ticker = symbol) # FIRST GET THE COMPANY NAME AND TICKERS
healthresult3 <- get_stock_data_wide(tickers, healthcompany_df)
head(healthresult3, 20) #THIS IS THE RAW RESULT WITHOUT ANY CALCULATE FIELDS

mediacompany_df <- mediamydf2 %>% select(company_name, Ticker = symbol) # FIRST GET THE COMPANY NAME AND TICKERS
mediaresult3 <- get_stock_data_wide(tickers, mediacompany_df)
head(mediaresult3, 20) #THIS IS THE RAW RESULT WITHOUT ANY CALCULATE FIELDS

indcompany_df <- indmydf2 %>% select(company_name, Ticker = symbol) # FIRST GET THE COMPANY NAME AND TICKERS
indresult3 <- get_stock_data_wide(tickers, indcompany_df)
head(indresult3, 20) #THIS IS THE RAW RESULT WITHOUT ANY CALCULATE FIELDS


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
write_xlsx(result4, "fullviewfinancials.result.xlsx")

# MUTATE THREE TO CALCULATE THE AVG DIVIDEND OVER 10 YEARS, THE STARTING PRICE, THE ENDING PRICE IN YEAR 10, AND EQUITY CHANGES.
autoresult4 <- autoresult3 %>%
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
autoresult4
write_xlsx(autoresult4, "fullviewautos.result.xlsx")

# MUTATE THREE TO CALCULATE THE AVG DIVIDEND OVER 10 YEARS, THE STARTING PRICE, THE ENDING PRICE IN YEAR 10, AND EQUITY CHANGES.
mediaresult4 <- mediaresult3 %>%
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
mediaresult4
write_xlsx(mediaresult4, "fullviewmedia.result.xlsx")

# MUTATE THREE TO CALCULATE THE AVG DIVIDEND OVER 10 YEARS, THE STARTING PRICE, THE ENDING PRICE IN YEAR 10, AND EQUITY CHANGES.
healthresult4 <- healthresult3 %>%
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
healthresult4
write_xlsx(healthresult4, "fullviewhealth.result.xlsx")

# MUTATE THREE TO CALCULATE THE AVG DIVIDEND OVER 10 YEARS, THE STARTING PRICE, THE ENDING PRICE IN YEAR 10, AND EQUITY CHANGES.
indresult4 <- indresult3 %>%
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
indresult4
write_xlsx(indresult4, "fullviewindustrials.result.xlsx")



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
write_xlsx(result5, "financials.result.xlsx")

# FINAL SELECT HAS JUST SOME OF THE FIELDS AS YOU MAY WANT TO RESTRICT TO ANY YEAR A NEGATIVE EQUITY WAS PRESENT
autoresult5 <- autoresult4 %>%
  select(
    Company,
    Ticker,
    avgdividend,
    totaldividends,
    price_start,
    price_end,
    change
  )
autoresult5
print(autoresult5)
# WRITE THE RESULTS BACK TO DISK
write_xlsx(autoresult5, "autos.result.xlsx")

# FINAL SELECT HAS JUST SOME OF THE FIELDS AS YOU MAY WANT TO RESTRICT TO ANY YEAR A NEGATIVE EQUITY WAS PRESENT
healthresult5 <- healthresult4 %>%
  select(
    Company,
    Ticker,
    avgdividend,
    totaldividends,
    price_start,
    price_end,
    change
  )
healthresult5
print(healthresult5)
# WRITE THE RESULTS BACK TO DISK
write_xlsx(healthresult5, "health.result.xlsx")

# FINAL SELECT HAS JUST SOME OF THE FIELDS AS YOU MAY WANT TO RESTRICT TO ANY YEAR A NEGATIVE EQUITY WAS PRESENT
mediaresult5 <- mediaresult4 %>%
  select(
    Company,
    Ticker,
    avgdividend,
    totaldividends,
    price_start,
    price_end,
    change
  )
mediaresult5
print(mediaresult5)
# WRITE THE RESULTS BACK TO DISK
write_xlsx(mediaresult5, "media.result.xlsx")

# FINAL SELECT HAS JUST SOME OF THE FIELDS AS YOU MAY WANT TO RESTRICT TO ANY YEAR A NEGATIVE EQUITY WAS PRESENT
indresult5 <- indresult4 %>%
  select(
    Company,
    Ticker,
    avgdividend,
    totaldividends,
    price_start,
    price_end,
    change
  )
indresult5
print(indresult5)
# WRITE THE RESULTS BACK TO DISK
write_xlsx(indresult5, "industrials.result.xlsx")
