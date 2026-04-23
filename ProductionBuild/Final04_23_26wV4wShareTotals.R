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
install.packages("DBI")
install.packages("RSQLite")
install.packages("ggplot")
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
library(DBI)
library(RSQLite)
library(ggplot2)
library(dplyr)


# SECTION1 - DOWNLOAD THE FIDELITY EXTRACTS WHICH WERE MANUALLY CONVERTED TO EXCEL97 FORMAT AND PUT THEM IN THE WORKING DIRECTORY
# SECTION1B - FIDELITY EXTRACTS WERE BUILT in XLS BUT IN NEW FORM, AND WERE MANUALLY CONVERTED BY JSS FOR THE STEPS BELOW.

#PART1-> WE USED FIDELITY INVESTMENTS STOCK SCREENER TO PARE DOWN TARGET INVESTMENTS (SEE SCREENSHOTS FOR DETAILS)
#PART2 -> PROCESS AND DOWNLOAD ALL FILES LOCALLY (SOURCE PATH-> DESTINATION PATH)
download.file("https://cyrusjones159.github.io/Cockey-Investments/Financials/financials3.xls", ".\\financials3.xls", mode = "wb")
download.file("https://cyrusjones159.github.io/Cockey-Investments/Automotive/autos3.xls", ".\\autos3.xls", mode = "wb")
download.file("https://cyrusjones159.github.io/Cockey-Investments/Automotive/industrials3.xls", ".\\industrials3.xls", mode = "wb")
download.file("https://cyrusjones159.github.io/Cockey-Investments/Healthcare/health3.xls", ".\\health3.xls", mode = "wb")
download.file("https://cyrusjones159.github.io/Cockey-Investments/Media/media3.xls", ".\\media3.xls", mode = "wb")
download.file("https://cyrusjones159.github.io/Cockey-Investments/CASHREITS/reits3.xls", ".\\reits3.xls", mode = "wb")

#SECTION2A - LOAD THE EXCEL FILES USING THE WICKHAM(OUR TEXTBOOKS AUTHORS PLUGIN) INTO DATAFAMES SO THEY CAN BE MANIPULATED - AND USE THE JANITOR PLUGIN TO CLEAN WEIRD SYMBOLS FROM THE COLUMNS

mydf <- read_excel(".//financials3.xls")
automydf <- read_excel(".//autos3.xls")
indmydf <- read_excel(".//industrials3.xls")
healthmydf <- read_excel(".//health3.xls")
mediamydf <- read_excel(".//media3.xls")
reitsmydf <- read_excel("//reits3.xls")
head(mydf, 5)

getwd()
list.files()

### THE DATA FRAME COMES BACK WITH UNCLEAN NAMES CLEAN NAMES IS PART OF THE JANITOR FUNCTION LIBRARY I LOADED EARLIER.
mydf2 <- clean_names(mydf)
head(mydf2, 5)
automydf2 <-clean_names(automydf)
indmydf2 <- clean_names(indmydf)
healthmydf2 <- clean_names(healthmydf)
mediamydf2 <- clean_names(mediamydf)
reitsmydf2 <- clean_names(reitsmydf)
head(reitsmydf2,5)
#### MYDF2 SHOULD BE A DATAFRAME VERSION OF THE ORIGINAL STOCK SCREENER IT HAS LOTS OF EXTRANEOUS COLUMNS OBVIOUSLY AS WE ONLY NEED THE TICKERS....


#SECTION3 - BUILD SOME CONFIDENCE WITH WHAT YOU ARE LOOKING AT AND ALLOW FOR INTERMEDIATE SNAPSHOTS OF THE CURRENT DATA WHICH IS ALREADY LARGE
##### THIS STEP IS JUST TO GET A FEEL FOR THE DATA SET TO GIVE YOU CONFIDENCE WE ARE NOT WILD AND CRAZY.

result <- sqldf("SELECT * FROM mydf2 WHERE company_name = 'Bank OZK'")
head(result,5)


#SECTION4 - OPTIONAL FURTHER RESTRICTION ON DATAFRAMES FOR HIGH DIVIDENDS - TAKE THIS OUT IF YOU ARE HAPPY - THIS SCREENS THE STOCK LISTS TO SOMEWHERE NORTH OF 20 STOCKS BY SECTOR

result2 <- sqldf("SELECT symbol, dividend_yield FROM mydf2 WHERE dividend_yield > 4 Order by dividend_yield DESC")
autoresult2 <- sqldf("SELECT symbol, dividend_yield FROM automydf2 WHERE dividend_yield > 4 Order by dividend_yield DESC")
indresult2 <- sqldf("SELECT symbol, dividend_yield FROM indmydf2 WHERE dividend_yield > 4 Order by dividend_yield DESC")
healthresult2 <- sqldf("SELECT symbol, dividend_yield FROM healthmydf2 WHERE dividend_yield > 4 Order by dividend_yield DESC")
mediaresult2 <- sqldf("SELECT symbol, dividend_yield FROM mediamydf2 WHERE dividend_yield > 4 Order by dividend_yield DESC")
reitsresult2 <- sqldf("SELECT symbol, dividend_yield FROM reitsmydf2 WHERE dividend_yield > 4 Order by dividend_yield DESC")

#####CAT OUT TO TERMINAL RESULT THE CURRENT LISTS
result2   # RETURNS THE PARED DOWN LIST
autoresult2 # RETURNS THE PARED DOWN LIST
indresult2 # RETURNS THE PARED DOWN LIST
healthresult2 # RETURNS THE PARED DOWN LIST
mediaresult2 # RETURNS THE PARED DOWN LIST
reitsresult2

#SECTION5 -CREATE THE PRIMARY FUNCTION INPUTS

tickers <- as.vector(result2$symbol)  #THIS RETURNS JUST THE TICKER FROM THE DATA TO A DATAFRAME.
autotickers <- as.vector(autoresult2$symbol)  #THIS RETURNS JUST THE TICKER FROM THE DATA TO A DATAFRAME.
indtickers <- as.vector(indresult2$symbol)  #THIS RETURNS JUST THE TICKER FROM THE DATA TO A DATAFRAME.
healthtickers <- as.vector(healthresult2$symbol)  #THIS RETURNS JUST THE TICKER FROM THE DATA TO A DATAFRAME.
mediatickers <- as.vector(mediaresult2$symbol)  #THIS RETURNS JUST THE TICKER FROM THE DATA TO A DATAFRAME.
reitstickers <- as.vector(reitsresult2$symbol)

#####CAT OUT TO TERMINAL RESULT THE CURRENT TICKERS
tickers # THIS JUST SHOWS YOU THAT YOU HAVE THE RIGHT SET OF TICKERS BEFORE WE USE IT TO THE FUNCTION CALL.
autotickers
indtickers
healthtickers
mediatickers
reitstickers

#SECTION6 - DEFINE THE FUNCTION WHICH USES THE YAHOO FINANCE API TO RETURN DATA
#PLEASE NOTE YOU ARE RESPONSIBLE FOR THE DATA RETURNED FROM THIS FUNCTION SO YOUR FINAL STOCKS NEED TO BE AUDITED MANUALLY AGAINST YAHOO FINANCE, MARKETWATCH, ETC.

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

#SECTION 7 CALL THE FUNCTION WITH THE RIGHT INPUTS
##### THE PREVIOUS STEP WAS JUST TO GET A FEEL THAT THE PROCESS WAS WORKING... THIS STEP ACTUALLY DOES THE FULL LIST... HAD TO WALK BEFORE RUNNING

company_df <- mydf2 %>% select(company_name, Ticker = symbol) # FIRST GET THE COMPANY NAMES AND TICKERS FOR EACH STOCK SCREENED (FROM 20+ STOCKS WITH MAX AS MANY AS 60 STOCKS FROM LISTS OF HUNDREDS)
result3 <- get_stock_data_wide(tickers, company_df)
head(result3, 20) #THIS IS THE RAW RESULT WITHOUT ANY CALCULATE FIELDS.

autocompany_df <- automydf2 %>% select(company_name, Ticker = symbol) # FIRST GET THE COMPANY NAME AND TICKERS
autoresult3 <- get_stock_data_wide(autotickers, autocompany_df)
head(autoresult3, 20) #THIS IS THE RAW RESULT WITHOUT ANY CALCULATE FIELDS

healthcompany_df <- healthmydf2 %>% select(company_name, Ticker = symbol) # FIRST GET THE COMPANY NAME AND TICKERS
healthresult3 <- get_stock_data_wide(healthtickers, healthcompany_df)
head(healthresult3, 20) #THIS IS THE RAW RESULT WITHOUT ANY CALCULATE FIELDS

mediacompany_df <- mediamydf2 %>% select(company_name, Ticker = symbol) # FIRST GET THE COMPANY NAME AND TICKERS
mediaresult3 <- get_stock_data_wide(mediatickers, mediacompany_df)
head(mediaresult3, 20) #THIS IS THE RAW RESULT WITHOUT ANY CALCULATE FIELDS

indcompany_df <- indmydf2 %>% select(company_name, Ticker = symbol) # FIRST GET THE COMPANY NAME AND TICKERS
indresult3 <- get_stock_data_wide(indtickers, indcompany_df)
head(indresult3, 20) #THIS IS THE RAW RESULT WITHOUT ANY CALCULATE FIELDS

reitscompany_df <- reitsmydf2 %>% select(company_name, Ticker = symbol) # FIRST GET THE COMPANY NAME AND TICKERS
reitsresult3 <- get_stock_data_wide(reitstickers, reitscompany_df)
head(reitsresult3, 20) #THIS IS THE RAW RESULT WITHOUT ANY CALCULATE FIELDS


#THESE TAKE A BIT TO RUN

#SECTION 8 -> MUTATE THE RESULTS TO INCLUDE CALCULATED INVESTMENT YIELDS

# MUTATE THREE TO CALCULATE THE AVG DIVIDEND OVER 10 YEARS, THE STARTING PRICE, THE ENDING PRICE IN YEAR 10, AND EQUITY CHANGES.
result4 <- result3 %>%
     mutate(
    #0) AddSector
    sector = "financials",
       
    # 1) Average dividend across all Div_* columns
    avgdividend = rowMeans(select(., starts_with("Div_")), na.rm = TRUE),
    
    # 2) Total dividends across all 10 years
    totaldividends = rowSums(select(., starts_with("Div_")), na.rm = TRUE),
    
    # 3) Starting price (first Price_* column)
    price_start = select(., starts_with("Price_"))[[1]],
    
    # 4) Ending price (last Price_* column)
    price_end = select(., starts_with("Price_"))[[ncol(select(., starts_with("Price_")))]],
    
    # 5) Total change in equity value
    change = price_end - price_start,
    
    # 6) Total return (change + dividends)
    totalreturn = change + totaldividends,
    
    # 7) Total return averaged over 10 years
    totalreturnover10 = totalreturn / 10,
    
    # 8) Number of shares purchasable with $500 (rounded UP)
    shares500 = ceiling(500 / price_end),
    
    # 9) Total spend = rounded shares × current market price
    totalspend = shares500 * price_end,

    #10) Projected Equity Return5
    fiveyearequityproj = totalreturnover10 * 5,

    #11) Projected Dividend Yield5
    fiveyeardivproj = avgdividend * 5,

    #12) Projected Total Yield5
    totalfiveyearview = fiveyearequityproj + fiveyeardivproj,
    
    #13) Selected By Team - Should be Yes Or No
    selected = "TBD"
   
  )

result4
result4b = sqldf("select * from result4 order by totalreturnover10 desc")
write_xlsx(result4b, "fullviewfinancials.result.xlsx")

# MUTATE THREE TO CALCULATE THE AVG DIVIDEND OVER 10 YEARS, THE STARTING PRICE, THE ENDING PRICE IN YEAR 10, AND EQUITY CHANGES.
autoresult4 <- autoresult3 %>%
  mutate(
    
    #0) AddSector
    sector = "autos",
    
    # 1) Average dividend across all Div_* columns
    avgdividend = rowMeans(select(., starts_with("Div_")), na.rm = TRUE),
    
    # 2) Total dividends across all 10 years
    totaldividends = rowSums(select(., starts_with("Div_")), na.rm = TRUE),
    
    # 3) Starting price (first Price_* column)
    price_start = select(., starts_with("Price_"))[[1]],
    
    # 4) Ending price (last Price_* column)
    price_end = select(., starts_with("Price_"))[[ncol(select(., starts_with("Price_")))]],
    
    # 5) Total change in equity value
    change = price_end - price_start,
    
    # 6) Total return (change + dividends)
    totalreturn = change + totaldividends,
    
    # 7) Total return averaged over 10 years
    totalreturnover10 = totalreturn / 10,
    
    # 8) Number of shares purchasable with $500 (rounded UP)
    shares500 = ceiling(500 / price_end),
    
    # 9) Total spend = rounded shares × current market price
    totalspend = shares500 * price_end,

    #10) Projected Equity Return5
    fiveyearequityproj = totalreturnover10 * 5,

    #11) Projected Dividend Yield5
    fiveyeardivproj = avgdividend * 5,

    #12) Projected Total Yield5
    totalfiveyearview = fiveyearequityproj + fiveyeardivproj,
    
    #13) Selected By Team - Should be Yes Or No
    selected = "TBD"
  )
autoresult4
autoresult4b = sqldf("select * from autoresult4 order by totalreturnover10 desc limit 20")
write_xlsx(autoresult4b, "fullviewautos.result.xlsx")

# MUTATE THREE TO CALCULATE THE AVG DIVIDEND OVER 10 YEARS, THE STARTING PRICE, THE ENDING PRICE IN YEAR 10, AND EQUITY CHANGES.
mediaresult4 <- mediaresult3 %>%
 mutate(
    #0) AddSector
    sector = "media",
   
    # 1) Average dividend across all Div_* columns
    avgdividend = rowMeans(select(., starts_with("Div_")), na.rm = TRUE),
    
    # 2) Total dividends across all 10 years
    totaldividends = rowSums(select(., starts_with("Div_")), na.rm = TRUE),
    
    # 3) Starting price (first Price_* column)
    price_start = select(., starts_with("Price_"))[[1]],
    
    # 4) Ending price (last Price_* column)
    price_end = select(., starts_with("Price_"))[[ncol(select(., starts_with("Price_")))]],
    
    # 5) Total change in equity value
    change = price_end - price_start,
    
    # 6) Total return (change + dividends)
    totalreturn = change + totaldividends,
    
    # 7) Total return averaged over 10 years
    totalreturnover10 = totalreturn / 10,
    
    # 8) Number of shares purchasable with $500 (rounded UP)
    shares500 = ceiling(500 / price_end),
    
    # 9) Total spend = rounded shares × current market price
    totalspend = shares500 * price_end,

    #10) Projected Equity Return5
    fiveyearequityproj = totalreturnover10 * 5,

    #11) Projected Dividend Yield5
    fiveyeardivproj = avgdividend * 5,

    #12) Projected Total Yield5
    totalfiveyearview = fiveyearequityproj + fiveyeardivproj,
    
    #13) Selected By Team - Should be Yes Or No
    selected = "TBD"
  )
mediaresult4
mediaresult4b = sqldf("select * from mediaresult4 order by totalreturnover10 desc limit 20")
write_xlsx(mediaresult4b, "fullviewfinancials.result.xlsx")

# MUTATE THREE TO CALCULATE THE AVG DIVIDEND OVER 10 YEARS, THE STARTING PRICE, THE ENDING PRICE IN YEAR 10, AND EQUITY CHANGES.
healthresult4 <- healthresult3 %>%
  mutate(
    
     #0) AddSector
    sector = "health",
    
    # 1) Average dividend across all Div_* columns
    avgdividend = rowMeans(select(., starts_with("Div_")), na.rm = TRUE),
    
    # 2) Total dividends across all 10 years
    totaldividends = rowSums(select(., starts_with("Div_")), na.rm = TRUE),
    
    # 3) Starting price (first Price_* column)
    price_start = select(., starts_with("Price_"))[[1]],
    
    # 4) Ending price (last Price_* column)
    price_end = select(., starts_with("Price_"))[[ncol(select(., starts_with("Price_")))]],
    
    # 5) Total change in equity value
    change = price_end - price_start,
    
    # 6) Total return (change + dividends)
    totalreturn = change + totaldividends,
    
    # 7) Total return averaged over 10 years
    totalreturnover10 = totalreturn / 10,
    
    # 8) Number of shares purchasable with $500 (rounded UP)
    shares500 = ceiling(500 / price_end),
    
    # 9) Total spend = rounded shares × current market price
    totalspend = shares500 * price_end,

    #10) Projected Equity Return5
    fiveyearequityproj = totalreturnover10 * 5,

    #11) Projected Dividend Yield5
    fiveyeardivproj = avgdividend * 5,

    #12) Projected Total Yield5
    totalfiveyearview = fiveyearequityproj + fiveyeardivproj,
    
    #13) Selected By Team - Should be Yes Or No
    selected = "TBD"
  )
healthresult4
healthresult4b = sqldf("select * from healthresult4 order by totalreturnover10 desc limit 20")
write_xlsx(healthresult4b, "fullviewhealth.result.xlsx")

# MUTATE THREE TO CALCULATE THE AVG DIVIDEND OVER 10 YEARS, THE STARTING PRICE, THE ENDING PRICE IN YEAR 10, AND EQUITY CHANGES.
indresult4 <- indresult3 %>%
  mutate(

    #0) AddSector
    sector = "industrials",
    
    # 1) Average dividend across all Div_* columns
    avgdividend = rowMeans(select(., starts_with("Div_")), na.rm = TRUE),
    
    # 2) Total dividends across all 10 years
    totaldividends = rowSums(select(., starts_with("Div_")), na.rm = TRUE),
    
    # 3) Starting price (first Price_* column)
    price_start = select(., starts_with("Price_"))[[1]],
    
    # 4) Ending price (last Price_* column)
    price_end = select(., starts_with("Price_"))[[ncol(select(., starts_with("Price_")))]],
    
    # 5) Total change in equity value
    change = price_end - price_start,
    
    # 6) Total return (change + dividends)
    totalreturn = change + totaldividends,
    
    # 7) Total return averaged over 10 years
    totalreturnover10 = totalreturn / 10,
    
    # 8) Number of shares purchasable with $500 (rounded UP)
    shares500 = ceiling(500 / price_end),
    
    # 9) Total spend = rounded shares × current market price
    totalspend = shares500 * price_end,

    #10) Projected Equity Return5
    fiveyearequityproj = totalreturnover10 * 5,

    #11) Projected Dividend Yield5
    fiveyeardivproj = avgdividend * 5,

    #12) Projected Total Yield5
    totalfiveyearview = fiveyearequityproj + fiveyeardivproj,
    
    #13) Selected By Team - Should be Yes Or No
    selected = "TBD"
  )
indresult4
indresult4b = sqldf("select * from indresult4 order by totalreturnover10 desc limit 20")
write_xlsx(indresult4b, "fullviewindustrials.result.xlsx")

# MUTATE THREE TO CALCULATE THE AVG DIVIDEND OVER 10 YEARS, THE STARTING PRICE, THE ENDING PRICE IN YEAR 10, AND EQUITY CHANGES.
reitsresult4 <- reitsresult3 %>%
  mutate(
    
    #0) AddSector
    sector = "reits",
    
    # 1) Average dividend across all Div_* columns
    avgdividend = rowMeans(select(., starts_with("Div_")), na.rm = TRUE),
    
    # 2) Total dividends across all 10 years
    totaldividends = rowSums(select(., starts_with("Div_")), na.rm = TRUE),
    
    # 3) Starting price (first Price_* column)
    price_start = select(., starts_with("Price_"))[[1]],
    
    # 4) Ending price (last Price_* column)
    price_end = select(., starts_with("Price_"))[[ncol(select(., starts_with("Price_")))]],
    
    # 5) Total change in equity value
    change = price_end - price_start,
    
    # 6) Total return (change + dividends)
    totalreturn = change + totaldividends,
    
    # 7) Total return averaged over 10 years
    totalreturnover10 = totalreturn / 10,
    
    # 8) Number of shares purchasable with $500 (rounded UP)
    shares500 = ceiling(500 / price_end),
    
    # 9) Total spend = rounded shares × current market price
    totalspend = shares500 * price_end,
    
    #10) Projected Equity Return5
    fiveyearequityproj = totalreturnover10 * 5,
    
    #11) Projected Dividend Yield5
    fiveyeardivproj = avgdividend * 5,
    
    #12) Projected Total Yield5
    totalfiveyearview = fiveyearequityproj + fiveyeardivproj,
    
    #13) Selected By Team - Should be Yes Or No
    selected = "TBD"
  )
reitsresult4
reitsresult4b <- sqldf("select * from reitsresult4 order by totalreturnover10 DESC LIMIT 20")
write_xlsx(reitsresult4b, "fullviewreits.result.xlsx")



#SECTION 9 - SUMMARIZE RESULTS IN A SINGLE DATAFRAME AND WRITE TO EXCEL

#BUILD SUPERDATAFRAME OF ALL STOCKS INTO ONE... AND WRITE IT TO DISK
superdf <- bind_rows(
  result4,
  autoresult4,
  mediaresult4,
  healthresult4,
  indresult4,
  reitsresult4
)
superdf
write_xlsx(superdf, "submission.stats542.xlsx")


#SECTION 10 - SUMMARIZE RESULTS IN A DATABASE FOR SQLITE

# Connect (creates DB file if it doesn't exist)
con <- dbConnect(SQLite(), "submission2.stats542.sqlite")
# Write the combined dataframe
dbWriteTable(con, "allstocks", superdf, overwrite = TRUE)

# Close connection
dbDisconnect(con)


#SECTION 11 - RESULTS BY SECTOR


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

# FINAL SELECT HAS JUST SOME OF THE FIELDS AS YOU MAY WANT TO RESTRICT TO ANY YEAR A NEGATIVE EQUITY WAS PRESENT
reitsresult5 <- reitsresult4 %>%
  select(
    Company,
    Ticker,
    avgdividend,
    totaldividends,
    price_start,
    price_end,
    change
  )
reitsresult4
print(reitsresult5)
# WRITE THE RESULTS BACK TO DISK
write_xlsx(indresult5, "industrials.result.xlsx")

# SECTION 12 - GGPLOT FUNCTION AND RESULTS

library(dplyr)
library(tidyr)
library(ggplot2)

# ============================================================
# 1. Long-format converters (for stock-level charts)
# ============================================================

convert_prices_long <- function(df) {
  df %>%
    pivot_longer(
      cols = matches("^Price_[0-9]{4}$"),
      names_to = "Year",
      values_to = "Price"
    ) %>%
    mutate(Year = as.numeric(gsub("Price_", "", Year)))
}

convert_dividends_long <- function(df) {
  df %>%
    pivot_longer(
      cols = matches("^Div_[0-9]{4}$"),
      names_to = "Year",
      values_to = "Dividend"
    ) %>%
    mutate(Year = as.numeric(gsub("Div_", "", Year)))
}

convert_equity_long <- function(df) {
  price_cols <- grep("^Price_[0-9]{4}$", colnames(df), value = TRUE)
  
  df %>%
    select(Company, Ticker, all_of(price_cols)) %>%
    pivot_longer(
      cols = all_of(price_cols),
      names_to = "Year",
      values_to = "Price"
    ) %>%
    mutate(Year = as.numeric(gsub("Price_", "", Year))) %>%
    arrange(Ticker, Year) %>%
    group_by(Ticker) %>%
    mutate(EquityChange = Price - lag(Price)) %>%
    ungroup()
}

# ============================================================
# 2. Stock-level plots (per stock)
# ============================================================

plot_stock_prices <- function(df, sector_name) {
  ggplot(df, aes(x = Year, y = Price, color = Ticker, group = Ticker)) +
    geom_line(size = 1) +
    labs(
      title = paste("Stock Prices Over Time -", sector_name),
      x = "Year", y = "Price"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
}

plot_stock_dividends <- function(df, sector_name) {
  ggplot(df, aes(x = Year, y = Dividend, color = Ticker, group = Ticker)) +
    geom_line(size = 1) +
    labs(
      title = paste("Dividends Over Time -", sector_name),
      x = "Year", y = "Dividend"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
}

plot_stock_equity <- function(df, sector_name) {
  ggplot(df, aes(x = Year, y = EquityChange, color = Ticker, group = Ticker)) +
    geom_line(size = 1) +
    labs(
      title = paste("Equity Change Per Year -", sector_name),
      x = "Year", y = "Equity Change"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
}

# ============================================================
# 3. Sector-level averages (one row per year)
# ============================================================

sector_avg_dividends <- function(df) {
  convert_dividends_long(df) %>%
    group_by(Year) %>%
    summarise(AvgDividend = mean(Dividend, na.rm = TRUE))
}

sector_avg_equity <- function(df) {
  convert_equity_long(df) %>%
    group_by(Year) %>%
    summarise(AvgEquityChange = mean(EquityChange, na.rm = TRUE))
}

sector_summary <- function(df) {
  left_join(
    sector_avg_dividends(df),
    sector_avg_equity(df),
    by = "Year"
  )
}

# ============================================================
# 4. Combined sector plot (dividends + equity + stock count)
# ============================================================

plot_sector_summary <- function(df, sector_name, original_df) {
  
  stock_count <- length(unique(original_df$Ticker))
  legend_label <- paste0("Stocks in Sector (N = ", stock_count, ")")
  
  start_year <- min(df$Year, na.rm = TRUE)
  end_year   <- max(df$Year, na.rm = TRUE)
  
  df_start <- df %>% filter(Year == start_year)
  df_end   <- df %>% filter(Year == end_year)
  
  ggplot(df, aes(x = Year)) +
    
    geom_line(aes(y = AvgDividend, color = "Avg Dividend"), size = 1.4) +
    geom_smooth(aes(y = AvgDividend, color = "Dividend Trend"),
                method = "lm", se = FALSE, size = 1.2, linetype = "dashed") +
    
    geom_line(aes(y = AvgEquityChange, color = "Avg Equity Change"), size = 1.4) +
    geom_smooth(aes(y = AvgEquityChange, color = "Equity Trend"),
                method = "lm", se = FALSE, size = 1.2, linetype = "dashed") +
    
    geom_line(aes(y = NA, color = legend_label), size = 0) +
    
    geom_point(data = df_start, aes(y = AvgDividend), color = "blue", size = 5) +
    geom_point(data = df_end,   aes(y = AvgDividend), color = "blue", size = 5) +
    geom_point(data = df_start, aes(y = AvgEquityChange), color = "red", size = 5) +
    geom_point(data = df_end,   aes(y = AvgEquityChange), color = "red", size = 5) +
    
    geom_text(data = df_start,
              aes(y = AvgDividend, label = round(AvgDividend, 2)),
              vjust = -1.2, size = 6, color = "blue", fontface = "bold") +
    geom_text(data = df_end,
              aes(y = AvgDividend, label = round(AvgDividend, 2)),
              vjust = -1.2, size = 6, color = "blue", fontface = "bold") +
    
    geom_text(data = df_start,
              aes(y = AvgEquityChange, label = round(AvgEquityChange, 2)),
              vjust = 1.5, size = 6, color = "red", fontface = "bold") +
    geom_text(data = df_end,
              aes(y = AvgEquityChange, label = round(AvgEquityChange, 2)),
              vjust = 1.5, size = 6, color = "red", fontface = "bold") +
    
    labs(
      title = paste("Sector Average Dividend & Equity Change -", sector_name),
      x = "Year", y = "Value", color = "Metric"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 20)
    )
}

# ============================================================
# 5. Build summaries for all sectors
# ============================================================

fin_summary    <- sector_summary(result4)
auto_summary   <- sector_summary(autoresult4)
health_summary <- sector_summary(healthresult4)
media_summary  <- sector_summary(mediaresult4)
ind_summary    <- sector_summary(indresult4)
reits_summary  <- sector_summary(reitsresult4)

# ============================================================
# 6. Build stock-level plots
# ============================================================

p_fin_prices   <- plot_stock_prices(convert_prices_long(result4), "Financials")
p_fin_divs     <- plot_stock_dividends(convert_dividends_long(result4), "Financials")
p_fin_equity   <- plot_stock_equity(convert_equity_long(result4), "Financials")

p_auto_prices   <- plot_stock_prices(convert_prices_long(autoresult4), "Automotive")
p_auto_divs     <- plot_stock_dividends(convert_dividends_long(autoresult4), "Automotive")
p_auto_equity   <- plot_stock_equity(convert_equity_long(autoresult4), "Automotive")

p_health_prices <- plot_stock_prices(convert_prices_long(healthresult4), "Healthcare")
p_health_divs   <- plot_stock_dividends(convert_dividends_long(healthresult4), "Healthcare")
p_health_equity <- plot_stock_equity(convert_equity_long(healthresult4), "Healthcare")

p_media_prices  <- plot_stock_prices(convert_prices_long(mediaresult4), "Media")
p_media_divs    <- plot_stock_dividends(convert_dividends_long(mediaresult4), "Media")
p_media_equity  <- plot_stock_equity(convert_equity_long(mediaresult4), "Media")

p_ind_prices    <- plot_stock_prices(convert_prices_long(indresult4), "Industrials")
p_ind_divs      <- plot_stock_dividends(convert_dividends_long(indresult4), "Industrials")
p_ind_equity    <- plot_stock_equity(convert_equity_long(indresult4), "Industrials")

p_reits_prices  <- plot_stock_prices(convert_prices_long(reitsresult4), "REITs")
p_reits_divs    <- plot_stock_dividends(convert_dividends_long(reitsresult4), "REITs")
p_reits_equity  <- plot_stock_equity(convert_equity_long(reitsresult4), "REITs")

# ============================================================
# 7. Build sector-level plots
# ============================================================

p_fin_summary    <- plot_sector_summary(fin_summary, "Financials", result4)
p_auto_summary   <- plot_sector_summary(auto_summary, "Automotive", autoresult4)
p_health_summary <- plot_sector_summary(health_summary, "Healthcare", healthresult4)
p_media_summary  <- plot_sector_summary(media_summary, "Media", mediaresult4)
p_ind_summary    <- plot_sector_summary(ind_summary, "Industrials", indresult4)
p_reits_summary  <- plot_sector_summary(reits_summary, "REITs", reitsresult4)

# ============================================================
# 8. Write ALL plots into ONE PDF
# ============================================================

pdf("all_sector_plots.pdf", width = 12, height = 8)

# Stock-level plots
print(p_fin_prices);   print(p_fin_divs);   print(p_fin_equity)
print(p_auto_prices);  print(p_auto_divs);  print(p_auto_equity)
print(p_health_prices); print(p_health_divs); print(p_health_equity)
print(p_media_prices); print(p_media_divs); print(p_media_equity)
print(p_ind_prices);   print(p_ind_divs);   print(p_ind_equity)
print(p_reits_prices); print(p_reits_divs); print(p_reits_equity)

# Sector-level summaries
print(p_fin_summary)
print(p_auto_summary)
print(p_health_summary)
print(p_media_summary)
print(p_ind_summary)
print(p_reits_summary)

dev.off()



# ============================================================
# FACET GRID OF FITTED LINES (6 SMALL BOXES)
# ============================================================

# 1. Combine all sector summaries into one long-format dataset
all_sectors_long <- bind_rows(
  fin_summary    %>% mutate(Sector = "Financials"),
  auto_summary   %>% mutate(Sector = "Automotive"),
  health_summary %>% mutate(Sector = "Healthcare"),
  media_summary  %>% mutate(Sector = "Media"),
  ind_summary    %>% mutate(Sector = "Industrials"),
  reits_summary  %>% mutate(Sector = "REITs")
) %>%
  pivot_longer(
    cols = c(AvgDividend, AvgEquityChange),
    names_to = "Metric",
    values_to = "Value"
  )

# 2. Build the 6‑box facet plot (2×3 layout)
p_facet_small <- ggplot(all_sectors_long, aes(x = Year, y = Value, color = Metric)) +
  geom_smooth(method = "lm", se = FALSE, size = 1.4) +
  facet_wrap(~ Sector, ncol = 3, scales = "free_y") +
  labs(
    title = "Fitted Trend Lines for Dividends and Stock Yields by Sector",
    x = "Year",
    y = "Fitted Value",
    color = "Metric"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 20),
    strip.text = element_text(size = 14, face = "bold")
  )

# 3. Export to a separate PDF
pdf("sector_fitted_trends_small_boxes.pdf", width = 14, height = 10)
print(p_facet_small)
dev.off()