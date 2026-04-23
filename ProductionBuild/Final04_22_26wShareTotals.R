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


# SECTION1 - DOWNLOAD THE FIDELITY EXTRACTS WHICH WERE MANUALLY CONVERTED TO EXCEL97 FORMAT AND PUT THEM IN THE WORKING DIRECTORY
# SECTION1B - FIDELITY EXTRACTS WERE BUILT in XLS BUT IN NEW FORM, AND WERE MANUALLY CONVERTED BY JSS FOR THE STEPS BELOW.

#PART1-> WE USED FIDELITY INVESTMENTS STOCK SCREENER TO PARE DOWN TARGET INVESTMENTS (SEE SCREENSHOTS FOR DETAILS)
#PART2 -> PROCESS AND DOWNLOAD ALL FILES LOCALLY (SOURCE PATH-> DESTINATION PATH)
download.file("https://cyrusjones159.github.io/Cockey-Investments/Financials/financials3.xls", ".\\financials3.xls", mode = "wb")
download.file("https://cyrusjones159.github.io/Cockey-Investments/Automotive/autos3.xls", ".\\autos3.xls", mode = "wb")
download.file("https://cyrusjones159.github.io/Cockey-Investments/Automotive/industrials3.xls", ".\\industrials3.xls", mode = "wb")
download.file("https://cyrusjones159.github.io/Cockey-Investments/Healthcare/health3.xls", ".\\health3.xls", mode = "wb")
download.file("https://cyrusjones159.github.io/Cockey-Investments/Media/media3.xls", ".\\media3.xls", mode = "wb")


#SECTION2A - LOAD THE EXCEL FILES USING THE WICKHAM(OUR TEXTBOOKS AUTHORS PLUGIN) INTO DATAFAMES SO THEY CAN BE MANIPULATED - AND USE THE JANITOR PLUGIN TO CLEAN WEIRD SYMBOLS FROM THE COLUMNS

mydf <- read_excel(".//financials3.xls")
automydf <- read_excel("autos3.xls")
indmydf <- read_excel("industrials3.xls")
healthmydf <- read_excel("health3.xls")
mediamydf <- read_excel("media3.xls")
head(mydf, 5)

### THE DATA FRAME COMES BACK WITH UNCLEAN NAMES CLEAN NAMES IS PART OF THE JANITOR FUNCTION LIBRARY I LOADED EARLIER.
mydf2 <- clean_names(mydf)
head(mydf2, 5)
automydf2 <-clean_names(automydf)
indmydf2 <- clean_names(indmydf)
healthmydf2 <- clean_names(healthmydf)
mediamydf2 <- clean_names(mediamydf)
#### MYDF2 SHOULD BE A DATAFRAME VERSION OF THE ORIGINAL STOCK SCREENER IT HAS LOTS OF EXTRANEOUS COLUMNS OBVIOUSLY AS WE ONLY NEED THE TICKERS....


#SECTION3 - BUILD SOME CONFIDENCE WITH WHAT YOU ARE LOOKING AT AND ALLOW FOR INTERMEDIATE SNAPSHOTS OF THE CURRENT DATA WHICH IS ALREADY LARGE
##### THIS STEP IS JUST TO GET A FEEL FOR THE DATA SET TO GIVE YOU CONFIDENCE WE ARE NOT WILD AND CRAZY.

result <- sqldf("SELECT * FROM mydf2 WHERE company_name = 'Bank OZK'")
head(result,5)


#SECTION4 - OPTIONAL FURTHER RESTRICTION ON DATAFRAMES FOR HIGH DIVIDENDS - TAKE THIS OUT IF YOU ARE HAPPY - THIS SCREENS THE STOCK LISTS TO SOMEWHERE NORTH OF 20 STOCKS BY SECTOR

result2 <- sqldf("SELECT symbol, dividend_yield FROM mydf2 WHERE dividend_yield > 4 Order by dividend_yield DESC")
autoresult2 <- sqldf("SELECT symbol, dividend_yield FROM automydf2 WHERE dividend_yield > 2 Order by dividend_yield DESC")
indresult2 <- sqldf("SELECT symbol, dividend_yield FROM indmydf2 WHERE dividend_yield > 4 Order by dividend_yield DESC")
healthresult2 <- sqldf("SELECT symbol, dividend_yield FROM healthmydf2 WHERE dividend_yield > 2 Order by dividend_yield DESC")
mediaresult2 <- sqldf("SELECT symbol, dividend_yield FROM mediamydf2 WHERE dividend_yield > 4 Order by dividend_yield DESC")

#####CAT OUT TO TERMINAL RESULT THE CURRENT LISTS
result2   # RETURNS THE PARED DOWN LIST
autoresult2 # RETURNS THE PARED DOWN LIST
indresult2 # RETURNS THE PARED DOWN LIST
healthresult2 # RETURNS THE PARED DOWN LIST
mediaresult2 # RETURNS THE PARED DOWN LIST

#SECTION5 -CREATE THE PRIMARY FUNCTION INPUTS

tickers <- as.vector(result2$symbol)  #THIS RETURNS JUST THE TICKER FROM THE DATA TO A DATAFRAME.
autotickers <- as.vector(autoresult2$symbol)  #THIS RETURNS JUST THE TICKER FROM THE DATA TO A DATAFRAME.
indtickers <- as.vector(indresult2$symbol)  #THIS RETURNS JUST THE TICKER FROM THE DATA TO A DATAFRAME.
healthtickers <- as.vector(healthresult2$symbol)  #THIS RETURNS JUST THE TICKER FROM THE DATA TO A DATAFRAME.
mediatickers <- as.vector(mediaresult2$symbol)  #THIS RETURNS JUST THE TICKER FROM THE DATA TO A DATAFRAME.

#####CAT OUT TO TERMINAL RESULT THE CURRENT TICKERS
tickers # THIS JUST SHOWS YOU THAT YOU HAVE THE RIGHT SET OF TICKERS BEFORE WE USE IT TO THE FUNCTION CALL.
autotickers
indtickers
healthtickers
mediatickers

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

    #11) Projected Equity Return5
    fiveyearequityproj = totalreturnover10 * 5,

    #12) Projected Dividend Yield5
    fiveyeardivproj = avgdividend * 5,

    #13) Projected Total Yield5
    totalfiveyearview = fiveyearequityproj + fiveyeardivproj
   
  )
  
result4
write_xlsx(result4, "fullviewfinancials.result.xlsx")

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
    totalfiveyearview = fiveyearequityproj + fiveyeardivproj
  )
autoresult4
write_xlsx(autoresult4, "fullviewautos.result.xlsx")

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
    totalfiveyearview = fiveyearequityproj + fiveyeardivproj
  )
mediaresult4
write_xlsx(mediaresult4, "fullviewmedia.result.xlsx")

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
    totalfiveyearview = fiveyearequityproj + fiveyeardivproj
  )
healthresult4
write_xlsx(healthresult4, "fullviewhealth.result.xlsx")

# MUTATE THREE TO CALCULATE THE AVG DIVIDEND OVER 10 YEARS, THE STARTING PRICE, THE ENDING PRICE IN YEAR 10, AND EQUITY CHANGES.
indresult4 <- indresult3 %>%
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
    totalfiveyearview = fiveyearequityproj + fiveyeardivproj
  )
indresult4
write_xlsx(indresult4, "fullviewindustrials.result.xlsx")

#SECTION 9 - SUMMARIZE RESULTS IN A SINGLE DATAFRAME AND WRITE TO EXCEL

#BUILD SUPERDATAFRAME OF ALL STOCKS INTO ONE... AND WRITE IT TO DISK
superdf <- bind_rows(
  result4,
  autoresult4,
  mediaresult4,
  healthresult4,
  indresult4
)
superdf
write_xlsx(superdf, "submission.stats542.xlsx")


#SECTION 10 - SUMMARIZE RESULTS IN A DATABASE FOR SQLITE

# Connect (creates DB file if it doesn't exist)
con <- dbConnect(SQLite(), "submission.stats542.sqlite")
# Write the combined dataframe
dbWriteTable(con, "allstocks", superdf, overwrite = TRUE)

# Close connection
dbDisconnect(con)


#SECTION 11 - IF YOU SO DESIRE SUMMARIZE RESULTS


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
