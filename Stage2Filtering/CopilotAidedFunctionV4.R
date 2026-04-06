company_df <- mydf2 %>% select(company_name, Ticker = symbol)
result3 <- get_stock_data_wide(tickers, company_df)

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
# THAT WAS SATISFYING
/*
 Company                            Ticker avgdividend totaldividends price_start price_end  change
   <chr>                              <chr>        <dbl>          <dbl>       <dbl>     <dbl>   <dbl>
 1 Absa Group Ltd                     AGRPY        1.26           12.6        25         28.5   3.51 
 2 Allianz SE                         ALIZY        1.14           11.4        16.5       42.4  25.9  
 3 ANZ Group Holdings Limited         ANZGY        0.996           9.96       21.8       25.6   3.78 
 4 ARES CAPITAL CORPORATION           ARCC         1.56           17.2        16.5       18.1   1.60 
 5 Atlantic Union Bankshares Corp     AUB          0.978          10.8        35.7       36.1   0.350
 6 Aviva PLC                          AVVIY        1.03           11.3        15.5       16.4   0.881
 7 Axa, Paris                         AXAHY        1.61           16.1        25.2       46.8  21.6  
 8 Banco Bilbao Vizcaya Argentaria SA BBVA         0.378           3.78        6.77      21.9  15.2  
 9 Banco De Chile                     BCH          1.38           13.8        23.5       36.4  12.9  
10 Franklin Resources Inc             BEN          1.26           13.9        39.6       23.4 -16.2  
# ℹ 36 more rows
# ℹ Use `print(n = ...)` to see more rows*/
