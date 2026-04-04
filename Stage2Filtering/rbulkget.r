import yfinance as yf
import pandas as pd

def get_stock_data(tickers, period="10y"):
    all_prices = {}
    all_dividends = {}

    for t in tickers:
        ticker = yf.Ticker(t)

        # Price history
        prices = ticker.history(period=period)
        prices["Ticker"] = t
        all_prices[t] = prices

        # Dividend history
        dividends = ticker.dividends
        dividends = dividends.to_frame(name="Dividend")
        dividends["Ticker"] = t
        all_dividends[t] = dividends

    # Combine into single DataFrames
    prices_df = pd.concat(all_prices.values(), axis=0)
    dividends_df = pd.concat(all_dividends.values(), axis=0)

    return prices_df, dividends_df


# Example usage
tickers = ["BAC", "WFC", "JPM"]
prices, dividends = get_stock_data(tickers)

print(prices.head())
print(dividends.head())
