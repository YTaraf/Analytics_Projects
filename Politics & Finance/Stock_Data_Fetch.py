from datetime import datetime, timezone
import pandas as pd
import requests

class StockDataFetcher:
    def __init__(self, api_key):
        self.api_key = api_key
        self.base_url = "https://api.polygon.io/v2/aggs/ticker"

    def fetch_data(self, ticker, timespan="day", multiplier=1, start_date="2024-11-05", end_date=None):
        if end_date is None:
        # Set end_date to today's date in 'YYYY-MM-DD' format
            end_date = datetime.today().strftime('%Y-%m-%d')
        url = f"{self.base_url}/{ticker}/range/{multiplier}/{timespan}/{start_date}/{end_date}?&limit=100&apiKey={self.api_key}"

        try:
            response = requests.get(url)
            response.raise_for_status()  #Error for bad responses
            return response.json()
        except requests.exceptions.RequestException as e:
            print(f"Error fetching data for {ticker}: {e}")
            return None

    def process_stock_data(self, stock_data):
        if stock_data and 'results' in stock_data:
            processed_data = []
            for record in stock_data['results']:
                # Convert timestamp to UTC datetime
                date = datetime.fromtimestamp(record['t'] / 1000, timezone.utc).strftime('%Y-%m-%d')
                processed_data.append({
                    "date": date,
                    "open": record['o'],
                    "close": record['c'],
                    "high": record['h'],
                    "low": record['l'],
                    "volume": record['v']
                })
            return processed_data
        return []

    def get_stock_data_for_tickers(self, tickers, timespan="day", multiplier=1, start_date="2024-11-05", end_date=None):
        if end_date is None:
        # Set end_date to today's date in 'YYYY-MM-DD' format
            end_date = datetime.today().strftime('%Y-%m-%d')
        all_data = {}
        for ticker in tickers:
            print(f"Fetching data for {ticker}...")
            stock_data = self.fetch_data(ticker, timespan, multiplier, start_date, end_date)
            processed_data = self.process_stock_data(stock_data)
            if processed_data:
                all_data[ticker] = processed_data
        return all_data
    def create_dataframe(self, stock_data):
    #Empty list to collect the data
        rows = []

    # Loop through the stock data
        for ticker, data in stock_data.items():
            stock_row = {"ticker": ticker}  # Start with the ticker
            for record in data:
             # Use the date as the key and the close price as the value
                stock_row[record['date']] = record['close']
            rows.append(stock_row)

    # Convert rows into a DataFrame
        df = pd.DataFrame(rows)

    # Set the ticker as the index and ensure columns are sorted by date
        df.set_index("ticker", inplace=True)
        df.columns = pd.to_datetime(df.columns)  # Convert date columns to datetime for sorting
        df = df.sort_index(axis=1)  # Sort columns (dates) from left to right

        return df
