import requests
import time
from datetime import datetime, timedelta

def get_btc_close(timestamp):
    url = f"https://min-api.cryptocompare.com/data/v2/histoday?fsym=BTC&tsym=USD&limit=1&toTs={timestamp}"
    response = requests.get(url)
    if response.status_code == 200:
        data = response.json()
        if data["Response"] == "Success" and data["Data"]:
            # Return the close of the last day (should match toTs)
            return data["Data"]["Data"][-1]["close"]
        else:
            return "No data"
    else:
        return f"Error: {response.status_code}"

def last_day_of_month(year, month):
    next_month = datetime(year, month % 12 + 1, 1) if month < 12 else datetime(year + 1, 1, 1)
    return (next_month - timedelta(days=1)).replace(hour=0, minute=0, second=0, microsecond=0)

# Start and end dates

# July 17, 2010, marks the debut of Mt. Gox, Bitcoin’s first major exchange.
start_year, start_month = 2010, 7  # July 2010
end_year, end_month = 2025, 2      # Feb 2025

prices = []
current_date = datetime(start_year, start_month, 1)
end_date = datetime(end_year, end_month, 28)


while current_date <= end_date:
    month_end = last_day_of_month(current_date.year, current_date.month)
    timestamp = int(month_end.timestamp())
    date_str = month_end.strftime("%Y-%m-%d")
    
    price = get_btc_close(timestamp)
    prices.append((date_str, price))
    print(f"{date_str}: ${price}")
    
    current_date = (month_end + timedelta(days=1)).replace(day=1)
    time.sleep(4)  # Rate limit buffer

# Save to CSV
import csv
with open("btc_month_end_closes.csv", "w", newline="") as f:
    writer = csv.writer(f)
    writer.writerow(["date", "close"])
    writer.writerows(prices)
