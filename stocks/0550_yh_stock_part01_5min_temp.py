import yfinance as yf
import pandas as pd
import time

#import feather

start = time.time()

#fno_csv = pd.read_csv("https://archives.nseindia.com/content/fo/fo_mktlots.csv")
fno_csv = pd.read_csv("https://www1.nseindia.com/content/fo/fo_mktlots.csv")

fno_csv ["symbol_yh"] = fno_csv["SYMBOL    "].str.strip() + ".NS"

Symbols = fno_csv["symbol_yh"]
Symbols = Symbols.to_list()

# The above line creates a list with additional bracket, that needs to be removed
# https://stackoverflow.com/questions/42616818/remove-redundant-square-brackets-in-a-list-python

Symbols.remove('MIDCPNIFTY.NS')
Symbols.remove('NIFTY.NS')
Symbols.remove('BANKNIFTY.NS')
Symbols.remove('FINNIFTY.NS')
Symbols.remove('Symbol.NS')

# Add Nifty data for the RRG calculations
#Symbols.append('^NSEI')

Symbols

data = yf.download(
        tickers = Symbols,
        period = "60d", interval = "5m",
        group_by = 'ticker',
        auto_adjust = False,
        prepost = False,
        threads = True,
        proxy = None
    )

data ['Datetime'] = data.index
#feather.write_dataframe(stock_final05, "D:/My-Shares/analysis/0550_data.feather")

#data.to_csv("D:/My-Shares/analysis/0550_data.csv")
#data = pd.DataFrame(data)
#data.to_feather("D:/My-Shares/analysis/0550_data.feather")

#data.to_pickle("D:/My-Shares/analysis/0550_data.pkl")

print('It took', time.time()-start, 'seconds.')
