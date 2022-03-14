# Python code:

import datatable as dt
from datatable import f

import csv
import os
import yfinance as yf
import pandas as pd
import time
start = time.time()


fno_csv = dt.fread("https://archives.nseindia.com/content/fo/fo_mktlots.csv")
fno_csv[:, "symbol_yh"] = fno_csv[:, dt.f.SYMBOL + '.NS', ]

Symbols = fno_csv["symbol_yh"]
Symbols = Symbols.to_list()

# The above line creates a list with additional bracket, that needs to be removed
# https://stackoverflow.com/questions/42616818/remove-redundant-square-brackets-in-a-list-python

Symbols = Symbols[0]

Symbols.remove('MIDCPNIFTY.NS')
Symbols.remove('NIFTY.NS')
Symbols.remove('BANKNIFTY.NS')
Symbols.remove('FINNIFTY.NS')
Symbols.remove('Symbol.NS')

data = yf.download(
        tickers = Symbols,
        period = "30d", interval = "5m",
        group_by = 'ticker',
        auto_adjust = False,
        prepost = False,
        threads = True,
        proxy = None
    )

#data = pd.dataFrame(data.T)
data.to_csv ("D:/My-Shares/analysis/0550_data.csv")

print('It took', time.time()-start, 'seconds.')