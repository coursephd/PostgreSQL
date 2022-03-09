# https://www.machinelearningplus.com/data-manipulation/101-python-datatable-exercises-pydatatable/
import datatable as dt

from datatable import f

# Load the necessary libraries
# The assumption is that these libraries are already installed

import yfinance as yf
import pandas as pd
import pandas_ta as pd_ta
from pandas_datareader import data as pdr
import numpy as np

pd.set_option("display.max_columns", None)

fno_csv = dt.fread("https://archives.nseindia.com/content/fo/fo_mktlots.csv")
fno_csv[:, "symbol_yh"] = fno_csv[:, dt.f.SYMBOL + '.NS', ]

Symbols = fno_csv["symbol_yh"]
Symbols = Symbols.to_list()

# The above line creates a list with additional bracket, that needs to be removed
# https://stackoverflow.com/questions/42616818/remove-redundant-square-brackets-in-a-list-python

Symbols = Symbols[0]

stock_final = pd.DataFrame()
# iterate over each symbol
for i in Symbols:  
    
    # print the symbol which is being downloaded
    print( str(Symbols.index(i)) + str(' : ') + i, sep=',', end=',', flush=True)  
    
    try:
        # download the stock price 
        stock = []
        stock = yf.download(i, period = "30d", interval = "5m")
        
        

        # append the individual stock prices 
        if len(stock) == 0:
            None
        else:
            stock['Name']=i
            stock_final = stock_final.append(stock,sort=False)
    except Exception:
        None

stock_final.reset_index(inplace=True)

print(stock_final)

