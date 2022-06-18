# Python code:

import time
start = time.time()

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
from math import floor, ceil


supertrend_period = 20
supertrend_multiplier = 2.7 

stock_final = dt.fread("D:/My-Shares/analysis/0550_stock_final.csv")
stock_final = stock_final.to_pandas()

stock_final02 = pd_ta.supertrend(stock_final["price.high"], stock_final["price.low"], stock_final["price.close"], length=supertrend_period, multiplier=supertrend_multiplier)

# Place the DataFrames side by side
stock_final03 = pd.concat([stock_final, stock_final02], axis=1)
stock_final03.to_csv ("D:/My-Shares/analysis/0550_stock_final03.csv")

print('It took', time.time()-start, 'seconds.')
