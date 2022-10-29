# Python code:

import time
start = time.time()

# https://www.machinelearningplus.com/data-manipulation/101-python-datatable-exercises-pydatatable/
#import datatable as dt
#from datatable import f

# Load the necessary libraries
# The assumption is that these libraries are already installed

import yfinance as yf
import pandas as pd
import pandas_ta as pd_ta
from pandas_datareader import data as pdr
import numpy as np
from math import floor, ceil

import feather

supertrend_period = 7
supertrend_multiplier = 3

stock_final05 = feather.read_dataframe("D:/My-Shares/analysis/0600_1d_data_stin.feather")

stock_final05_2 = pd_ta.supertrend(stock_final05["price.high"], stock_final05["price.low"], stock_final05["price.close"], length=supertrend_period, multiplier=supertrend_multiplier)
stock_final05 ['SUPERT05_7_3'] = stock_final05_2 ['SUPERT_7_3.0']
stock_final05 ['SUPERTd05_7_3'] = stock_final05_2 ['SUPERTd_7_3.0']
#stock_final05 ['SUPERTl05_10_2'] = stock_final05_2 ['SUPERTl_10_2.0']
#stock_final05 ['SUPERTs05_10_2'] = stock_final05_2 ['SUPERTs_10_2.0']

feather.write_dataframe(stock_final05, "D:/My-Shares/analysis/0600_1d_data_stout.feather")

print('It took', time.time()-start, 'seconds.')