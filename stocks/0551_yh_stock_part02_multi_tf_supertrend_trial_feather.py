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

#import feather
#import pyarrow.feather as feather

supertrend_period = 10
supertrend_multiplier = 2

#stock_final05 = feather.read_dataframe("D:/My-Shares/analysis/0551_5min_data_stin.feather")
stock_final05 = pd.read_pickle("D:/My-Shares/analysis/0551_5min_data_stin.pkl")

stock_final05_2 = pd_ta.supertrend(stock_final05["price.high"], stock_final05["price.low"], stock_final05["price.close"], length=supertrend_period, multiplier=supertrend_multiplier)
stock_final05 ['SUPERT05_10_2'] = stock_final05_2 ['SUPERT_10_2.0']
stock_final05 ['SUPERTd05_10_2'] = stock_final05_2 ['SUPERTd_10_2.0']
stock_final05 ['SUPERTl05_10_2'] = stock_final05_2 ['SUPERTl_10_2.0']
stock_final05 ['SUPERTs05_10_2'] = stock_final05_2 ['SUPERTs_10_2.0']

#feather.write_dataframe(stock_final05, "D:/My-Shares/analysis/0551_5min_data_stout.feather")

stock_final15 = pd.read_pickle("D:/My-Shares/analysis/0551_15min_data_stin.pkl")

stock_final15_2 = pd_ta.supertrend(stock_final15["price.high"], stock_final15["price.low"], stock_final15["price.close"], length=supertrend_period, multiplier=supertrend_multiplier)
stock_final15 ['SUPERT15_10_2'] = stock_final15_2 ['SUPERT_10_2.0']
stock_final15 ['SUPERTd15_10_2'] = stock_final15_2 ['SUPERTd_10_2.0']
stock_final15 ['SUPERTl15_10_2'] = stock_final15_2 ['SUPERTl_10_2.0']
stock_final15 ['SUPERTs15_10_2'] = stock_final15_2 ['SUPERTs_10_2.0']

#feather.write_dataframe(stock_final15, "D:/My-Shares/analysis/0551_15min_data_stout.feather")

stock_final30 = pd.read_pickle("D:/My-Shares/analysis/0551_30min_data_stin.pkl")

stock_final30_2 = pd_ta.supertrend(stock_final30["price.high"], stock_final30["price.low"], stock_final30["price.close"], length=supertrend_period, multiplier=supertrend_multiplier)
stock_final30 ['SUPERT30_10_2'] = stock_final30_2 ['SUPERT_10_2.0']
stock_final30 ['SUPERTd30_10_2'] = stock_final30_2 ['SUPERTd_10_2.0']
stock_final30 ['SUPERTl30_10_2'] = stock_final30_2 ['SUPERTl_10_2.0']
stock_final30 ['SUPERTs30_10_2'] = stock_final30_2 ['SUPERTs_10_2.0']

#feather.write_dataframe(stock_final30, "D:/My-Shares/analysis/0551_30min_data_stout.feather")

stock_final60 = pd.read_pickle("D:/My-Shares/analysis/0551_60min_data_stin.pkl")

stock_final60_2 = pd_ta.supertrend(stock_final60["price.high"], stock_final60["price.low"], stock_final60["price.close"], length=supertrend_period, multiplier=supertrend_multiplier)
stock_final60 ['SUPERT60_10_2'] = stock_final60_2 ['SUPERT_10_2.0']
stock_final60 ['SUPERTd60_10_2'] = stock_final60_2 ['SUPERTd_10_2.0']
stock_final60 ['SUPERTl60_10_2'] = stock_final60_2 ['SUPERTl_10_2.0']
stock_final60 ['SUPERTs60_10_2'] = stock_final60_2 ['SUPERTs_10_2.0']

#feather.write_dataframe(stock_final60, "D:/My-Shares/analysis/0551_60min_data_stout.feather")

print('It took', time.time()-start, 'seconds.')
