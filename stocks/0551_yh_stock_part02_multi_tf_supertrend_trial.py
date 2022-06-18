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


supertrend_period = 10
supertrend_multiplier = 2

stock_final05 = dt.fread("D:/My-Shares/analysis/0551_5min_data_stin.csv")
stock_final05 = stock_final05.to_pandas()

stock_final05_2 = pd_ta.supertrend(stock_final05["price.high"], stock_final05["price.low"], stock_final05["price.close"], length=supertrend_period, multiplier=supertrend_multiplier)
stock_final05 ['SUPERT_10_2'] = stock_final05_2 ['SUPERT_10_2.0']
stock_final05 ['SUPERTd_10_2'] = stock_final05_2 ['SUPERTd_10_2.0']
stock_final05 ['SUPERTl_10_2'] = stock_final05_2 ['SUPERTl_10_2.0']
stock_final05 ['SUPERTs_10_2'] = stock_final05_2 ['SUPERTs_10_2.0']
stock_final05.to_csv ("D:/My-Shares/analysis/0551_5min_data_stout.csv")

stock_final15 = dt.fread("D:/My-Shares/analysis/0551_15min_data_stin.csv")
stock_final15 = stock_final15.to_pandas()

stock_final15_2 = pd_ta.supertrend(stock_final15["price.high"], stock_final15["price.low"], stock_final15["price.close"], length=supertrend_period, multiplier=supertrend_multiplier)
stock_final15 ['SUPERT_10_2'] = stock_final15_2 ['SUPERT_10_2.0']
stock_final15 ['SUPERTd_10_2'] = stock_final15_2 ['SUPERTd_10_2.0']
stock_final15 ['SUPERTl_10_2'] = stock_final15_2 ['SUPERTl_10_2.0']
stock_final15 ['SUPERTs_10_2'] = stock_final15_2 ['SUPERTs_10_2.0']
stock_final15.to_csv ("D:/My-Shares/analysis/0551_15min_data_stout.csv")

stock_final30 = dt.fread("D:/My-Shares/analysis/0551_30min_data_stin.csv")
stock_final30 = stock_final30.to_pandas()

stock_final30_2 = pd_ta.supertrend(stock_final30["price.high"], stock_final30["price.low"], stock_final30["price.close"], length=supertrend_period, multiplier=supertrend_multiplier)
stock_final30 ['SUPERT_10_2'] = stock_final30_2 ['SUPERT_10_2.0']
stock_final30 ['SUPERTd_10_2'] = stock_final30_2 ['SUPERTd_10_2.0']
stock_final30 ['SUPERTl_10_2'] = stock_final30_2 ['SUPERTl_10_2.0']
stock_final30 ['SUPERTs_10_2'] = stock_final30_2 ['SUPERTs_10_2.0']
stock_final30.to_csv ("D:/My-Shares/analysis/0551_30min_data_stout.csv")


stock_final60 = dt.fread("D:/My-Shares/analysis/0551_60min_data_stin.csv")
stock_final60 = stock_final60.to_pandas()

stock_final60_2 = pd_ta.supertrend(stock_final60["price.high"], stock_final60["price.low"], stock_final60["price.close"], length=supertrend_period, multiplier=supertrend_multiplier)
stock_final60 ['SUPERT_10_2'] = stock_final60_2 ['SUPERT_10_2.0']
stock_final60 ['SUPERTd_10_2'] = stock_final60_2 ['SUPERTd_10_2.0']
stock_final60 ['SUPERTl_10_2'] = stock_final60_2 ['SUPERTl_10_2.0']
stock_final60 ['SUPERTs_10_2'] = stock_final60_2 ['SUPERTs_10_2.0']
stock_final60.to_csv ("D:/My-Shares/analysis/0551_60min_data_stout.csv")

print('It took', time.time()-start, 'seconds.')
