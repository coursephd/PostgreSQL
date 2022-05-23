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

stock_final = dt.fread("D:/My-Shares/analysis/0550_stock_final_mtf.csv")
stock_final = stock_final.to_pandas()

stock_final02 = pd_ta.supertrend(stock_final["price.high"], stock_final["price.low"], stock_final["price.close"], length=supertrend_period, multiplier=supertrend_multiplier)
stock_final15 = pd_ta.supertrend(stock_final["h15"], stock_final["l15"], stock_final["c15"], length=supertrend_period, multiplier=supertrend_multiplier)
stock_final30 = pd_ta.supertrend(stock_final["h30"], stock_final["l30"], stock_final["c30"], length=supertrend_period, multiplier=supertrend_multiplier)
stock_final60 = pd_ta.supertrend(stock_final["h60"], stock_final["l60"], stock_final["c60"], length=supertrend_period, multiplier=supertrend_multiplier)

stock_final ['SUPERT_10_2'] = stock_final02 ['SUPERT_10_2.0']
stock_final ['SUPERTd_10_2'] = stock_final02 ['SUPERTd_10_2.0']
stock_final ['SUPERTl_10_2'] = stock_final02 ['SUPERTl_10_2.0']
stock_final ['SUPERTs_10_2'] = stock_final02 ['SUPERTs_10_2.0']

stock_final ['SUPERT15_10_2'] = stock_final15 ['SUPERT_10_2.0']
stock_final ['SUPERTd15_10_2'] = stock_final15 ['SUPERTd_10_2.0']
stock_final ['SUPERTl15_10_2'] = stock_final15 ['SUPERTl_10_2.0']
stock_final ['SUPERTs15_10_2'] = stock_final15 ['SUPERTs_10_2.0']

stock_final ['SUPERT30_10_2'] = stock_final30 ['SUPERT_10_2.0']
stock_final ['SUPERTd30_10_2'] = stock_final30 ['SUPERTd_10_2.0']
stock_final ['SUPERTl30_10_2'] = stock_final30 ['SUPERTl_10_2.0']
stock_final ['SUPERTs30_10_2'] = stock_final30 ['SUPERTs_10_2.0']

stock_final ['SUPERT60_10_2'] = stock_final60 ['SUPERT_10_2.0']
stock_final ['SUPERTd60_10_2'] = stock_final60 ['SUPERTd_10_2.0']
stock_final ['SUPERTl60_10_2'] = stock_final60 ['SUPERTl_10_2.0']
stock_final ['SUPERTs60_10_2'] = stock_final60 ['SUPERTs_10_2.0']

stock_final.to_csv ("D:/My-Shares/analysis/0550_stock_final03.csv")

print('It took', time.time()-start, 'seconds.')