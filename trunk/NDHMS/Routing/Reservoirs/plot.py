#!/usr/bin/env python
import pandas as pd
#import matplotlib
#matplotlib.use('TkAgg')
import matplotlib.pyplot as plt

df = pd.read_csv("hybrid_logs_120052939.csv", header=0)
#df = pd.read_csv("hybrid_logs_1312051.csv", header=0)

print(df.columns)

df[[' Inflow', ' Persisted Outflow', ' Levelpool Outflow', ' Weighted Outflow']].plot()

plt.savefig('fs_plot')
