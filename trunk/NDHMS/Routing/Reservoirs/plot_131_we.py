#!/usr/bin/env python
import pandas as pd
#import matplotlib
#matplotlib.use('TkAgg')
import matplotlib.pyplot as plt

#df = pd.read_csv("hybrid_logs_120052939.csv", header=0)
df = pd.read_csv("levelpool_logs_3528295.csv", header=0)

print(df.columns)

#df[['Inflow', 'Water Elevation', 'Outflow']].plot()
#df[['Inflow', 'Outflow']].plot()
df[['Water Elevation']].plot()


plt.savefig('fs_plot_3528295_we')
