#!/usr/bin/env python
import pandas as pd
#import matplotlib
#matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec


#df = pd.read_csv("hybrid_logs_120052939.csv", header=0)
df = pd.read_csv("levelpool_logs_3528295.csv", header=0)

print(df.columns)

#df[['Inflow', 'Water Elevation', 'Outflow']].plot()

#df[['Inflow', 'Outflow']].plot()

#plt.savefig('fs_plot_3528295')


fig = plt.figure()


gs = gridspec.GridSpec(2, 1)


#df['delta_h_in'].plot()
ax = fig.add_subplot(gs[0,:])
df[['Inflow', 'Outflow']].plot(ax=ax)
ax2 = fig.add_subplot(gs[1,:])
df[['Water Elevation']].plot(ax=ax2)
ax.set_ylim([100, 900])
#plt.suptitle('LP With Mods')

plt.suptitle('3528295')

#plt.savefig('LP_powell')

plt.savefig('3528295')



#####################################
df = pd.read_csv("levelpool_logs_166758723.csv", header=0)

print(df.columns)

#df[['Inflow', 'Water Elevation', 'Outflow']].plot()

#df[['Inflow', 'Outflow']].plot()

#plt.savefig('fs_plot_3528295')


fig = plt.figure()


gs = gridspec.GridSpec(2, 1)


#df['delta_h_in'].plot()
ax = fig.add_subplot(gs[0,:])
df[['Inflow', 'Outflow']].plot(ax=ax)
ax2 = fig.add_subplot(gs[1,:])
df[['Water Elevation']].plot(ax=ax2)
ax.set_ylim([100, 900])
#plt.suptitle('LP With Mods')

plt.suptitle('166758723')

#plt.savefig('LP_powell')

plt.savefig('166758723')


###########################


df = pd.read_csv("levelpool_logs_16944276.csv", header=0)

print(df.columns)

#df[['Inflow', 'Water Elevation', 'Outflow']].plot()

#df[['Inflow', 'Outflow']].plot()

#plt.savefig('fs_plot_3528295')


fig = plt.figure()


gs = gridspec.GridSpec(2, 1)


#df['delta_h_in'].plot()
ax = fig.add_subplot(gs[0,:])
df[['Inflow', 'Outflow']].plot(ax=ax)
ax2 = fig.add_subplot(gs[1,:])
df[['Water Elevation']].plot(ax=ax2)
ax.set_ylim([100, 900])
#plt.suptitle('LP With Mods')

plt.suptitle('16944276')

#plt.savefig('LP_powell')

plt.savefig('16944276')


