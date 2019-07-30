#!/usr/bin/env python

import pandas as pd
import matplotlib.pyplot as plt
import glob
import matplotlib.gridspec as gridspec


#files = glob.glob('node1/hybrid_logs_*')
#files = glob.glob('node1/hybrid_logs_123264*')

files = glob.glob('hybrid_logs_*')


for f in files:
	df = pd.read_csv(f)
	df.columns = df.columns.map(lambda x: x.lstrip())
	df = df.set_index('Current Time')
	if(len(df) > 0):
		#df[['Inflow', 'Gage Discharge', 'Levelpool Outflow', 'Weighted Outflow']].plot()
		#plt.title(f)
		
		#print (df.iloc[0]['Gage ID'])

		#gage_id = int(df.iloc[0]['Gage ID'])

		
		gage_id = df.iloc[0]['Gage ID'].astype(int)

		print (gage_id)

		gage_id = int(gage_id)

		#plt.title(gage_id)

		#plt.ylabel('cms')


		gage_id_str = str(gage_id)


		#mng = plt.get_current_fig_manager()
		#mng.full_screen_toggle()		
		
		fig = plt.figure()

		gs = gridspec.GridSpec(2, 1)

		ax = fig.add_subplot(gs[0,:])

		ax.title.set_text(gage_id_str)

		#ax.ylabel('CMS')

		df[['Inflow', 'Gage Discharge', 'Levelpool Outflow', 'Weighted Outflow']].plot(ax=ax)

		ax2 = fig.add_subplot(gs[1,:])
		df[['Water Elevation']].plot(ax=ax2)
		#ax.set_ylim([100, 900])


		#ax2.ylabe('M')

		#file_name = 'Gage_' + str(gage_id)

		file_name = 'gage_' + str(gage_id) + '.png'

		plt.savefig(file_name)

		#plt.show()
