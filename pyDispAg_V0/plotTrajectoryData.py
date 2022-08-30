'''
Working to develop code that mimics the Agtraj.for and Agtrgo.for subroutines

AGTRAJ initializes the trajectory details toolbox calculation

Call to agtraj(ud, drop, ntr, sv, av, jav

UD     - USERDATA data structure - should not need as is contained in the various classes set
DROP   - Initial drop diameter (micrometers)
NTR    - Flag to inverse coordinate transform (0=no; 1=yes) Looking from front or back of plane
SV     - Scale limits (x,y,z) for this diameter
AV     - Initial position array (x,y,z) for all nozzles
JAV    - Position counter set to 1 = active: Initially comes in as all ones.

'''

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.image as image
from matplotlib.offsetbox import (OffsetImage, AnnotationBbox)

'''
The following are parameters to change boom height based on extent to wing tip, 
drop number for diameter, and wind speed.
'''

im = image.imread('mostplane.png')

read_data = False

if read_data == True:

    boomheights = [12, 15, 20] # feet, convert to m below
    extents = [-1.0, -0.75, -0.5, -0.25, 0.0, 0.25, 0.5, 0.75, 1.0]
    wind_speeds = [2, 5, 10] # mph, convert to m below
    diamvs = [50, 100, 250, 500, 750]

    trdf = pd.DataFrame()
    ht12 = []
    ht15 = []
    ht20 = []

    for boomheight in boomheights:
        for extent in extents:
            for wind_speed in wind_speeds:
                for diamv in diamvs:

                    name = str(int(diamv)) + '_' + str(round(extent,2))\
                           + '_' + str(int(wind_speed))+ '_' + str(int(boomheight))

                    filename = name + '.npy'
                    data = np.load(filename)

                    newdata = pd.DataFrame([{'Name': name, 'Boom Ht (ft)': boomheight, 'Extent (%)': extent,
                                             'WS (mph)': wind_speed, 'Diameter (um)':diamv,
                                             'X Data': data[0], 'Y Data': data[1],
                                             'Z Data': data[2], 'T Data': data[3]}])
                    trdf = pd.concat([trdf, newdata], ignore_index=True)

    trdf.to_pickle('AgDisp Trajectories.pkl')

else:
    trdf = pd.read_pickle('AgDisp Trajectories.pkl')

def getExtent(df, boomht, ws, diam, extent):
    ext = df.loc[(df['Boom Ht (ft)'] == boomht) & (df['WS (mph)'] == ws) & (df['Diameter (um)'] == diam)
                  & (df['Extent (%)'] == extent)].reset_index(drop=True)
    return ext


def plotByExtent(df, boomht, ws, diam, im, figsave=False):

    filename = 'Boom Ht ' + str(int(boomht)) + ' ft WS ' + str(int(ws)) + 'mph Diam ' + str(int(diam)) + ' um.png'

    ext0 = getExtent(df, boomht, ws, diam, 0.0)
    ext25 = getExtent(df, boomht, ws, diam, 0.25)
    ext50 = getExtent(df, boomht, ws, diam, 0.5)
    ext75 = getExtent(df, boomht, ws, diam, 0.75)
    ext100 = getExtent(df, boomht, ws, diam, 1)

    fig = plt.figure(figsize=(15,10), constrained_layout=True)
    
    ax = fig.add_subplot()

    size = 75

    label0 = '0%: Traveled '+str(int(ext0['Y Data'][0][-1])) + ' ft in ' + str(int(ext0['Y Data'][0][-1])) + ' sec'
    label25 = '25%: Traveled ' + str(int(ext25['Y Data'][0][-1])) + ' ft in ' + str(int(ext25['Y Data'][0][-1])) + ' sec'
    label50 = '50%: Traveled ' + str(int(ext50['Y Data'][0][-1])) + ' ft in ' + str(int(ext50['Y Data'][0][-1])) + ' sec'
    label75 = '75%: Traveled ' + str(int(ext75['Y Data'][0][-1])) + ' ft in ' + str(int(ext75['Y Data'][0][-1])) + ' sec'
    label100 = '100%: Traveled ' + str(int(ext100['Y Data'][0][-1])) + ' ft in ' + str(int(ext100['Y Data'][0][-1])) + ' sec'

    ax.scatter(ext0['Y Data'][0], ext0['Z Data'][0], marker='o', s = size, color='royalblue', label=label0)
    ax.scatter(ext25['Y Data'][0], ext25['Z Data'][0], marker='v', s=size, color='black', label=label25)
    ax.scatter(ext50['Y Data'][0], ext50['Z Data'][0], marker='s', s=size, color='maroon', label=label50)
    ax.scatter(ext75['Y Data'][0], ext75['Z Data'][0], marker='p', s=size, color='darkgreen', label=label75)
    ax.scatter(ext100['Y Data'][0], ext100['Z Data'][0], marker='^', s=size, color='orange', label=label100)
    ax.set_ylabel('Height (ft)', size=20)
    ax.set_xlabel('Distance (ft)', size=20)

    if boomht == 12:
        imagebox = OffsetImage(im, zoom = .315)
        ab = AnnotationBbox(imagebox, (12, 13.35), frameon = False)
        ax.add_artist(ab)

    t = ax.text(8, 25, "Wind " +str(int(ws)) + ' mph', ha="center", va="center", rotation=0, size=35,
        bbox=dict(boxstyle="rarrow,pad=0.3", fc="lightgrey", ec="black", lw=2))
    bb = t.get_bbox_patch()
    bb.set_boxstyle("rarrow", pad=0.6)

    ax.text(-2.5, 28, str(int(diam)) + ' ' + r'$\mu m$ drops', fontsize = 35)

    ax.legend(fontsize=20)


    ax.set_ylim(0,30)
    ax.set_xlim(-5,80)
    start, end = ax.get_xlim()
    stepsize = 5
    ax.xaxis.set_ticks(np.arange(start, end, stepsize))

    ax.yaxis.set_tick_params(labelsize=20)
    ax.xaxis.set_tick_params(labelsize=20)
    
    plt.show()

    if figsave == True:
        fig.savefig(filename, dpi=300)


def plotByDiameter(df, boomht, ws, extent, im, figsave=False):

    filename = 'Boom Ht ' + str(int(boomht)) + ' ft WS ' + str(int(ws)) + 'mph Boom % ' + str(round(extent, 2)) + ' um.png'

    d50 = getExtent(df, boomht, ws, 50, extent)
    d100 = getExtent(df, boomht, ws, 100, extent)
    d250 = getExtent(df, boomht, ws, 250, extent)
    d500 = getExtent(df, boomht, ws, 500, extent)
    d750 = getExtent(df, boomht, ws, 750, extent)

    fig = plt.figure(figsize=(15, 10), constrained_layout=True)

    ax = fig.add_subplot()

    label50 = r' 50 $\mu m:$' + ' Traveled '+str(int(d50['Y Data'][0][-1])) + ' ft in ' + str(int(d50['Y Data'][0][-1])) + ' sec'
    label100 = r' 100 $\mu m:$' + ' Traveled ' + str(int(d100['Y Data'][0][-1])) + ' ft in ' + str(
        int(d100['Y Data'][0][-1])) + ' sec'
    label250 = r' 250 $\mu m:$' + ' Traveled ' + str(int(d250['Y Data'][0][-1])) + ' ft in ' + str(
        int(d250['Y Data'][0][-1])) + ' sec'
    label500 = r' 500 $\mu m:$' + ' Traveled ' + str(int(d500['Y Data'][0][-1])) + ' ft in ' + str(
        int(d500['Y Data'][0][-1])) + ' sec'
    label750 = r' 750 $\mu m:$' + ' Traveled ' + str(int(d750['Y Data'][0][-1])) + ' ft in ' + str(
        int(d750['Y Data'][0][-1])) + ' sec'

    ax.scatter(d50['Y Data'][0], d50['Z Data'][0], marker='o', s=20, color='royalblue', label=label50)
    ax.scatter(d100['Y Data'][0], d100['Z Data'][0], marker='v', s=40, color='black', label= label100)
    ax.scatter(d250['Y Data'][0], d250['Z Data'][0], marker='s', s=80, color='maroon', label=label250)
    ax.scatter(d500['Y Data'][0], d500['Z Data'][0], marker='p', s=120, color='darkgreen', label=label500)
    ax.scatter(d750['Y Data'][0], d750['Z Data'][0], marker='^', s=200, color='orange', label=label750)
    ax.set_ylabel('Height (ft)', size=20)
    ax.set_xlabel('Distance (ft)', size=20)

    if boomht == 12:
        imagebox = OffsetImage(im, zoom = .315)
        ab = AnnotationBbox(imagebox, (12, 13.35), frameon = False)
        ax.add_artist(ab)

    t = ax.text(8, 25, "Wind " +str(int(ws)) + ' mph', ha="center", va="center", rotation=0, size=35,
        bbox=dict(boxstyle="rarrow,pad=0.3", fc="lightgrey", ec="black", lw=2))
    bb = t.get_bbox_patch()
    bb.set_boxstyle("rarrow", pad=0.6)

    ax.text(-2.5, 28, str(int(extent)) + ' % Boom', fontsize = 35)

    ax.legend(fontsize=20)

    ax.set_ylim(0, 30)
    ax.set_xlim(-5, 80)
    start, end = ax.get_xlim()
    stepsize = 5
    ax.xaxis.set_ticks(np.arange(start, end, stepsize))

    ax.yaxis.set_tick_params(labelsize=20)
    ax.xaxis.set_tick_params(labelsize=20)

    plt.show()

    if figsave == True:
        fig.savefig(filename, dpi=300)

for ws in [2, 5, 10]:
    for d in [50, 100, 250, 500, 750]:
        plotByExtent(trdf, 12, ws, d, im, figsave=True)

for extent in [0.0, 0.25, 0.5, 0.75, 1.0]:
    for ws in [2, 5, 10]:
        plotByDiameter(trdf, 12, ws, extent, im, figsave=True)




