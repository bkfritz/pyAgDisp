
import math
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from controlDataClass import controlData
from aircraftDataClass import aircraftData
from terrrainDataClass import terrainData
from canopyDataClass import canopyData
from metDataClass import metData
from dropletDataClass import dropletData
from nozzleDataClass import nozzleData
from sprayMaterialDataClass import sprayMaterialData
from verticalFluxClass import verticalFluxData
from totalAccountArraysClass import totalAccountArrays
from runDataClass import runData
from agSaveClass import saveData
from agCon import agcon
from agBkg import agBkg
from solveEOM import solveEOM
from canopyDepAndTotHtAccount import canDepTotHtAccount
from totalAccountDistance import totalAccountDistance
from totalAccountTime import totalAccountTime
from updateParamsGetDSDs import updateParamsGetDSDs
from determineNewVorticePosits import newVorts
from correctCirculationDecay import correctCirculationDecay
from correctParameters import correctParameters
from checkSolnAndContinue import checkSolnContinue

# maa is sent into Aginit
maa = 0 # -1, 0 = no and a #=wind speed index: Multiple Application Assessment I think
# Constants
tpi = 6.2831853 # 2*pi

boomheight = 12 * 0.3048 # feet, convert to m below
extent = .50
wind_speed = 2 * 0.44704 # m/s
swath_width = 60 * 0.3048

control = controlData() # Instantiate control object
acraft = aircraftData() # Instantiate aircraft object
terrain = terrainData() # Instantiate terrain object
canopy = canopyData() # Instantiate canopy object
met = metData() # Instantiate met object
drops = dropletData() # Instantiate drops object
nozz = nozzleData() # Instantiate nozzle object
spray = sprayMaterialData() # Instantiate spray material object
vertflx = verticalFluxData()

# Drop data is one of the first things established
drops.constructProfile()

# The following will end up being basically Aginit.for
control.changeBoomHeight(boomheight)
control.changeNumSwaths(1)
control.getLineReps()


# Not sure what the following 3 parameters do.
iidep = -1
iidis = -1
iigau = -1

# Aginit 64-75
if maa < 0:
        lfmaa = 1 # Flags for multiple application, met, accountability, i think
        lfmet = 0
        lfmac = 0
elif maa == 0:
        lfmaa = 1
        lfmet = 1
        lfmac = 0
else:
        lfmaa = 0
        lfmet = 1
        lfmac = maa

# Establish initial values based on data structures.  Start line 78 Aginit.for
drops.initializeDrops() # lines 81-85 are done using the drops object function
acraft.userData('Air Tractor AT-602', control.boomht, lfmaa) # lines 113-134 done in object
control.initializeControl(acraft.lmvel, acraft.s, terrain.zref) # line 155 Aginit.for sets ldry, line 274:swath; lines 303-329
control.changeSwathWidth(swath_width)

# The below are added just to make testing easier
yhorz = 4.267
dropnumber = 10

# Lines 155 - 164 are done in nozz Object initialization.  1 nozzle, 0,0,0 spacing.  Needs changing later
nozz.setNozLocArrays()
nozz.setNozHorizontal(yhorz)
nozz.findExtremePositions(acraft) # lines 122-231, 243-244 Aginit.for
nozz.setInitialDropsPositsVels(acraft, terrain, control.boomht) # lines 246-270 Aginit.for
spray.calcFlows(acraft.uo, control.boomht) # lines 278 - 297 Aginit.for
spray.setEvapParams() # lines 303-320 Aginit.for
met.changeMet(wind_speed) # Wind speed must be changed prior to initializing Met.
met.initializeMet(lfmaa) # lines 331-336 Aginit.for

# Canopy stuff and Multiple Application assessment stuff to be added later to this later.
# The above completes set the parameters that are outside the various input Objects
# And completes the execution of Aginit.for
# Next, Aglims.for which finishes the initialization process prior to model execution.
# hordep = horizontalDepPlanes() # Instantiate horizontal deposition plane object
# hordep.initializePlanes(control.ydepx, control.ndepr, control.nswth, control.swath, control.ydepx2)
vertflx.verticalFluxPlane(control.zflxn, control.zflxx, control.nflxr)
drops.transferDropSizeDistribution()
drops.initializeComputedDSdistribution()
control.setInitialValues(terrain.cts)

# Set turbulence level in discrete wind speed, only for table of met data
# Set canopy constants, only if canopy
totacct = totalAccountArrays() # Instantiate total account array object
totacct.initiate_time_arrays(control.tmax)
totacct.initiate_distance_arrays(control.ygrid2)
totacct.initiate_height_arrays(terrain.zref, control.boomht, acraft.s)

# Now, Agdrop is run iteratively over each droplet size bin
# Below will just be for a single bin, but will modify to iterate later.

''' 
Agdrop.for
nndrop: drop size bin number is "sent" in.
'''
# Set current drop data and initialize stuff
nndrop = dropnumber # This is sent to Agdrop.for, specifies the bin to use.

# The next couple of items are Agdrop.for lines 38-97
rund = runData()
rund.initializeRunData(drops.ddiamn[nndrop], spray.vfrac, drops.dmassn[nndrop], nozz.nvar,
                       control.lvtflg, nozz.xs, terrain.sts, terrain.cts, acraft, nozz.xos)
rund.setDepAndFluxFlags(drops.dmassn[nndrop], control.swath, terrain.cts, spray.afrac, nozz.nvar,
                        control.nswth)

rund.initializePlanes(control)
# Set time old
rund.setOldTime(0.0) # This may, or may not, be needed

'''
Now, xv is sent to Ageqn.for.
'''
# Save initial positions
saved = saveData()
saved.initiateResults(nozz.nvar)

rund.initialPositions(control.nswth, control.iboom, nozz.nvar, control.sdisp)
# Calling the saved class outside of the rund class to save the initial positions:
saved.checkTime(rund.xv, 0.0, rund.edov, rund.diam, rund.isw)
# Increment deposition, this is technically called as part of agsave
agcon(0.0, saved, rund, terrain.zref, spray.afrac, nozz.ihalf)
# Increment flux
# Need to make this function: Agvrf.for, also called as part of Agsav
# Then there are SCIPUFF, CALPUFF stuff as well.
# Agsav.for also sets told = t current in line 150
rund.setOldTime(0.0)

rund.initializeIntegration(acraft.nprp)
# # Start integration to tmax process
# rund.setTermVandDt()
# # Get background at initial locations
# dv = agBkg(rund, nozz.nvar,
#       acraft.nvor, acraft.rlim, acraft.nprp, control.lspflg, met.lmcrs, met.zo,
#       met.pstab, canopy.lcanf, canopy.hcan, met.ccw, met.scw,
#       spray.denf, spray.denn, control.ldry, met.qqmx, acraft.lmvel)

# Temporary arrays to hold continuous results for sanity plotting
xdata = []
ydata = []
zdata = []
tdata = []

accounting = False

while rund.iswc != 0:
#while rund.nstep < 500:

    #print('Time: %4.1f, Height: %5.2f, Drift: %4.1f, ' % (rund.t, rund.xv[2,0], rund.xv[1,0]) )
    xdata.append(rund.xv[0, 0])
    ydata.append(rund.xv[1, 0])
    zdata.append(rund.xv[2, 0])
    tdata.append(rund.t)
    #print('Dist: ',rund.xv[2,0], 'Ht: ',rund.xv[1,0])
    # Iterating through each nozzle position
    for i in range(nozz.nvar):
        # Start integration to tmax process
        rund.setTermVandDt()
        # Get background at initial locations
        dv = agBkg(rund, nozz.nvar,
                   acraft.nvor, acraft.rlim, acraft.nprp, control.lspflg, met.lmcrs, met.zo,
                   met.pstab, canopy.lcanf, canopy.hcan, met.ccw, met.scw,
                   spray.denf, spray.denn, control.ldry, met.qqmx, acraft.lmvel)

        #print('Time: ', rund.t)
        if rund.isw[i] != 0:
            # Solve Equations of Motion
            xnv = solveEOM(rund, nozz.nvar, i, dv, terrain.ctu, terrain.cts, terrain.stu,
                           terrain.sts, terrain.zref)
            if accounting == True:
                # Canopy Dep and Total Height Accountancy
                etem, ctem, cnew = canDepTotHtAccount(i, rund, totacct, xnv, terrain.zref, canopy.hcan)
                # Total Accountancy in Distance
                totalAccountDistance(i, rund, totacct, xnv, etem, ctem, cnew)
                # Total Accountancy in Time
                totalAccountTime(i, rund, totacct, xnv, etem, ctem, cnew, control.ygrid2)
            else:
                cnew = rund.cmass[i]
                etem = rund.ymass * rund.cmass[i] * ((rund.edov[i] / rund.diam) ** 3 - (rund.ednv[i] / rund.diam) ** 3)
                ctem = rund.ymass * (rund.cmass[i] - cnew) * (rund.ednv[i] / rund.diam) ** 3

            # Update parameters and flags and get DSDs (lines 169-212 Ageqn.for
            updateParamsGetDSDs(i, rund, xnv, etem, cnew, control, nozz.ihalf) # efrac and yflxv should move from control to rund
    # Determine Positions of new vortices
    newVorts(rund, acraft.lmvel, acraft.nvor, acraft.rlim, acraft.nprp, control.lspflg, met.lmcrs,
             met.zo, met.pstab, canopy.lcanf, canopy.hcan, met.ccw, met.scw)
    # Correct circulation decay
    correctCirculationDecay(rund, xnv, acraft.nvor, met.gdk, met.gdko, met.bstab, nozz.nvar)
    # Correct Parameters
    correctParameters(rund, acraft.uo, acraft.nprp)
    # Check solution and continue
    checkSolnContinue(rund, nozz.nvar, xnv, control.grdmx, control.tmax)

    i = max(1, int(200.0 / rund.dmin))
    if (rund.nstep % i) == 0:
        rund.mswc = 1
    if rund.mswc == 1:
        saved.checkTime(rund.xv, rund.t, rund.edov, rund.diam, rund.isw)
        agcon(rund.t, saved, rund, terrain.zref, spray.afrac, nozz.ihalf)
        rund.setOldTime(rund.t)

saved.checkTime(rund.xv, -rund.t, rund.edov, rund.diam, rund.isw)
agcon(-rund.t, saved, rund, terrain.zref, spray.afrac, nozz.ihalf)

# Convert out list data to arrays
xdata = np.array(xdata) * 3.28084
ydata = np.array(ydata) * 3.28084
zdata = np.array(zdata) * 3.28084
tdata = np.array(tdata)

fig = plt.figure(figsize=(15,10), constrained_layout=True)

ax = fig.add_subplot()
# 1 = Time by Height
# 2 = Time by Distance
# 3 = Height by Distance

option = 3

if option == 1:
    ax.scatter(tdata,zdata)
    ax.set_ylabel('Height (ft)', size=16)
    ax.set_xlabel('Time (s)', size=16)

elif option == 2:
    ax.scatter(tdata,ydata)
    ax.set_ylabel('Distance (ft)', size=16)
    ax.set_xlabel('Time (s)', size=16)

elif option == 3:
    ax.scatter(ydata, zdata)
    ax.set_ylabel('Height (ft)', size=16)
    ax.set_xlabel('Distance (ft)', size=16)


    ax.set_ylim(0,25)
    ax.set_xlim(-40,115)
    start, end = ax.get_xlim()
    stepsize = 5
    ax.xaxis.set_ticks(np.arange(start, end, stepsize))


ax.yaxis.set_tick_params(labelsize=16)
ax.xaxis.set_tick_params(labelsize=16)

plt.show()

# fig = plt.figure(figsize=(15,10), constrained_layout=True)

# ax = fig.add_subplot()
# ax.scatter(ydata,zdata)
#
# ax.set_ylim(0,10)
# ax.set_xlim(-5,100)
# start, end = ax.get_xlim()
# stepsize = 5
# ax.xaxis.set_ticks(np.arange(start, end, stepsize))
#
# ax.set_ylabel('Height (m)', size=16)
# ax.yaxis.set_tick_params(labelsize=16)
#
# ax.set_xlabel('Downwind Distance (m)', size=16)
# ax.xaxis.set_tick_params(labelsize=12)
#
# ax.yaxis.set_tick_params(labelsize=16)
# ax.xaxis.set_tick_params(labelsize=16)
#
# plt.show()









