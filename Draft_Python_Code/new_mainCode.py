
import math
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

from new_controlDataClass import controlData
from new_aircraftDataClass import aircraftData
from new_terrrainDataClass import terrainData
from new_canopyDataClass import canopyData
from new_metDataClass import metData
from new_dropletDataClass import dropletData
from new_nozzleDataClass import nozzleData
from new_sprayMaterialDataClass import sprayMaterialData
from new_verticalFluxClass import verticalFluxData
from new_totalAccountArraysClass import totalAccountArrays
from new_runDataClass import runData
from new_agSaveClass import saveData
from new_agCon import agcon
from new_agBkg import agBkg
from new_solveEOM import solveEOM
from new_canopyDepAndTotHtAccount import canDepTotHtAccount
from new_totalAccountDistance import totalAccountDistance
from new_totalAccountTime import totalAccountTime
from new_updateParamsGetDSDs import updateParamsGetDSDs
from new_determineNewVorticePosits import newVorts
from new_correctCirculationDecay import correctCirculationDecay
from new_correctParameters import correctParameters
from new_checkSolnAndContinue import checkSolnContinue

# maa is sent into Aginit
maa = 0 # -1, 0 = no and a #=wind speed index: Multiple Application Assessment I think
# Constants
tpi = 6.2831853 # 2*pi

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
control.changeBoomHeight(5.0)
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
acraft.userData('Air Tractor AT-401', control.boomht, lfmaa) # lines 113-134 done in object
control.initializeControl(acraft.lmvel, acraft.s, terrain.zref) # line 155 Aginit.for sets ldry, line 274:swath; lines 303-329

# Lines 155 - 164 are done in nozz Object initialization.  1 nozzle, 0,0,0 spacing.  Needs changing later
nozz.setNozLocArrays()
nozz.findExtremePositions(acraft) # lines 122-231, 243-244 Aginit.for
nozz.setInitialDropsPositsVels(acraft, terrain, control.boomht) # lines 246-270 Aginit.for
spray.calcFlows(acraft.uo, control.boomht) # lines 278 - 297 Aginit.for
spray.setEvapParams() # lines 303-320 Aginit.for
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
nndrop = 10 # This is sent to Agdrop.for, specifies the bin to use.
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

while rund.iswc != 0:
#while rund.nstep < 200:

    #print('Time: %4.1f, Height: %5.2f, Drift: %4.1f, ' % (rund.t, rund.xv[2,0], rund.xv[1,0]) )
    xdata.append(rund.xv[0, 0])
    ydata.append(rund.xv[1, 0])
    zdata.append(rund.xv[2, 0])
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
            # Canopy Dep and Total Height Accountancy
            etem, ctem, cnew = canDepTotHtAccount(i, rund, totacct, xnv, terrain.zref, canopy.hcan)
            # Total Accountancy in Distance
            totalAccountDistance(i, rund, totacct, xnv, etem, ctem, cnew)
            # Total Accountancy in Time
            totalAccountTime(i, rund, totacct, xnv, etem, ctem, cnew, control.ygrid2)
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





fig, ax = plt.subplots()
ax.scatter(ydata,zdata)

ax.set_ylabel('Height (m)', size=12)
ax.yaxis.set_tick_params(labelsize=12)

ax.set_xlabel('Downwind Distance (m)', size=12)
ax.xaxis.set_tick_params(labelsize=12)

plt.show()









