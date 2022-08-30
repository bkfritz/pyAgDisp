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
from agBkgTraj import agBkgTraj
from solveEOM_Traj import solveEOM_Traj
from trajectoryDataClass import trajData
from newVorticePositsTraj import newVortsTraj
from correctCircDecayTraj import correctCircDecayTraj
from correctModelParamsTraj import correctModelParamsTraj
from checkSolnContTraj import checkSolnContinueTraj

control = controlData() # Instantiate control object
acraft = aircraftData() # Instantiate aircraft object
terrain = terrainData() # Instantiate terrain object
canopy = canopyData() # Instantiate canopy object
met = metData() # Instantiate met object
drops = dropletData() # Instantiate drops object
nozz = nozzleData() # Instantiate nozzle object
spray = sprayMaterialData() # Instantiate spray material object
traj = trajData()

lfmaa = 1

'''
The following are parameters to change boom height based on extent to wing tip, 
drop number for diameter, and wind speed.
'''
boomheight = 12 # feet, convert to m below
extent = .50
wind_speed = 2 # mph, convert to m below

dmass = 0.00141
diamv = 54.49

# The following will end up being basically Aginit.for
control.changeBoomHeight(boomheight * 0.3048)
control.changeNumSwaths(1)
control.getLineReps()
control.changeSwathWidth(60 * 0.3048)

# Establish initial values based on data structures.  Start line 78 Aginit.for
acraft.userData('Air Tractor AT-602', control.boomht, lfmaa) # lines 113-134 done in object
control.initializeControl(acraft.lmvel, acraft.s, terrain.zref) # line 155 Aginit.for sets ldry, line 274:swath; lines 303-329

# All units are in meters.  aircraft.s is the semispan.
yhorz = acraft.s * extent

# Lines 155 - 164 are done in nozz Object initialization.  1 nozzle, 0,0,0 spacing.  Needs changing later
nozz.setNozLocArrays()
nozz.setNozHorizontal(yhorz)
nozz.findExtremePositions(acraft) # lines 122-231, 243-244 Aginit.for
nozz.setInitialDropsPositsVels(acraft, terrain, control.boomht) # lines 246-270 Aginit.for
#spray.calcFlows(acraft.uo, control.boomht) # lines 278 - 297 Aginit.for
spray.setEvapParams() # lines 303-320 Aginit.for

'''   
THE TRAJECTORY DATA WAS OFF BECAUSE THE MET DATA WAS NOT SETTING THE WIND SPEED CORRECTLY.

THIS NEEDS TO BE FIXED IN ALL MET DATA CLASS OBJECT DATA CODE
'''

met.changeMet(wind_speed * 0.44704)
met.initializeMet(lfmaa) # lines 331-336 Aginit.for

# The above completes set the parameters that are outside the various input Objects
# And completes the execution of Aginit.for
# Next, Aglims.for which finishes the initialization process prior to model execution.


control.setInitialValues(terrain.cts)

traj.initializeRunData(diamv, spray.vfrac, dmass, nozz.nvar,
                       control.lvtflg, nozz.xs, terrain.sts, terrain.cts, acraft, nozz.xos)

traj.calcWetBulbTemp(met.temp, met.humidity, control.pramb)
traj.setScaleSizes(control.boomht, spray.erate, met.zo, met.scw)
traj.setSV(control.boomht, acraft.s, met.windspd, met.gdk, nozz.nvar)
traj.initializeIntegration(acraft.nprp, nozz.nvar)

# Setup arrays to hold the trajectory data
xdata = []
ydata = []
zdata = []
tdata = []
gdkvdata = []

while traj.iswc > 0 :# and traj.avdst< 1.0:

    xdata.append(traj.xov[0, 0])
    ydata.append(traj.xov[1, 0])
    zdata.append(traj.xov[2, 0])
    tdata.append(traj.t)
    gdkvdata.append(traj.gdkv[0])

    # Integrate to TMAX
    traj.integrate(nozz.nvar)

    # Next get Background conditions
    dv = agBkgTraj(traj, acraft.nvor, acraft.rlim, acraft.nprp, control.lspflg,
                   met.lmcrs, met.zo, met.pstab, canopy.lcanf, canopy.hcan, met.ccw, met.scw,
                   spray.denf, spray.denn, control.ldry, met.qqmx, acraft.lmvel, nozz.nvar)

    # Solve EOM
    xnv = solveEOM_Traj(dv, traj, nozz.nvar, terrain.stu, terrain.sts, terrain.ctu, terrain.cts,
                             terrain.zref)


    #print(traj.gdkv[0])
    # Determine new positions of vortices
    newVortsTraj(traj, acraft.lmvel, acraft.nvor, acraft.rlim, acraft.nprp, control.lspflg,
                 met.lmcrs, met.zo, met.pstab, canopy.lcanf, canopy.hcan, met.ccw, met.scw)

    #print(traj.t, traj.ybal[0], traj.zbal[0])

    # Correct circulation decay
    correctCircDecayTraj(traj, acraft.nvor, met.gdk, met.gdko, met.bstab)

    # Correct model parameters
    correctModelParamsTraj(traj, acraft.uo, acraft.nprp)

    # Check solution and continue
    checkSolnContinueTraj(traj, nozz.nvar, xnv, control.grdmx, control.tmax, spray.levap)

    for i in range(nozz.nvar):
        # Ignore the code in Fortran where NTRPL = 1 to inverse coordinates
        traj.av[0, i] = -1 * traj.xov[0,i]
        traj.av[1, i] = traj.xov[1, i]
        traj.av[2, i] = traj.xov[2, i]



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






