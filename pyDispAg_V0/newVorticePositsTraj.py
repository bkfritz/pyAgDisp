'''
Author: Brad Fritz
Date: February 2022
Project: AgDISP Fortran to Python

Description:  Porting AgDISP to Python.

This file contains the various helper functions used with the AgDISP code
'''

import numpy as np
import math
from agVelTraj import agVelTraj

def newVortsTraj(traj, lmvel, nvor, rlim, nprp, lspflg, lmcrs, zo, pstab, lcanf, hcan, ccw, scw):
    dt = traj.dt
    ynr = np.zeros([2])
    znr = np.zeros([2])
    ynl = np.zeros([2])
    znl = np.zeros([2])
    ynp = np.zeros([2])
    znp = np.zeros([2])
    # Determine the new positions of the vortices
    if lmvel != 0:
        for n in range(nvor):
            # acraft.xos is actually XO in fortran, but FORTRAN sets XO = XOS, does XOS Change later??
            # The FORTRAN code also has a call to tem in the sub call, i think just to not use the u value set
            # x, y, z, rund, nvor, rlim, nprp, lspflg, lmcrs, zo, pstab, lcanf, hcan, ccw, scw
            tem, vbar, wbar = agVelTraj(traj.xo, traj.ybar[n], traj.zbar[n], nvor, rlim, nprp,
                                    lspflg, lmcrs, zo, pstab, lcanf, hcan, ccw, scw, traj, False)

            ynr[n] = traj.ybar[n] + dt * vbar
            znr[n] = traj.zbar[n] + dt * wbar

            tem, vbal, wbal = agVelTraj(traj.xo, traj.ybal[n], traj.zbal[n], nvor, rlim, nprp,
                                    lspflg, lmcrs, zo, pstab, lcanf, hcan, ccw, scw, traj, False)
            ynl[n] = traj.ybal[n] + dt * vbal
            znl[n] = traj.zbal[n] + dt * wbal

            #print('Right: ', znr[n], 'Left: ', znl[n])

        if nprp != 0:

            for n in range(nprp):
                #print(traj.t, traj.yprp[n])
                tem, vbar, wbar = agVelTraj(traj.xo - traj.xprp[n],
                                        traj.yprp[n], traj.zprp[n], nvor, rlim, nprp,
                                    lspflg, lmcrs, zo, pstab, lcanf, hcan, ccw, scw, traj, False)
                ynp[n] = traj.yprp[n] + dt * vbar
                znp[n] = traj.zprp[n] + dt * wbar

        # Another set of cals here for heli, add later...

        for n in range(nvor):
            traj.ybar[n] = ynr[n]
            traj.zbar[n] = znr[n]
            traj.ybal[n] = ynl[n]
            traj.zbal[n] = znl[n]

        if nprp != 0:
            for n in range(nprp):
                traj.yprp[n] = ynp[n]
                traj.zprp[n] = znp[n]