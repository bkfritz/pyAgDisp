'''
Author: Brad Fritz
Date: February 2022
Project: AgDISP Fortran to Python

Description:  Porting AgDISP to Python.

This file contains the various helper functions used with the AgDISP code
'''

import numpy as np
from agVel import agVel

def newVorts(rund, lmvel, nvor, rlim, nprp, lspflg, lmcrs, zo, pstab, lcanf, hcan, ccw, scw):
    dt = rund.dt
    ynr = np.zeros([2])
    znr = np.zeros([2])
    ynl = np.zeros([2])
    znl = np.zeros([2])
    ynp = np.zeros([2])
    znp = np.zeros([2])
    flag='vort'
    # Determine the new positions of the vortices
    if lmvel != 0:
        for n in range(nvor):
            # acraft.xos is actually XO in fortran, but FORTRAN sets XO = XOS, does XOS Change later??
            # The FORTRAN code also has a call to tem in the sub call, i think just to not use the u value set
            # x, y, z, rund, nvor, rlim, nprp, lspflg, lmcrs, zo, pstab, lcanf, hcan, ccw, scw
            tem, vbar, wbar = agVel(rund.xo, rund.ybar[n], rund.zbar[n], rund, nvor, rlim, nprp,
                                    lspflg, lmcrs, zo, pstab, lcanf, hcan, ccw, scw, flag)
            ynr[n] = rund.ybar[n] + dt * vbar
            znr[n] = rund.zbar[n] + dt * wbar

            tem, vbal, wbal = agVel(rund.xo, rund.ybal[n], rund.zbal[n], rund, nvor, rlim, nprp,
                                    lspflg, lmcrs, zo, pstab, lcanf, hcan, ccw, scw, flag)
            ynl[n] = rund.ybal[n] + dt * vbal
            znl[n] = rund.zbal[n] + dt * wbal

        if nprp != 0:

            for n in range(nprp):
                tem, vbar, wbar = agVel(rund.xo - rund.xprp[n],
                                        rund.yprp[n], rund.zprp[n], rund, nvor, rlim, nprp,
                                    lspflg, lmcrs, zo, pstab, lcanf, hcan, ccw, scw, 'flag')
                ynp[n] = rund.yprp[n] + dt * vbar
                znp[n] = rund.zprp[n] + dt * wbar

        # Another set of cals here for heli, add later...

        for n in range(nvor):
            rund.ybar[n] = ynr[n]
            rund.zbar[n] = znr[n]
            rund.ybal[n] = ynl[n]
            rund.zbal[n] = znl[n]

        if nprp != 0:
            for n in range(nprp):
                rund.yprp[n] = ynp[n]
                rund.zprp[n] = znp[n]