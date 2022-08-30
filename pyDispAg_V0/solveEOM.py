import math
import numpy as np

def solveEOM(rund, nvar, i, dv, ctu, cts, stu, sts, zref):
    '''
    Replicates lines 63-99 Ageqn.for
    '''

    dt = rund.dt
    xov = rund.xv.copy()
    xnv = np.zeros([9,nvar])

    expt = 0.0
    if dv[0, i] > 0.0:
        expt = math.exp(-1 * min(dt / dv[0, i], 25.0))

    tem1 = dv[1, i] + 9.8 * stu * dv[0, i]
    tem2 = xov[3, i] - tem1
    xnv[0, i] = xov[0, i] + tem1 * dt + tem2 * dv[0, i] * (1.0 - expt)
    xnv[3, i] = tem1 + tem2 * expt

    tem1 = dv[2, i] - 9.8 * ctu * sts * dv[0, i]
    tem2 = xov[4, i] - tem1
    xnv[1, i] = xov[1, i] + tem1 * dt + tem2 * dv[0, i] * (1.0 - expt)
    xnv[4, i] = tem1 + tem2 * expt

    tem1 = dv[3, i] - 9.8 * ctu * cts * dv[0, i]
    tem2 = xov[5, i] - tem1
    xnv[2, i] = xov[2, i] + tem1 * dt + tem2 * dv[0, i] * (1.0 - expt)
    xnv[5, i] = tem1 + tem2 * expt

    tem1 = dv[4, i] + dv[0, i] * dv[5, i]
    tem2 = xov[7, i] - dv[4, i] + (dv[0, i] * (xov[8, i] - 2.0 * dv[5, i]) )
    tem3 = xov[8, i] - dv[5, i]
    xnv[8, i] = dv[5, i] + tem3 * expt * expt
    xnv[7, i] = tem1 + tem2 * expt - tem3 * dv[0, i] * expt * expt
    xnv[6, i] = (xov[6, i] + 2.0 * tem1 * dt +
                 2.0 * tem2 * dv[0, i] * (1.0 - expt) -
                 tem3 * dv[0, i] * dv[0, i] * (1.0 - expt * expt)
                 )
    xnv[6, i] = max(0.0, xnv[6, i])
    xnv[8, i] = max(0.0, xnv[8, i])

    if xnv[2, i] <= zref:
        rate = (xov[2, i] - zref) / (xov[2, i] - xnv[2, i])
        for j in range(9):
            xnv[j, i] = xov[j, i] + rate * (xnv[j, i] - xov[j, i])
        xnv[2, i] = zref
        rund.isw[i] = -1

    return xnv