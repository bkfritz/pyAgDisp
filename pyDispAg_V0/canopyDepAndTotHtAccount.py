import math
import numpy as np
''' 
Replicates lines 101-137 AgEqn.for.
Canopy Deposition and total accountancy in height
'''

def canDepTotHtAccount(i, rund, totacct, xnv, zref, hcan):

    xov = rund.xv.copy()
    # Canopy deposition and total accountancy in height
    cnew = rund.cmass[i]
    # AGDISP has if, then here that sends cnew to Agcan if lcanf != 0, i.e. there is a canopy
    # cnew will only change here as a result of canopy deposition
    # NOTE cnew MAY change as a result of canopy deposition, need to return cnew
    etem = rund.ymass * rund.cmass[i] * ((rund.edov[i] / rund.diam) ** 3 - (rund.ednv[i] / rund.diam) ** 3)
    ctem = rund.ymass * (rund.cmass[i] - cnew) * (rund.ednv[i] / rund.diam) ** 3
    atem = rund.ymass * (rund.cmass[i] - cnew) # This gets assigned but only used line 132 which is commented out.
    dh = xnv[2, i] - xov[2, i]  # Change in height, z, from last time step
    nh = max(int(abs(dh) / 0.1) + 1, 2)
    dh = dh / (nh - 1)

    if ctem > 0:
        nc = 0
        for n in range(1, nh):
            print(n)
            htem = xov[2, i] - zref + (n - 1) * dh
            if htem > 0 and htem < (hcan - zref): nc = nc + 1
        if nc > 0:
            temnc = 1 / nc
        else:
            temnc = 0

    for n in range(1, nh):
        htem = xov[2, i] - zref + (nh - 1) * dh
        ih = max(min(int(htem / totacct.dahh) + 1, totacct.nahh), 1)
        totacct.tahfv[0, ih] = totacct.tahfv[0, ih] + etem / (nh - 1)

        if ctem > 0:
            if (htem > 0 and htem < (hcan - zref)):
                totacct.tahfv[1, ih] = totacct.tahfv[1, ih] + temnc * ctem
        if rund.isw[i] < 0:
            totacct.tahfv[2, ih] = totacct.tahfv[2, ih] + rund.ymass * cnew * (rund.ednv[i] / rund.diam) ** 3

    return etem, ctem, cnew