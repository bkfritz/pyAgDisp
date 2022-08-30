'''
This function replicates the fortran Agdep.for subroutine.

Agdep computes the continuous deposition contribution.

av = current x,y,z spread
dv = v,w spread
dt = time step
dmcv = current volume ratio
ymn = minimum y location
dy = y increment
nvec = number of y points
temnd = units normalization
zvecs = results array for full deposition
zvech = results array for upwind deposition
ihf = half boom flag
igk = activity flag
'''

import math
import numpy as np
def agdep(av, dv, dt, dmcv, ymn0, dy, nvec, temnd, zvecs, zvech, ihf):

    igk = 0
    # Agcon from fortran sends in ydeps as ymn, but ydeps is an array.  I think the following line fixes it
    ymn = np.min(ymn0)
    snew = math.sqrt(abs(av[2]))

    testvalue = 0.25 * abs(av[1])

    if snew > testvalue:
        xtem = 0.707107 * av[1] / snew
        ttem = 1.0 / (1.0 + 0.47047 * abs(xtem))
        etem = (ttem * (0.3480242 + ttem * (-0.0958798 +
                                           ttem * 0.7478556))*
                math.exp(-1.0* min(xtem*xtem, 25.0)))
        if xtem < 0:
            etem = 2.0 - etem
        ynew = av[0]
        znew = abs(av[1])
        iss = int(max(int((ynew - 4.0*snew - ymn))-1.0, 1))
        iee = int(min(int((ynew + 4.0*snew - ymn))+1.0, nvec))

        for i in range(iss, iee): # Need to double check this for loop, this is the same as Fortran, but does either end need adjusting?
            y = ymn + (i-1)*dy
            ytem = math.exp(-min(0.5 * ((y-ynew)/snew)**2.0, 25.0))
            ztem = math.exp(-min(0.5 * (znew/snew)**2.0, 25))
            dmdt1 = -0.5 * ytem * etem *dv[2]/ snew / av[2]
            dmdt2b = 0.5 * ytem * etem * (y-ynew)**2 * dv[2]/av[2]/snew/av[2]
            dmdt3a = -0.79788456*ytem*ztem* dv[1] / av[2]
            dmdt3b = 0.39894228*ytem*ztem*znew* dv[2] / av[2] / av[2]
            dmdt = dmdt1 + dmdt2b + max(dmdt3a, 0.0) + dmdt3b
            if dmdt > 0:
                zvecs[i] = zvecs[i] + dmdt*dt*temnd * dmcv
                if ihf == 1:
                    zvech[i] = zvech[i] *dmdt*dt*temnd*dmcv

        igk = 1

    else: # This is a goto loop in fortran that the above only runs if snew > testvalue, otherwise set igk, zvecs, zvech to be unchanged
        igk = igk
        zvecs = zvecs
        zvech = zvech

    return zvecs, zvech, igk