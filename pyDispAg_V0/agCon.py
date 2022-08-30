'''
This function replicates the fortran Agcon.for subroutine.

tnew = Time
ans = Trajectory results array.

AGCON computes the continuous ground deposition pattern.
isw =   1 Active drop above surface
        0 Drops hits the surface and penetrates
        -1 Four standard deviations below the surface and finish
'''

import math
import numpy as np
from agDep import agdep

def agcon(tnew, saved, rund, zref, afrac, ihalf):
    xv = np.zeros([3]) # This is a stand-alone variable xv that gets reset and reused every iteration of Agcon
    ans = saved.answ
    if tnew >= 0:
        rund.dte = tnew - rund.told
        for n in range(saved.nvar):
            if rund.isw[n] != 0:
                saved.xndep[0 ,n] = ans[0 ,n]
                saved.xndep[1 ,n] = ans[1 ,n] - zref
                saved.xndep[2 ,n] = ans[2 ,n]
                saved.dsdep[n] = afrac
            else:
                for i in range(3):
                    saved.xndep[i ,n] = saved.xndep[i,n] + rund.dte * saved.dndep[i ,n]

            iss = 0 # This is actually is in the fortran code

            if rund.idepv[n] >= 0:
                xv[0] = saved.xndep[0 ,n] # This is a different xv that is used throught, this starts empty and is filled
                xv[1] = saved.xndep[1 ,n] # Using xv as name to match fortran code
                xv[2] = saved.xndep[2 ,n]
                ''' 
                The following call to Agdep from Fortran sends in the following parameters which are then set
                to the indicated parameters in Agdep.
                xv (above):         av = current x,y,z spread
                dndep[1,n]:         dv = v,w spread
                dte(above):         dt = time step
                dsdep[n]:           dmcv = current volume ratio
                ydeps:              ymn = minimum y location
                ddepr:              dy = y increment
                ndeps:              nvec = number of y points
                temnd*cmass[n]:     temnd = units normalization
                zdeps:              zvecs = results array for full deposition
                zdeph:              zvech = results array for upwind deposition
                ihalf[n]:           ihf = half boom flag
                i:                  igk = activity flag
                
                Other than xv, dte and ihalf, the remaining parameters are in the rund or saved object.
                ihalf is in the nozz object.
                
                These should be set first and sent in as there is a different call to Agdep later with a few
                of the inputs modified
                
                av = xv (above) will send in as xv and set to av in Agdep
                dv = saved.dndep[1, n]
                dte = dte (above)
                dmcv = saved.dsdep[n]
                ymn0 = rund.ydeps  This is an array, but agdep uses and singular point float, just set to  min
                dy = rund.ddepr
                nvec = rund.ndeps
                temnd = rund.temnd * rund.cmass[n]
                zvecs = rund.zdeps
                zvech = rund.zdeph
                ihf = ihalf[n]
                '''
                zvecs, zvech, i = agdep(xv, saved.dndep[:,n], rund.dte, saved.dsdep[n], rund.ydeps, rund.ddepr,
                               rund.ndeps, rund.temnd*rund.cmass[n], rund.zdeps, rund.zdeph, ihalf[n])
                rund.zdeps = zvecs
                rund.zdeph = zvech
                if i == 0 and rund.idepv[n] == -1:
                    iss = 1
            if iss == 1:
                rund.idepv[n] = -1
            if rund.isw[n] < 0 and rund.idepv[n] > 0:
                rund.idepv[n] = 0

    # Extend deposition for active drops below the surface
    else:
        timee = rund.told
        tmaxe = 10.0*timee
        dtee = rund.dte # Fortran does this, but dte is set here but only if tnew >= 0
        # Will assume that dte should somehow be persistent and will add it to the rund object.
        timee = timee + dtee
        l = 0
        for n in range(saved.nvar):
            if rund.isw[n] == 0:
                for i in range(3):
                    saved.xndep[i,n] = saved.xndep[i,n] + dtee * saved.dndep[i,n]
                if rund.idepv[n] == 0:
                    l = l+1
                    xv[0] = saved.xndep[0, n]
                    xv[1] = saved.xndep[1, n]
                    xv[2] = saved.xndep[2, n]
                    zvecs, zvech, i = agdep(xv, saved.dndep[:, n], dtee, saved.dsdep[n], rund.ydeps, rund.ddepr,
                                            rund.ndeps, rund.temnd * rund.cmass[n], rund.zdeps, rund.zdeph, ihalf[n])
                    rund.zdeps = zvecs
                    rund.zdeph = zvech
                    if i == 0:
                        rund.idepv[n] = -1
        dtee = 1.1 * dtee
        while l != 0 and timee < tmaxe: # This is a bit of a weird goto loop in fortran, this, combined with the previous few lines is a stopgap fix
            timee = timee + dtee
            l = 0
            for n in range(saved.nvar):
                if rund.isw[n] == 0:
                    for i in range(3):
                        saved.xndep[i, n] = saved.xndep[i, n] + dtee * saved.dndep[i, n]
                    if rund.idepv[n] == 0:
                        l = l + 1
                        xv[0] = saved.xndep[0, n]
                        xv[1] = saved.xndep[1, n]
                        xv[2] = saved.xndep[2, n]
                        zvecs, zvech, i = agdep(xv, saved.dndep[0, n], dtee, saved.dsdep[n], rund.ydeps, rund.ddepr,
                                                rund.ndeps, rund.temnd * rund.cmass[n], rund.zdeps, rund.zdeph, ihalf)
                        rund.zdeps = zvecs
                        rund.zdeph = zvech
                        if i == 0:
                            rund.idepv[n] = -1
            dtee = 1.1 * dtee
