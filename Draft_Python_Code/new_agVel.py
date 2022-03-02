import math
import numpy as np

def agVel(x, y, z, rund, nvor, rlim, nprp, lspflg, lmcrs, zo, pstab, lcanf, hcan, ccw, scw):
    '''
     AGVEL.for
     x, y, z are locations
     u, v, w are velocities
     lspflg = SCIPUFFFLAG 0=not invoked, 1=is invoked

     '''

    u = 0
    v = 0
    w = 0

    while z > 0:
        # There is a goto here that sends it to mean crosswind if nvor = 0: ground sprayer
        if x >= 0:

            for n in range(nvor):
                # Quadrant 1 vortex
                r = max(0.01, math.sqrt(abs((y - rund.ybar[n]) ** 2 + (z - rund.zbar[n]) ** 2)))
                b = rund.g2pi[n] * rund.gdkv[n] / max(r, rlim) / r
                v = v - b * (z - rund.zbar[n])
                w = w + b * (y - rund.ybar[n])
                #print('q1',u,v,w)

                # Quadrant 2 vortex
                r = max(0.01, math.sqrt(abs((y - rund.ybal[n]) ** 2 + (z - rund.zbal[n]) ** 2)))
                b = rund.g2pi[n] * rund.gdkv[n] / max(r, rlim) / r
                v = v + b * (z - rund.zbal[n])
                w = w - b * (y - rund.ybal[n])
                #print('q2',u, v, w)

                # Quadrant 3 vortex
                r = max(0.01, math.sqrt(abs((y - rund.ybal[n]) ** 2 + (z + rund.zbal[n]) ** 2)))
                b = rund.g2pi[n] * rund.gdkv[n] / max(r, rlim) / r
                v = v - b * (z + rund.zbal[n])
                w = w + b * (y - rund.ybal[n])
                #print('q3',u, v, w)

                # Quadrant 4 vortex
                r = max(0.01, math.sqrt(abs((y - rund.ybar[n]) ** 2 + (z + rund.zbar[n]) ** 2)))
                b = rund.g2pi[n] * rund.gdkv[n] / max(r, rlim) / r
                v = v + b * (z + rund.zbar[n])
                w = w - b * (y - rund.ybar[n])
                #print('q4', u, v, w)

        # Propeller values for fixed wing
        if nprp != 0:
            for n in range(nprp):
                if x >= rund.xprp[n]:
                    r = math.sqrt(abs((y - rund.yprp[n]) ** 2 + (z - rund.zprp[n]) ** 2))
                    e = 15.174 * r / rund.cpxi[n]
                    ua = 11.785 * rund.cpur / rund.cpxi[n] / (1.0 + 0.25 * e * e) ** 2
                    va = 5.894 * rund.cpur * (1.0 - 0.25 * e * e) / (rund.cpxi[n] ** 2) / (1.0 + 0.25 * e * e) ** 2
                    vs = rund.vprp[n] / rund.rprp[n]
                    if r > rund.rprp[n]:
                        vs = 0
                    #print('y - acraft.yprp',y - acraft.yprp[n],)
                    u = u + ua
                    v = v + va * (y - rund.yprp[n]) + vs * (z - rund.zprp[n])
                    w = w + va * (z - rund.zprp[n]) - vs * (y - rund.yprp[n])
                    #print('prop',u, v, w)

        # Mean crosswind
        if lspflg == 1:
            dum=0
            # Can add in the CALL function and cals later
        else:
            if lmcrs == 1:
                if lcanf == 0:
                    b = math.log((z + zo)/zo) - pstab
                else:
                    if z <= hcan:
                        dum = 0 # Need to add canopy cals here
                u = u + b*ccw
                v = v - b*scw
                #print(u, v, w)

        return u, v, w
    else:
        return u, v, w