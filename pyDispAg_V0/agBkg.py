'''
This represents an attempt at developing the simplest working iteration of the AgDISP code.

For now, will ignore all "extras" like multiple assessment, pond deposition, vapor, and so on.

Will use default AT-401, no canopy and only 1 nozzles.

This is a working draft of Agbkg
'''

import math
import numpy as np
from agVel import agVel

def agBkg(rund, nvar,
          nvor, rlim, nprp, lspflg, lmcrs, zo, pstab, lcanf, hcan, ccw, scw,
          denf, denn, ldry, qqmx, lmvel):
    # ''' From here, AGBKG.for is called, send in xov and t, returning a background array, dv'''
    t = rund.t
    uxi = 0.0
    uvi = 0.0
    aevap = 0.24
    bevap = 0.24
    dv = np.zeros([6, nvar])

    # Look for drops
    vmax = 0.1
    # The xo value below is set line 45 agdrop and equals the acraft.xos value.

    for i in range(nvar):
        if rund.isw[i] != 0:
            x = rund.xo + rund.xv[0,i]
            y = rund.xv[1,i]
            z = rund.xv[2,i]

            # These x,y,z values then get sent into AGVEL.for to determine mean velocity at each position
            '''
            AGVEL.for
            x, y, z are locations
            u, v, w are velocities
            '''
            u, v, w = agVel(x, y, z, rund, nvor, rlim, nprp, lspflg, lmcrs,
                            zo, pstab, lcanf, hcan, ccw, scw, 'bkgd')
            #print('Dist: ',z, 'Ht: ',y)

            '''Out of AGVEL back to AGBKG'''
            # Determine mean velocity at the drop position
            vmax = max(vmax, math.sqrt(abs(rund.xv[4, i]**2 + rund.xv[5,i]**2)), math.sqrt(abs(v*v + w*w)))

            # Determine the decay constant
            vrel = math.sqrt(abs( (rund.xv[3,i]-u)**2 + (rund.xv[4,i]-v)**2 +
                                  (rund.xv[5,i]-w)**2 ) )

            # Time decay evaluation
            d = rund.edov[i]
            denc = ( (d**3-rund.dcut**3) * denf + rund.dcut**3 * denn) / d**3
            dtau = (3.12e-6) * d * d * denc
            etau = 0 # Protect for dry evaporation and calpuff
            reyno = 0.0688 * d * vrel

            if vrel > 0:
                if ldry == 0:
                    dtau = dtau / (1.0 + 0.197*reyno**0.63 + 0.00026*reyno**1.38)

            # Evaporation stuff here, Ageqn lines

            # Scale Length
            sl = 0.65 * z
            qq = 0
            if nvor > 0:
                for n in range(nvor):
                    r = math.sqrt(abs( (y-rund.ybar[n])**2 + (z-rund.zbar[n])**2))
                    sl = min(sl, 0.6*r)
                    r = math.sqrt(abs( (y-rund.ybal[n])**2 + (z-rund.zbal[n])**2))
                    sl = min(sl, 0.6*r)

            if sl != 0: # In fortran, there is a goto statement that sends to Evaluate Params if sl = 0

                # Turbulence
                if lmcrs == 1: # Wind type = 1 for single, 2 for table
                    qq = qqmx

                #if rund.lcanf > 0:
                    # Only does something if canopy

                if nprp != 0:
                    for n in range(nprp):
                        r = math.sqrt(abs( (y-rund.yprp[n])**2 + (z-rund.zprp[n])**2))
                        e = 15.174 * r / rund.cpxi[n]
                        ua = 11.785 * rund.cpur/rund.cpxi[n] / (1.0 + 0.25 * e*e)**2
                        qq = qq + 0.2034 * ua * ua

                # Determine Analytic turbulent correlations with the droplet
                if qq != 0:
                    wtau = sl / (vrel + 0.375*math.sqrt(qq))
                    c = t / wtau
                    expc = math.exp(-1*min(c, 25.0))
                    expt = 0
                    if d > 0:
                        expt = math.exp(-1*min(t/dtau, 25.0))
                    b = (dtau/wtau)**2

                    if abs(b-1.0) > 0.01:
                        sum1 = 0.5 * (3.0-b) / (b-1.0)**2
                        sum2 = 0.5 / (b-1.0)
                        xk1 = sum1 * (1.0-dtau/wtau) + sum2
                        xk2 = sum1 * (expc-expt*dtau/wtau) + sum2*expc*(1.0+c)
                        xk3 = sum1 * (expc-expt) + sum2 * expc * c
                    else:
                        xk1 = 0.375
                        xk2 = (3.0 + 3.0*c - c*c) * expc/8.0
                        xk3 = (5.0-c)*c*expc/8.0

                    xk4 = 0.5*(1.0+ expc*(c-1.0))
                    uxi = -1.0*dtau * xk1 + dtau*expt * (xk2-xk3*dtau/wtau) + wtau*xk4
                    uvi = xk1-expt*(xk2-xk3*dtau/wtau)

            # Evaluate background parameters
            dv[0,i] = dtau
            dv[1,i] = u
            dv[2,i] = v
            if lmvel == 0:
                dv[3,i] = 0.0
            else:
                dv[3,i] = w
            dv[4,i] = uxi*qq/3.0
            dv[5,i] = uvi*qq/3.0

    return dv
