import math

def agVelTraj(x, y, z, nvor, rlim, nprp, lspflg, lmcrs, zo, pstab, lcanf, hcan, ccw, scw,
              traj, printvals):
    '''
     AGVEL.for
     x, y, z are locations
     u, v, w are velocities
     lspflg = SCIPUFFFLAG 0=not invoked, 1=is invoked

     '''

    u = 0
    v = 0
    w = 0

    if z > 0:
        # There is a goto here that sends it to mean crosswind if nvor = 0: ground sprayer

        if x >= 0:

            for n in range(nvor):
                # Quadrant 1 vortex
                r = max(0.01, math.sqrt(abs((y - traj.ybar[n]) ** 2 + (z - traj.zbar[n]) ** 2)))
                b = traj.g2pi[n] * traj.gdkv[n] / max(r, rlim) / r
                v = v - b * (z - traj.zbar[n])
                w = w + b * (y - traj.ybar[n])
                if printvals == True:
                    print('qs', (y - traj.ybar[n]), (y - traj.ybal[n]))
                #print('q1',u,v,w)

                # Quadrant 2 vortex
                r = max(0.01, math.sqrt(abs((y - traj.ybal[n]) ** 2 + (z - traj.zbal[n]) ** 2)))
                b = traj.g2pi[n] * traj.gdkv[n] / max(r, rlim) / r
                v = v + b * (z - traj.zbal[n])
                w = w - b * (y - traj.ybal[n])
                #print('q2',u, v, w)

                # Quadrant 3 vortex
                r = max(0.01, math.sqrt(abs((y - traj.ybal[n]) ** 2 + (z + traj.zbal[n]) ** 2)))
                b = traj.g2pi[n] * traj.gdkv[n] / max(r, rlim) / r
                v = v - b * (z + traj.zbal[n])
                w = w + b * (y - traj.ybal[n])
                #print('q3',u, v, w)

                # Quadrant 4 vortex
                r = max(0.01, math.sqrt(abs((y - traj.ybar[n]) ** 2 + (z + traj.zbar[n]) ** 2)))
                b = traj.g2pi[n] * traj.gdkv[n] / max(r, rlim) / r
                v = v + b * (z + traj.zbar[n])
                w = w - b * (y - traj.ybar[n])
                #print('q4', u, v, w)

        # Propeller values for fixed wing
        if nprp != 0:
            for n in range(nprp):
                if x >= traj.xprp[n]:
                    r = math.sqrt(abs((y - traj.yprp[n]) ** 2 + (z - traj.zprp[n]) ** 2))
                    e = 15.174 * r / traj.cpxi[n]
                    ua = 11.785 * traj.cpur / traj.cpxi[n] / ((1.0 + 0.25 * e * e) ** 2)
                    va = 5.894 * traj.cpur * (1.0 - 0.25 * e * e) / (traj.cpxi[n] ** 2) / ((1.0 + 0.25 * e * e) ** 2)
                    vs = traj.vprp[n] / traj.rprp[n]
                    if r > traj.rprp[n]:
                        vs = 0
                    #print('y - acraft.yprp',y - acraft.yprp[n],)
                    u = u + ua
                    v = v + (va * (y - traj.yprp[n])) + (vs * (z - traj.zprp[n]))
                    w = w + (va * (z - traj.zprp[n])) - (vs * (y - traj.yprp[n]))

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

        return u, v, w
    else:
        return u, v, w