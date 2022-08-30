import math

''' 
Replicates lines 169 - 212 AgEqn.for.
Updating parameters, flags and getting droplet size distribution data for various points and locations
'''

def updateParamsGetDSDs(i, rund, xnv, etem, cnew, control, ihalf):

    rund.efrac = rund.efrac + etem # this likely needs to be in rund as well

    # CALL AGDSD here canopy DSD, when canopy is present, cnew might be modified?  Sets DSCP
    rund.cmass[i] = cnew

    if rund.isw[i] < 0:
        rund.xdtot = rund.xdtot + rund.ymass * rund.cmass[i]

    for j in range(rund.nswtm):
        ytem = 0.0
        if j == 1 and control.iboom == 1:
            if ihalf[i] == 1:
                ytem = 1.0
        elif j == rund.nswtm and control.iboom == 1:
            if ihalf[i] == 0:
                ytem = 1.0
        else:
            ytem = 1.0

        tem = control.yedge2 + (j - 1) * control.swath

        if xnv[1, i] > tem and rund.iydv[i, j] == 0:
            rund.ydrft = rund.ydrft + ytem * rund.ymass * rund.cmass[i]
            rund.iydv[i, j] = 1

        tem = control.yflxv + (j - 1) * control.swath

        if xnv[1, i] > tem and rund.iyfv[i, j] == 0:
            rund.fdtot = rund.fdtot + ytem * rund.ymass * rund.cmass[i]
            rund.iyfv[i, j] = 1
            # CALL AGDSD for transport DSD, Sets DSVP

        if rund.isw[i] < 0:
            tef = (j - rund.sfac) * control.swath
            if xnv[1, i] < tef:
                dum = 0
                # CALL AGDSD for spray block DSD, Sets DSSB
            else:
                dum = 0
                # CALL AGDSD for downwind DSD, Sets DSDW

            tem = control.yflxv + (j - 1) * control.swath
            temy = 0.5 * (xnv[1, i] - tem)**2 / xnv[6, i]
            temm = ytem * (rund.ymass * rund.cmass[i] * math.exp(-1 * min(temy, 25))
                           / math.sqrt(xnv[6, i]))
            # CALL AGDSD for point DSD, sets DSDP

    # End of calcs for a given nozzle location
    # What parameters need to be returned, or better yet set into a class to hold the results