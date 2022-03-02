import math
import numpy as np

def checkSolnContinue(rund, nvar, xnv, grdmx, tmax):

    # Check solution and continue
    rund.t = rund.t + rund.dt
    rund.iswc = 0
    rund.mswc = 0
    rund.ivtt = 0

    for i in range(nvar):
        if rund.isw[i] != 0:
            rund.iswc = rund.iswc + 1
            for j in range(9):
                rund.xv[j ,i] = xnv[j ,i] # Set the current results in Ageqn to the where it is held in rund

        # Some evaporation stuff here, leave out for now

        if abs(xnv[1 ,i]) > grdmx:
            rund.isw[i] = -2
        if rund.isw[i] < 0:
            rund.mswc = 1
        # if drops.isw[i] < 0:
        # # iswc = 0 # I added this until I can figure out what iswc and mswc do.
        if rund.edov[i] < 2:
            rund.isw[i] = 0 # stop small droplets < 2 um

        rund.ivtt = rund.ivtt + rund.ivt[i]

    if rund.t >= tmax:
        rund.iswc = 0
        rund.mswc = 1


