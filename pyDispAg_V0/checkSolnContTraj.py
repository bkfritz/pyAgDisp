import math

def checkSolnContinueTraj(traj, nvar, xnv, grdmx, tmax, levap):

    # Check solution and continue
    traj.t = traj.t + traj.dt
    traj.iswc = 0
    avdst = 0.0
    xnewv = xnv.copy()

    for i in range(nvar):
        if traj.isw[i] != 0:
            traj.iswc = traj.iswc + 1
            avdst = avdst + (xnv[0,i] - traj.sv[0,i])**2 + (xnv[1,i] - traj.sv[1,i])**2 + (xnv[2,i] - traj.sv[2,i])**2

            for j in range(9):
                traj.xov[j, i] = xnewv[j , i]

            if levap != 0:
                traj.edov[i] = max(traj.ednv[i], traj.dcut)
                traj.dmin = min(traj.dmin, traj.edov[i])

            #print(xnv[1,i])
            if abs(xnewv[1 ,i]) > grdmx:
                traj.isw[i] = -2
            if traj.isw[i] < 0:
                traj.isw[i] = 0
            if traj.edov[i] < 2:
                traj.isw[i] = 0 # stop small droplets < 2 um

        else:
            if traj.nstep == traj.nsave:
                traj.jav[i] = 0

    avdst = math.sqrt(avdst / int(max(traj.iswc,1)))

    traj.avdst = avdst

    if traj.t >= tmax or traj.ntraj == 500:
        traj.iswc = 0

    #print(traj.iswc, traj.avdst)

    # Fortran has a goto statement here that goes back to nstep+1 if iswc>0 or avdst<1.0