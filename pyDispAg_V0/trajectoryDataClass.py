from dataclasses import dataclass, field
import numpy as np
import math
from agWetBulbTemp import wetBulbTemp

@dataclass
class trajData:
    '''
Initiates, holds, and advances the solutions for each droplet diameter.

    '''

    def initializeRunData(self, diam, vfrac, dmass, nvar, lvtflg, xs, sts, cts, acraft, xos):

        # Testing droplet size and limit between max and min
        self.diam = max(min(diam, 2000.0), 5.0)
        self.dcut = max(self.diam * (1.0-vfrac)**0.33333, 2.0)
        self.ymass = dmass / nvar
        self.xo = xos # xos is from nozzle object, x position

        self.sv = np.zeros([6])
        self.av = np.zeros([3,nvar])
        self.tslim = np.zeros([6])
        self.jav = np.ones([nvar])


        # the following is lines 56-72 Agdrop.for
        self.isw = np.ones([nvar]) # 1=active droplet above surface, 0=drop hits penetrates, -1=4 stdev below surf and finish
        #self.ivt = np.full((nvar),lvtflg)
        xov = np.zeros([9,nvar])
        for n in range(nvar):
            for k in range(9):
                xov[k, n] = xs[k, n]
            # The following uses the initial nozzle state (x,y,z,u,v,w,et...) data and sets equal to initial state
            xov[1, n] = xs[2, n] * sts + xs[1, n] * cts
            xov[2, n] = xs[2, n] * cts - xs[1, n] * sts
        self.xov = xov.copy() # this xv, or rund.xv will iterate and hold the results for each time step, droplet and nozzle location
        self.edov = np.full((nvar), self.diam)
        self.ednv = np.full((nvar), self.diam)
        self.cmass = np.ones([nvar])

        # Ignoring the discrete receptor stuff below for now.
        # if iidis == 1:
        #     for nr in range(nndsr):
        #         ntdsr[nr, n] = itdsr[nr]
        if acraft.nvor > 0:
            g2pi = np.zeros([nvar])
            ybar = np.zeros([nvar])
            zbar = np.zeros([nvar])
            ybal = np.zeros([nvar])
            zbal = np.zeros([nvar])
            for n in range(acraft.nvor):
                g2pi[n] = acraft.g2pis[n]
                ybar[n] = acraft.zbars[n]*sts + acraft.ybars[n]*cts
                zbar[n] = acraft.zbars[n]*cts - acraft.ybars[n]*sts
                ybal[n] = acraft.zbals[n] * sts + acraft.ybals[n] * cts
                zbal[n] = acraft.zbals[n] * cts - acraft.ybals[n] * sts
            self.gdkv = np.ones([acraft.nvor])
        self.cpur = acraft.cpur # This should likely just stay with acraft, but will put here for now
        self.g2pi = g2pi
        self.ybar = ybar
        self.zbar = zbar
        self.ybal = ybal
        self.zbal = zbal

        # Additional if lmvel == 2 stuff here for heli
        # need to add here later
        if acraft.nprp > 0:
            self.xprp = np.zeros([acraft.nprp])
            self.rprp = np.zeros([acraft.nprp])
            self.vprp = np.zeros([acraft.nprp])
            self.cpxi = np.zeros([acraft.nprp])
            self.yprp = np.zeros([acraft.nprp])
            self.zprp  = np.zeros([acraft.nprp])
            for n in range(acraft.nprp):
                self.xprp[n] = acraft.xprps
                self.yprp[n] = acraft.zprps*sts + acraft.yprps[n]*cts
                self.zprp[n] = acraft.zprps*cts - acraft.yprps[n]*sts
                self.rprp[n] = acraft.rprps
                self.vprp[n] = acraft.vprps
                self.cpxi[n] = acraft.cpxis

    def calcWetBulbTemp(self, temp, rhum, pres):
        self.dtemp = wetBulbTemp(temp, rhum, pres)

    def setScaleSizes(self, boomht, erate, zo, scw):

        tt = 0.0
        self.yy = 0.0
        dnew = self.diam
        hh = boomht
        dh = 0.1*boomht
        for nn in range(10):
            wtn = 1.0
            dtau = 3.21e-06 * dnew * dnew
            wto = wtn
            reyno = 0.0688 * dnew * wto
            dtem = dtau / (1.0 + 0.197 * reyno ** 0.63 + 0.00026 * reyno ** 1.38)
            wtn = 9.8 * dtem
            while abs(wtn/wto - 1.0) > 0.0001: # This is a goto loop in fortran
                wto = wtn
                reyno = 0.0688 * dnew * wto
                dtem = dtau/ (1.0+ 0.197 * reyno**0.63 + 0.00026 * reyno**1.38)
                wtn = 9.8 * dtem
            dt = dh / wtn
            tt = tt + dt
            if (erate * tt * self.dtemp / self.diam / self.diam) > 1.0:
                break
            hh = max(hh - dh, zo)
            bb = math.log((hh + zo) / zo)
            self.yy = self.yy - bb * scw * dt
            self.dnew = self.diam * math.sqrt(abs(1.0 - erate * tt * self.dtemp / self.diam / self.diam))

    def setSV(self, boomht, s, windsp, gdk, nvar):

        # The following sv1 data setting don't seem to be used for anything, but adding here just in case
        # This is from lines 68-73 agtraj.for
        sv1 = np.zeros([6])
        sv1[0] = -2.0 * boomht
        sv1[1] = boomht
        sv1[2] = -1.2 * s
        sv1[3] = min(1.2 * s + self.yy, 20.0 * s * windsp / gdk)
        sv1[4] = 0.0
        sv1[5] = boomht + 0.5 * s
        self.sv1 = sv1.copy()

        # sv = np.zeros([3,nvar])
        # for i in range(nvar):
        #     for j in range(3):
        #         sv[j,i] = self.xov[j,i]
        # self.sv = sv.copy()

    def setOldTime(self,told):
        self.told = told


    def initializeIntegration(self, nprp, nvar):
        av = np.zeros([3,nvar])
        jav = np.zeros([nvar])
        self.dt = 0.0
        self.t = 0.0
        self.dmin = self.diam
        self.iswc = nvar # Trajectory flag I think
        self.istt = 0
        if nprp != 0:
            for n in range(nprp):
                self.cpxi[n] = self.cpxi[n] + self.xo-self.xprp[n]

        for i in range(nvar):
            av[0,i] = -1*self.xov[0,i]
            av[1,i] = self.xov[1,i]
            av[2,i] = self.xov[2,i]
            jav[i] = 1
        self.av = av.copy()
        self.jav = jav.copy()

        self.nstep = 0
        self.ntraj = 1
        self.avdst = 0.0

    def integrate(self, nvar):
        sv = np.zeros([3, nvar])

        for i in np.arange(0, nvar, 1):
            for j in np.arange(0, 2, 1):
                sv[j, i] = self.xov[j, i]
        self.sv = sv.copy()

        self.nsave = self.nstep + 1
        self.ntraj = self.ntraj + 1
        self.nstep = self.nstep + 1
        self.uterm = 9.58 * (1.0-math.exp(-1.0*(self.dmin/1700.0)**1.147))
        self.dt = 0.0002*self.dmin/self.uterm














