from dataclasses import dataclass, field
import numpy as np
import math
from new_agBkg import agBkg

@dataclass
class runData:
    '''
Initiates, holds, and advances the solutions for each droplet diameter.

    '''
    xdtot: float = field(default=0.0) # This is reset for every new droplet bin but creating a new object
    fdtot: float = field(default=0.0) # This is reset for every new droplet bin but creating a new object
    ydepn: float = field(default=-60, metadata={'unit': 'm'})  # Set by Agsome.for line 491
    nstep: int = field(default=0)
    lcpend: int = field(default=0)
    lspend: int = field(default=0)
    dte: float = field(default=0.0, metadata={'unit':'sec'}) # Add to make Agcon work, see notes there.

    def initializeRunData(self, diam, vfrac, dmass, nvar, lvtflg, xs, sts, cts, acraft, xos):

        self.diam = diam
        self.dcut = max(self.diam * (1.0-vfrac)**0.33333, 2.0)
        self.ymass = dmass / nvar
        self.xo = xos # xos is from nozzle object, x position

        # the following is lines 56-72 Agdrop.for
        self.isw = np.ones([nvar]) # 1=active droplet above surface, 0=drop hits penetrates, -1=4 stdev below surf and finish
        self.ivt = np.full((nvar),lvtflg)
        xv = np.zeros([9,nvar])
        for n in range(nvar):
            for k in range(9):
                xv[k, n] = xs[k, n]
            # The following uses the initial nozzle state (x,y,z,u,v,w,et...) data and sets equal to initial state
            xv[1, n] = xs[2, n] * sts + xs[1, n] * cts
            xv[2, n] = xs[2, n] * cts - xs[1, n] * sts
        self.xv = xv # this xv, or rund.xv will iterate and hold the results for each time step, droplet and nozzle location
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

    def setDepAndFluxFlags(self, dmass, swath, cts, afrac, nvar, nswth):
        self.temnd = dmass * swath * cts * cts / afrac / nvar / 5.01326
        self.idepv = np.ones([nvar])
        self.cndep = np.zeros([nvar])
        self.csdep = np.ones([nvar])
        self.temnf = dmass * swath * cts * cts / afrac / nvar / 5.01326
        self.iflxv = np.ones([nvar,nswth])
        self.cnflx = np.zeros([nvar,nswth])
        self.csflx = np.ones([nvar,nswth])

    def initializePlanes(self, control):

        ymin = self.ydepn
        ymax = control.ydepx
        nvec = int(control.ndepr) # Initial assignment results in odd number, must be int to size array
        dy = (ymax-ymin) / (nvec-1)
        self.ydepr = np.zeros([nvec])
        self.zdepr = np.zeros([nvec])
        for n in range(nvec):
            self.ydepr[n] = ymin + (n * dy)
        self.ddepr = dy
        ymax = ymax + (control.nswth+2.5) * control.swath
        nvec = int((ymax - ymin) / dy + 2)
        self.ndeps = nvec
        self.ydeps = np.zeros([nvec])
        self.zdeps = np.zeros([nvec])
        self.zdept = np.zeros([nvec])
        self.zdeph = np.zeros([nvec])
        self.zdepi = np.zeros([nvec])
        self.zdepn = np.zeros([nvec])
        for n in range(nvec):
            self.ydeps[n] = ymin + (n * dy)
        self.ndeps2 = (control.ydepx2 + (control.nswth + 2.5)*control.swath - self.ydepn) / dy + 2 # Should this be set to int?
        # The following 4 should likely be placed elsewhere in this run data object
        self.ygaus1 = control.ygaus1
        self.ydrft = control.ydrft
        self.efrac = control.efrac

    def setOldTime(self,told):
        self.told = told

    def initialPositions(self, nswth, iboom, nvar, sdisp):
        self.iswc = nvar
        self.sfac = sdisp + 0.5 * (1.0+iboom)
        self.nswtm = nswth
        if iboom == 1:
           self.nswtm = self.nswtm + 1
        self.iydv = np.zeros([nvar, self.nswtm])
        self.iyfv = np.zeros([nvar, self.nswtm])
        self.ivtt = nvar # Not sure what this does
        self.mswc = None

    def initializeIntegration(self, nprp):
        self.dt = 0.0
        self.t = 0.0
        self.dmin = self.diam
        self.istt = 0 # Trajectory flag I think
        if nprp != 0:
            for n in range(nprp):
                self.cpxi[n] = self.cpxi[n] + self.xo-self.xprp[n]

        # Another if then here to update things for if a heli.

    def setTermVandDt(self):
        self.nstep = self.nstep + 1
        self.uterm = 9.58 * (1.0-math.exp(-1.0*(self.dmin/1700.0))**1.147)
        self.dt = 0.0002*self.dmin/self.uterm













