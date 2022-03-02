from dataclasses import dataclass, field
import numpy as np
import math
from probabilityFunctions import fp

@dataclass
class dropletData:
    '''
    Holds the various droplet data parameters.
    '''
    ndrp: int = field(default=32) # From user input, Number of drop categories NUMDROP
    npts: float = ndrp
    lflag: int = field(default=1) # 0=distribution know, 1,2: VMD and RS known, if 2, distribution will be optimized(?)
    vmd: float = field(default=350.0, metadata={'unit':'um'}) # VMD
    xrs: float = field(default=1.2) # RS
    icls: int = field(default=-1) #Size class flag: -1 = no, 0 - 10XX = class to use.This would be just picking a class in AGDISP

    def distributionSource(self, lflag):
        '''
        User input to define source of droplet size distribution.
        For now, will only look at providing VMD and RS and recovering from that.

        Later, will look at adding the droplet size libraries and nozzle models.

        lflag: 0=distribution know, 1,2: VMD and RS known, if 2, distribution will be optimized(?)
        '''
        self.lflag = lflag

    def setVMD_RS(self, vmd, rs):
        '''
        Sets vmd and rs that will be used to recover DSD using the routine established by Agparm.for
        vmd = Volume Median Diameter (um)
        rs = Relative Span
        '''
        self.vmd = vmd
        self.xrs = rs

    def constructProfile(self):
        '''
        Constructs a DS profile using the set vmd and rs.

        This will run if lflag = 1
        '''
        ddv = np.array([
            [10.77, 16.73, 19.39, 22.49, 26.05, 30.21, 35.01,
             40.57, 47.03, 54.50, 63.16, 73.23, 84.85, 98.12,
             113.71, 131.73, 152.79, 177.84, 205.84, 238.45, 276.48,
             320.60, 372.18, 430.74, 498.91, 578.54, 670.72, 777.39,
             900.61, 1044.42, 1210.66, 1403.04],
            [13.92, 20.84, 24.20, 28.15, 32.55, 37.72, 43.73,
             50.64, 58.76, 68.12, 78.99, 91.62, 106.30, 123.22,
             142.76, 165.29, 191.34, 221.91, 256.94, 298.07, 345.60,
             400.75, 464.83, 538.47, 623.61, 722.82, 838.00, 971.78,
             1126.51, 1305.88, 1513.71, 1754.16],
            [13.61, 20.07, 24.06, 28.05, 33.09, 40.13, 48.11,
             57.15, 68.18, 80.15, 93.18, 110.30, 135.55, 165.45,
             195.38, 230.58, 275.76, 330.91, 390.77, 461.16, 551.51,
             661.81, 792.06, 942.26, 1122.97, 1343.57, 1604.07, 1904.48,
             2265.89, 2707.09, 3228.10, 3828.91]
        ])

        npts = 32
        # Set up empty array to fill
        dkv = np.zeros([npts])
        xkv = np.zeros([npts])
        cc = self.xrs / 5.126915
        psave = 0.0
        i = 0
        xp = (math.sqrt(ddv[0, npts - 1] / self.vmd) - 1) / cc
        pp = fp(xp)
        if pp < 0.99:
            i = 1
            xp = (math.sqrt(ddv[1, npts - 1] / self.vmd) - 1) / cc
            pp = fp(xp)
            if pp < 0.999:
                i = 2

        for n in range(npts):
            dkv[n] = ddv[i, n]
            xp = (math.sqrt(dkv[n] / self.vmd) - 1) / cc
            prob = fp(xp)
            xkv[n] = prob - psave
            psave = prob
        if psave < 1:
            xkv = xkv / psave

        if npts > 1:
            ipts = npts
            csum = xkv[0]
            for n in range(1, npts):
                csum = csum + xkv[n]
                if csum > 0.999:
                    ipts = min(ipts, n)
            self.npts = ipts

        # Compute cumulative volume fractions
        if self.npts > 1:
            tkv = np.zeros([npts])
            ckv = np.zeros([npts])
            tkv[0] = dkv[0] ** 3
            ckv[0] = min(1.0, max(0.0, xkv[0]))
            for n in range(1, npts):
                tkv[n] = dkv[n] ** 3
                ckv[n] = min(1.0, max(0.0, ckv[n - 1] + xkv[n]))

            self.dkv = tkv ** 0.33333 # Removes cube of D
            # The following np.diff may need to be revisted to ensure it conforms to what the
            # Fortran code is doing.
            self.xkv = np.diff(ckv, prepend=ckv[0]) # Converts from cumulative to incremental

    def computeDSDnumerics(self):
        # Compute drop size distribution numerics
        xnd10 = np.interp(0.1, self.xkv, self.dkv) ** 0.33333
        if lflag == 0:
            vmd = np.interp(0.5, self.xkv, self.dkv) ** 0.33333
        xnd90 = np.interp(0.9, self.xkv, self.dkv) ** 0.33333
        if lflag == 0:
            xrs = (xnd90 - xnd10) / vmd
        xf141 = np.interp(141 ** 3, self.dkv, self.xkv)

        self.vmd = vmd
        self.xrs = xrs
        self.dv10 = xnd10
        self.dv90 = xnd90
        self.f141 = xf141

    def initializeDrops(self):
        self.diamv = self.dkv
        self.dmass = self.xkv
        self.ndrp = self.npts

    def transferDropSizeDistribution(self):
        '''
        The following is executed in Aglims.for lines 77-94
        I think dmax and nndrpt need to be set to self.vars
        '''
        self.npts = len(self.diamv)
        self.ddiamn = self.diamv
        self.dmassn = self.dmass
        self.dmax = np.max(self.ddiamn)
        self.nndrpt = self.npts

    def initializeComputedDSdistribution(self):
        '''
        The following is lines 96-112 in Aglims.for
        It uses a goto loop to create the drop size data array used later in determining
        the various droplet size distributions downwind, vertical, etc...
        It uses the set dsdv data below and starts an iteration that progressively
        adds droplet diameters to the various arrays until dmax is reached.
        When dmax is > the 1403.04 max in dsdv, the iterations continues by
        setting the next diameter = 1.159 * previous diameter.
        To port to python, the process was changed somewhat but has the same effect.
        '''
        dsdv = [8.00, 9.27, 10.75, 12.45, 14.43, 16.73, 19.39, 22.49, 26.05, 30.21, 35.01,
                40.57, 47.03, 54.50, 63.16, 73.23, 84.85, 98.12, 113.71, 131.73, 152.79,
                177.84, 205.84, 238.45, 276.48, 320.60, 372.18, 430.74, 498.91, 578.54,
                670.72, 777.39, 900.61, 1044.42, 1210.66, 1403.04]
        dtest = min(self.dmax, max(dsdv))

        if dtest >= self.dmax:
            new = [x for x in dsdv if x <= self.dmax]
        else:
            while dtest < self.dmax:
                dsdv.append(dtest)
                dnew = 1.159 * dtest
                dtest = dnew
            new = np.array(dsdv)
        self.dsdc = new
        self.dssb = np.zeros([len(self.dsdc)])
        self.dsdw = np.zeros([len(self.dsdc)])
        self.dsvp = np.zeros([len(self.dsdc)])
        self.dscp = np.zeros([len(self.dsdc)])
        self.dsdp = np.zeros([len(self.dsdc)])










