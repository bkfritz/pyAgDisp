from dataclasses import dataclass, field
import numpy as np
import math

@dataclass
class totalAccountArrays:
    '''
    Sets and holds the total accountancy arrays.
    '''
    tattv = np.zeros([200])
    tatfv = np.zeros([3, 200])
    tadfv = np.zeros([3, 200])
    tahfv = np.zeros([3, 200])
    tahhv = np.zeros([200])
    taddv = np.zeros([200])
    tadfv = np.zeros([3,200])

    def initiate_time_arrays(self, tmax):
        # Set pointers a short cuts
        tv = self.tattv
        tv[0] = 0
        tv[1] = 0.1
        natt = 1 # NATT starts at 2 in FORTRAN, starts at 1 here to reflect Python starting indexing from 0, not 1
        nn = natt
        dt = 0.1
        ft = 1.05

        while nn < 200 and (tv[nn]+dt) < tmax:
            dt = dt*ft
            nn = nn+1
            tv[nn] = tv[nn-1] + dt

        self.tattv = tv
        self.natt = nn # Do I need to +1 here??
        self.inmax = 2

    def initiate_distance_arrays(self, ygrid2):
        # Set pointers a short cuts
        td = self.taddv
        for n in range(27):
            td[n] = 2*(n-25)
        nadd = 26 # NATT starts at 2 in FORTRAN, starts at 1 here to reflect Python starting indexing from 0, not 1
        nn = nadd
        dd = 2.0
        tt = 1.05

        while dd < 200 and td[nn] < ygrid2:
            nn = nn + 1
            dd = dd * tt
            td[nn] = td[nn-1] + dd
        self.nadd = nn
        self.taddv = td

    def initiate_height_arrays(self, zref, boomht, s):
        # Requires sending in zref, boom height, and s (semispan)
        dahh = 0.005
        dahh = 2.0*dahh
        nahh = (boomht - zref + 0.5*s)/ dahh + 1
        while nahh > 200:
            dahh = 2.0*dahh
            nahh = (boomht - zref + 0.5 * s) / dahh + 1
        self.nahh = max(int(nahh),2)
        self.dahh = dahh
        for n in range(self.nahh):
            self.tahhv[n] = n*dahh + zref

