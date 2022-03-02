from dataclasses import dataclass, field
import numpy as np
import math

@dataclass
class horizontalDepPlanes:
    '''
    Initiates the horizontal deposition planes.
    '''
    ydepn: float = field(default=-60, metadata={'unit':'m'}) # Set by Agsome.for line 491

    def initializePlanes(self, ydepx, ndepr, nswth, swath, ydepx2):
        ymin = self.ydepn
        ymax = ydepx
        nvec = int(ndepr) # Initial assignment results in odd number, must be int to size array
        dy = (ymax-ymin) / (nvec-1)
        self.ydepr = np.zeros([nvec])
        self.zdepr = np.zeros([nvec])
        for n in range(nvec):
            self.ydepr[n] = ymin + (n * dy)
        ddepr = dy
        ymax = ymax + (nswth+2.5) * swath
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
        self.ndeps2 = (ydepx2 + (nswth + 2.5)*swath - self.ydepn) / dy + 2 # Should this be set to int?


