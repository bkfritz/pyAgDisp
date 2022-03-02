from dataclasses import dataclass, field
import numpy as np
import math

@dataclass
class verticalFluxData:
    '''
    Initializes and holds the vertical flux data.
    '''

    def verticalFluxPlane(self, zflxn, zflxx, nflxr):
        ymin = zflxn
        ymax = zflxx
        nvec = nflxr
        dy = (ymax - ymin) / nvec
        self.yflxr = np.zeros([int(nvec)])
        self.zflxr = np.zeros([int(nvec)])
        self.zflxt = np.zeros([int(nvec)])
        self.zflxd = np.zeros([int(nvec)])
        for n in range(int(nvec)):
            self.yflxr[n] = ymin +(n+1)*dy
        self.dflxr = dy