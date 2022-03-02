from dataclasses import dataclass, field
import numpy as np
import math

@dataclass
class nozzleData:
    '''
    Holds the various nozzle data parameters that are used.
    '''
    nvar: int = field(default=1) # Number of nozzles on the boom, max 60 Version 8.29

    # Adding the following parameter ihalf[60] here for now until can if should be elsewhere
    ihalf = np.zeros([60]) # Half boom flag, 1=negative horizontal, 0=positive horizontal position

    def changeNozzleNumber(self, numnoz):
        self.nvar = numnoz

    def setNozLocArrays(self):
        # For now, the following parameters will be set assuming a single nozzle, centered 0,0,0
        # Can build the spacing and placing functions later
        self.xnoz = np.zeros([self.nvar])  # Forward positions of nozzles
        #self.ynoz = np.zeros([self.nvar])  # Horizontal positions of nozzles
        self.ynoz = ([5])
        self.znoz = np.zeros([self.nvar])  # vertical positions of nozzles
        self.unoz = np.zeros([self.nvar])  # Initial u vel
        self.vnoz = np.zeros([self.nvar])  # Initial v vel


    def findExtremePositions(self, acraft):
        xosmn = 1.0e10
        xosmx = -1.0e10
        zosmn = 1.0e10
        zosmx = -1.0e10

        self.xosmn = min(xosmn, np.min(self.xnoz))
        self.xosmx = max(xosmx, np.max(self.xnoz))
        self.zosmn = min(zosmn, np.min(self.znoz))
        self.zosmx = max(zosmx, np.max(self.znoz))

        self.xos = -acraft.boomfd - self.xosmx # line 243 Aginit.for

        if acraft.lmvel == 1:
            self.zosmn = min(self.zosmn, acraft.dzprp-acraft.boomvt-acraft.rprps)

        self.hosmn = acraft.dist # line 245 Aginit.for

    def setInitialDropsPositsVels(self, acraft, terr, boomht):
        xs = np.zeros([9,self.nvar])
        # Set initial positions, velocities for nozzle positions
        for n in range(self.nvar):
            xs[0,n] = -acraft.boomfd - self.xnoz[n]
            xs[1,n] = self.ynoz[n]
            xs[2,n] = boomht + self.znoz[n]
            self.hosmn = min(self.hosmn, xs[2,n]*terr.cts - xs[1,n]*terr.sts)
            xs[3,n] = -acraft.uo + self.unoz[n]
            xs[4,n] = self.vnoz[n]
            for k in range(5,9):
                xs[k,n] = 0.0
            # lines 260-262 Aginit.for set ground nozzle stuff, add later
            if xs[1,n] < 0:
                self.ihalf[n] = 1
            else:
                self.ihalf[n] = 0
        self.xs = xs



