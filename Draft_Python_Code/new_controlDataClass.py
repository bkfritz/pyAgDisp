from dataclasses import dataclass, field
import numpy as np
import math

@dataclass
class controlData:
    '''
    Holds the various control data parameters that are used to run and control model routines.

    The parameters held here should be ones that control the overriding controls of the program,
    i.e. application type, number of swath, etc...

    This will also include things like the advance settings, displacement, etc....

    These should not chance once the actual solution calculations initiate.
    '''
    applmeth: int = field(default=0)  # Application Method from Agstruc.inc 0=aerial, 1=ground
    airtype: int = field(default=0) # Aerial application type: 0=liquid, 1=dry
    vaporflag: int = field(default = 0) # Vapor Tracking flag: 0=not invoked, 1=invoked
    calpuffflag: int = field(default=0) # CALPUFF flag: 0=not invoked, 1=invoked
    cscipuffflag: int = field(default=0)  # SCIPUFF flag: 0=not invoked, 1=invoked
    scipuffcutoff: float = field(default=0.0) # SCIPUFF cutoff factor

    boomht: float = field(default=2.0, metadata={'unit':'m'}) # User specified boom height
    numlines: float = field(default = 20) # Number of spray lines, typically 20
    lineopt: int = field(default=0) # Flight line optimization flag: 0=off, 1=on
    swtype: int = field(default=0) # Swath width type: 0=fixed value, 1 = 1.2 x wingspan, 2 = swath width x wingspan
    swathwid: float = field(default=20.0, metadata={'unit':'m'}) # Swath width, fixed value
    sdtype: int = field(default=0) # Swath displacement type: 0=fraction of swath width
                # 1=fraction of application rate, 3=fixed value, 4=aircraft centerline
    sdvalue: float = field(default=0.0, metadata={'unit':'m'}) # Fraction or fixed value
    flxplane: float = field(default=0.0, metadata={'unit':'m'}) # Flux plane i.e. Transport distance, Agdisp default = 0
    hgtrnmn: float = field(default=0.0, metadata={'unit':'m'}) # Minimum transport height
    hgtrnmax: float = field(default=200.0, metadata={'unit':'m'}) # Maximum transport height
    maxtime: float = field(default=600.0, metadata={'unit':'sec'}) # Maximum computation time  NEEDS AGCHK
    maxdwnd: float = field(default=795.0, metadata={'unit':'m'}) # Maximum downwind distance
    halfboom: int = field(default=0) # Half boom effects: 0=no, 1=yes
    savetraj: int = field(default=0) # Save trajectory results: 0=off, 1=on
    swathoff: int = field(default=1) # Swath offset: 0=1/2 swath, 1=0 swath.  Default in Agdisp is 0, set to 1 here for testing

    def getLineReps(self):
        self.linereps = np.ones([self.numlines])  # Number of replicates for each flight line.  Will need to make editable

    def changeBoomHeight(self, boomht):
        self.boomht = boomht

    def changeNumSwaths(self, numlines):
        '''
        Would be done as part of UI and prior to initializeControl function
        '''
        self.numlines = numlines

    def changeSwathWidth(self, swathwidht):
        self.swath = swathwidht

    def initializeControl(self, lmvel, s, zref):
        '''
        For stuff "done" in Aginit.for

        Many of the self."varname" on the right of the = below are the names used in the Basic UI and taken in
        using the Agread.for subroutine.

        '''

        self.ldry = self.airtype
        self.swath = self.swathwid # line 274 Aginit.for
        self.nswth = self.numlines
        self.iboom = self.halfboom
        self.sdisp = -self.sdvalue / max(self.swath, 1) + 0.5 * (self.swathoff - 1) # line 311 Aginit
        self.yflxv = self.flxplane
        self.swdisp = -self.sdisp * self.swath # Line 320 Aginit
        self.lfopt = self.lineopt
        self.nfrep = self.linereps
        # line 329 Aginit sets self.nfrep[self.nswth + 1] = 1, why?  Maybe can only have 1 rep on last swath pass.
        self.tmax = self.maxtime
        self.idsb = self.savetraj
        self.lcpflg = self.calpuffflag
        self.lspflg = self.cscipuffflag
        self.fspflg = self.scipuffcutoff
        # Setting parameters for deposition and sampling arrays
        # Will leave these in Control object for now, may be a better place
        # The following down to ngrid lines 354 - 361 aginit
        self.ndepr2 = 0.5 * (self.maxdwnd + 155.0) + 1
        self.ydepx2 = 2.0 * (self.ndepr2-31)
        self.ygrid2 = self.maxdwnd
        self.ngrid2 = self.ndepr2-46
        self.ndepr = 0.5 * (4000.0+155.0)+1 # This gives an odd number, yet is used to size array. Must be int
        self.ydepx = 2.0 * (self.ndepr-31)
        self.ygrid = 4000.0
        self.ngrid = self.ndepr-46
        # The following are read/set in Agsome.for starting at line 514: Transport aloft cards: distance
        self.nflxr = 200 # line 536 Agsome
        dyflxv = (0.5 * (1 - self.iboom) - self.sdisp) * self.swath
        self.yflxv = self.yflxv + dyflxv

        if lmvel == 0: # Ground application
            temh = 2.0*self.boomht
        else:
            temh = self.boomht + 5.0 * s
        self.zflxn = zref
        self.zflxx = temh
        self.lvtflg = 0 # This is done Aglims.for line 248
        # The following are done lines 511 and 512 Agsome.for
        self.grdmx = self.ydepx + (self.nswth - 0.5)*self.swath
        self.grdmx2 = self.ydepx2 +(self.nswth - 0.5)*self.swath

    def setInitialValues(self, cts):
        '''
        Sets initial values.
        lines 114-123 Aglims.for
        '''
        self.swath = self.swath / cts
        self.sfac = self.sdisp + 0.5*(1+self.iboom)
        self.yedge2 = self.ygrid2 + (1.0-self.sfac) * self.swath
        self.ydrft = 0.0
        self.efrac = 0.0
        self.aleft = 0.0
        self.ygaus1 = 0.0










