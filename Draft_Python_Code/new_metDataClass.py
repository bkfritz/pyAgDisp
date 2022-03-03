from dataclasses import dataclass, field
import numpy as np
import math

@dataclass
class metData:
    '''
    Holds the various control data parameters that are used to run and control model routines.
    '''

    kstv = [
        [1, 1, 2, 3, 3],
        [1, 2, 2, 3, 4],
        [2, 3, 3, 4, 4],
        [3, 4, 4, 4, 4],
        [4, 4, 4, 4, 4],
        [6, 5, 4, 4, 4],
        [6, 6, 5, 4, 4]
    ]
    pstv = [0.524, 0.373, 0.211, 0.0, -0.533, -3.175]
    qstv = [2.207, 1.693, 1.309, 1.0, 0.734, 0.500]
    bstv = [1.911, 1.393, 1.161, 1.0, 0.893, 0.786]
    tpi: float = 6.2831853

    qqmx: float = field(default=1.0)  #

    windtype: int = field(default=0) # Wind input type: 0=single height, 1 = wind table
    windspd: float = field(default=2.24, metadata={'unit':'m/s'}) # Default starting value Agdisp
    winddir: float = field(default=-90, metadata={'unit':'deg'}) # Wind direction
    windhgt: float = field(default=2.0, metadata={'unit':'m'}) # Wind measurement height, 2 is agdisp default
    surfruff: float = field(default=0.04, metadata={'unit':'m'}) # Middle-ish of crops in Agdisp
    temp: float = field(default=18.33 ,metadata={'unit':'degC'}) # Default starting value Agdisp
    humidity: float = field(default=50, metadata={'unit':'%'}) # Default starting value Agdisp
    insol: int = field(default=4) # Insolation Index: 0-6, Strong, slight etc.  4 default, night and overcast
    prtr: float = field(default=1013.0, metadata={'unit':'mb'}) # Ambient pressure, set in Agdisp Advance settings

    # Other default advanced settings:
    # The 2* below on gdko and gdk is done in Aginit.for lines 341-342
    gdko: float = field(default=2*0.15, metadata={'unit': 'm/s'})  # Vortext decay rate OGE  NEEDS AGCHK
    gdk: float = field(default=2*0.56, metadata={'unit': 'm/s'})  # Vortext decay rate IGE   NEEDS AGCHK

    def changeMet(self):
        print('Add this function later')

    def initializeMet(self, lfmaa):
        self.windsp = self.windspd
        self.zo = self.surfruff
        self.temptr = self.temp
        self.rhumtr = self.humidity

        item = min(int((self.windsp + 1.0) / 2.0) + 1, 5)
        self.kstab = self.insol + 1
        # The -1 in the following four params reflects conversion fortran to python
        # Maybe should update item, kstab and kstv?
        self.lstab = self.kstv[item-1][self.kstab-1]
        self.pstab = self.pstv[self.lstab-1]
        self.qstab = self.qstv[self.lstab-1]
        self.bstab = self.bstv[self.lstab-1]

        if lfmaa == 1:
            self.winddr = self.winddir
        else:
            self.winddr = -90
        self.windht = self.windhgt

        self.lmcrs = self.windtype + 1

        # The following cals are done in Agread starting line 325
        # self.usk = self.windsp / math.log((self.windht + self.zo) / self.zo) Fortran calcs usk twice for some reason???
        self.usk = self.windsp / math.log(((self.windht + self.zo) / self.zo) - self.pstab) # This is the second usk calc
        self.qqmx = 0.845 * self.qstab * self.usk ** 2
        tem = self.winddr * self.tpi / 360
        self.ccw = self.usk * math.cos(tem)
        self.scw = self.usk * math.sin(tem)
        self.chz = self.zo





