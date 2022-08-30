from dataclasses import dataclass, field
import numpy as np
import math

@dataclass
class sprayMaterialData:
    '''
    Holds the various spray material data parameters that are used.
    '''
    lflow: int = field(default=1) # FLOWUNIT (0=L/ha, 1=L/min) + 1, so lflow=1 = L/ha
    denf: float = field(default=1.0) # Specific gravity of the tank mix (Carrier, default in AgDisp)
    denn: float = field(default=1.0) # Specific gravity of the nonvolatile fraction (Active + Additive in AgDisp)

    basictype: int = field(default=1) # Oil or Water, or No-Evap or Evap, 0=oil 1-water
    erate: float = field(default=84.76, metadata={'unit':'um^2/deg C/sec'}) # Default Agdisp Evaporate Rate
    levap: int = field(default=0) # Evaporation flag 1=yes, 0=no.

    # The volatile and nonvolatile fraction stuff that follows will need updating to mimic the Agdisp Spray Material UI
    # As they do not have the % Tank mix stuff calculations in place and assume 100% of Active and Additive are Nonvol.
    nvfrac: float = field(default=0.03) # Nonvolatile Fraction; Default in Agdisp
    acfrac: float = field(default=0.015) # Non-vol Active Fraction; Default

    flowrate: float = field(default=18.71, metadata={'unit':'L/ha'}) # Spray Volume Rate

    def calcFlows(self, uo, swath):
        ''' Converts L/ha to/from L/min'''
        if self.lflow == 1:
            self.flow = 0.001585 * uo * self.flowrate * swath
            self.flown = self.flowrate
        else:
            self.flow = 0.2642 * self.flowrate
            self.flown = self.flowratef/ 0.006 / uo / swath

    def setEvapParams(self):
        self.vfrac = 1.0 - self.nvfrac
        self.afrac = self.acfrac
        if self.basictype == 0:
            self.erate = 0.0
            self.levap = 0
        else:
            self.erate = self.erate
            self.levap = 1

