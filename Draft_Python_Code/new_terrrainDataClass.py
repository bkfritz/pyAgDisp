from dataclasses import dataclass, field
import numpy as np
import math

@dataclass
class terrainData:
    '''
    Holds the various terrain data parameters.
    '''
    ctu: float = field(default=1.0)  # Cosine( upslopeAngle * tpi/360 )
    stu: float = field(default=0.0) # Sine( upslopeAngle * tpi/360 )
    cts: float = field(default=1.0)  # Cosine( sideslopeAngle * tpi/360 )
    sts: float = field(default=0.0)  # Sine( sideslopeAngle * tpi/360 )
    angtu: float = field(default=0.0, metadata={'unit':'deg'}) # Upslope angle
    angts: float = field(default=0.0, metadata={'unit': 'deg'}) # Sideslope angle
    zref: float = field(default=0.0, metadata={'unit': 'm'})  # Ground reference. set in Agdisp Advance settings
    tpi: float = field(default=6.2831853) # 2 * pi()

    def changeUpslope(self, angtu):
        self.angtu = angtu

    def changeSideslope(self, angts):
        self.angts = angts

    def initilizeTerrain(self):
        '''
        This function is excecuted during the steps of Aginit.for

        '''
        tpi = self.tpi

        if self.angtu != 0:
            self.ctu = math.cos(self.angtu * tpi/360.0)
            self.stu = math.cos(self.angtu * tpi/360.0)

        if self.angts != 0:
            self.cts = math.cos(self.angts * tpi/360.0)
            self.sts = math.cos(self.angts * tpi/360.0)