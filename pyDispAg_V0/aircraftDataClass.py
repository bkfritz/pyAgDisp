from dataclasses import dataclass, field
import numpy as np
import math

@dataclass
class aircraftData():
    '''
    Class to hold aircraft data and other related parameters.

    Does this just need to be called "sprayer" do reflect potential ground sprayer data?

    This current version will following the fortran types, defaults and naming exactly.
    Much of this information comes from AgCommon.inc

    For the aircraft type dictionary, the array of variables, in order, are:
    Fixed Wing (3) or Heli (4), Semispan (m), Typ.Speed (m/s), Biplane Sep. (m), Weight (kg),
    Platform Area (m^2), Propeller RPM, Prop Radius (m), Engine Vert (m),
        Engine Fwd (m), Engines (#), Engine Hor (m), Engine Hor (m), Wing Vert (m), Boom Vert (m),
         Boom Fwd (m), Last Var (1,2 or 3)???
    List of variable names, as set in Agstruc.inc, of aircraft library content.
    type = 0=basic, 1=user-defined, 2=library
    name = Aircraft name from library
    wingtype = Aircraft wing type: 3=fixed, 4=helicopter
    semispan = Semispan or Rotor radius (m)
    typspeed = Typical spraying speed (m/s)
    biplsep = Biplane distance between wings (m)
    weight = Weight (kg)
    planarea = Platform area (m^2)
    proprpm = Propeller or rotor RPM
    proprad = Propeller radius (m)
    engvert = Engine vertical position (m)
    engfwd = Engine forward position (m)
    numeng = Number of engines
    enghoriz = Engine horizontal positions (m)
    wingvert = Distance vertical from trailing edge to wingtip
    boomvert = Distance vertical from trailing edge to boom
    boomfwd = Distance forward from trailing edge to boom
    lastitem = 1, 2, or 3.  Not sure what this is.
    '''

    actype = {
        "Air Tractor AT-401": [3, 7.4677, 53.640, 0, 2720.4715, 27.3143, 2000,
                               1.3716, -0.3658, 3.6272, 1, [0, 0], 0.4603, -0.3505, -0.2540, 1],

        "Air Tractor AT-602": [3, 8.5345, 64.822, 0, 4104.3084, 27.7830, 1500,
                               1.4324, 0, 4.5358, 1, [0, 0], 0.4775, -0.2012, -0.1524, 2]
              } # This is from the aircraft library data.  Needs to get remaining data into dictionary.

    #The following are parameters set from values taken in from the library.
    #The parameter names in the *.for and the *.inc files differ, the *.for names are used here.

    type: int = field(default =2) # 0=basic, 1=user, 2 = library
    name: str = '' #Aircraft name from library, should be identical to allow dict call
    iactyp: int = field(default=1) # Wingtype: 1 = fixed, 2 = Heli.
    s: float = field(default=0.0, metadata={'unit': 'm'}) # Semispan, AGCHK
    uo: float = field(default=0.0, metadata={'unit': 'm/s'}) # Typical spraying speed
    dzbp: float = field(default=0.0, metadata={'unit': 'm'}) # Biplane separation
    wt: float = field(default=0.0, metadata={'unit': 'kg'}) # Weight
    area: float = field(default=0.0, metadata={'unit':'m^2'}) # Platform area, this is declared as in Fortran, area=as
    tdot: float = field(default=0.0) # Prop/Rotor RPM
    rprps: float = field(default=0.0, metadata={'unit':'m'}) # Prop radius
    dzprp: float = field(default=0.0, metadata={'unit':'m'}) # Engine vertical position
    xprps: float = field(default=0.0, metadata={'unit':'m'}) # Engine forward position
    nprp: int = field(default = 0) # Number of engines/props
    yprps = np.zeros([4]) # Array of Engine horizontal positions
    wingvt: float = field(default=0.0, metadata={'unit':'m'}) # Wing vertical
    boomvt: float = field(default=0.0, metadata={'unit':'m'}) # Boom vertical
    boomfd: float = field(default=0.0, metadata={'unit':'m'}) # Boom forward

    # The following are initialized here and updated with actual values later.
    nvor: int = field(default = 0) # Number of vortices??

    psbp: float = field(default=0.0) # 0 if fixed wing, 1 if bi-wing
    pgbp: float = field(default=0.0) # 0 if fixed wing, 1 if bi-wing
    dzbp: float = field(default=0.0, metadata={'unit':'m'}) # biplane separation
    jhel: float = field(default=0.0) # Not sure what this is yet
    sdisp: float = field(default=0.0, metadata={'unit':'m'}) # Swath displacement

    # The following are set in AgDisp's Advanced settings interface, should NOT be changed.
    # Should these be in the Control object?
    drag: float = field(default = 0.1) # Aircraft drag coefficient, set in Agdisp Advance settings
    prop: float = field(default = 0.8) # Aircraft prop efficiency, set in Agdisp Advance settings

    tpi: float = 6.2831853  # 2 * pi

    # Initialize arrays
    g2pis = np.zeros([2])
    ybars = np.zeros([2])
    zbars = np.zeros([2])
    ybals = np.zeros([2])
    zbals = np.zeros([2])

    def userData(self, name, boomht, lfmaa):
        '''
        userData is intended to replicate the functionality of Agread.for
        Take in user input data and check limits, units, etc...
        '''
        self.name = name
        self.iactyp = self.actype[self.name][0]-2 # Aircraft type
        self.lmvel = self.iactyp # 1 for aerial, 0 for ground, 2 for heli
        self.s = self.actype[self.name][1] # semispan for aircraft, swath width for ground sprayer
        self.uo = self.actype[self.name][2] # typical speed
        self.dzbp = self.actype[self.name][3] #biplane separation
        if self.dzbp != 0:
            # Both of the following are zero unless a biplane, then they are 1
            self.pgbp: int = 1
            self.psbp: int = 1
        self.wt = 9.81*self.actype[self.name][4] # weight in N
        self.area = self.actype[self.name][5] # Platform area - This is as in FORTRAN code, cant use in python
        self.tdot = self.actype[self.name][6]  # Prop RPM
        self.rprps = self.actype[self.name][7]  # Prop Radius
        self.dzprp = self.actype[self.name][8]  # Engine Vertical
        self.xprps = -1*self.actype[self.name][9]  # Engine forward
        self.nprp = self.actype[self.name][10]  # Number of engines
        if self.nprp == 1:
            self.yprps[0] = self.actype[self.name][11][0]  # Engine horizontal
        elif self.nprp == 2:
            self.yprps[0] = self.actype[self.name][11][0]  # Engine horizontal
            self.yprps[1] = -1*self.actype[self.name][11][0]  # Engine horizontal
        else:
            self.yprps[0] = self.actype[self.name][11][0]  # Engine horizontal
            self.yprps[1] = -1 * self.actype[self.name][11][0]  # Engine horizontal
            self.yprps[2] = self.actype[self.name][11][1]  # Engine horizontal
            self.yprps[3] = -1 * self.actype[self.name][11][1]  # Engine horizontal
        self.wingvt = self.actype[self.name][12]  # wing vertical
        self.boomvt = self.actype[self.name][13] # boom vertical
        self.boomfd = self.actype[self.name][14]  # boom forward
        self.dist = boomht - self.boomvt + self.wingvt
        # The following is from Agread.for 250-282 Wing loading,
        if (self.iactyp == 1 & lfmaa == 1):
            self.nvor = 1
            self.g2pis[0] = self.wt / (1 + self.pgbp) / self.s / self.uo / self.tpi / 1.9267
            self.ybars[0] = 0.7854 * self.s
            self.zbars[0] = self.dist
            self.ybals[0] = -0.7854 * self.s
            self.zbals[0] = self.dist
            self.rlim = 0.1 * self.s
            # If biplane, more settings here updated

        # The following if from Agread.for 543-549 Propeller Card
        self.aprp = 0.5 * self.tpi * self.rprps**2 # Prop disc area
        self.ui = 0.5 * self.uo*(-1.0 + math.sqrt(1.0 + self.drag *self.area/self.aprp)) # incremental velocity at disc
        self.vprps = 60.0 * self.drag * self.area * self.uo**3 / (self.tpi * self.prop * self.tdot *
                                                              self.aprp * self.rprps * (self.uo + self.ui)) # Angular vel. prop
        self.cpxis = 11.785 * self.rprps
        self.cpur = self.ui * self.rprps
        self.zprps = boomht + self.dzprp - self.boomvt






