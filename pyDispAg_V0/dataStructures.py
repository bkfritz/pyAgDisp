'''
This code duplicates the Agdstruc.inc code, which defines the data structures used
throughout the remaining code.

Overall code development is following basAGDISP.bas code, which is the main code
module for AgDISP.

These data structures are passed from the basic code to the fortran code.
'''

from dataclasses import dataclass, field
import numpy as np
import math

# Assign global constants for sizing data arrays
max_drops = 100 # Maximum number of droplet categories
max_nozzles = 60 # maximum number of nozzles
max_engines = 2 # maximum number of engines
max_fltlines = 50 # maximum number of spray lines
max_envelope = 25 # maximum canopy envelope levels
max_lai = 10 # maximum canopy LAI levels
max_calcdata = 4900 # Maximum size of calculation arrays
max_asae = 6 # maximum ASAE drop distributions NEEDS TO BE 7
max_winds = 10 # maximum number of heights/windspeeds

@dataclass
class groundApplicationData:
    numswaths: int = field(default = 0) # number of swaths for ground sprayer - numswath is used in agdstruc
    noztype: int = field(default = 0) # Ground nozzle type: 0 = flat fan, 1 = air injection
    pressure: float = field(default = 0, metadata={'unit':'bar'}) # boom pressure

@dataclass
class dropSizeDistributionData:
    type: int = field(default = 0) # 0=basic, 1=(not used), 2=user defined, no 4, 5=ARS, 6=FS
    basictype: int = field(default = 0) # basic DSD category: 0-17
    name: str = field(default = '') # name for DSD
    numdrop: int = field(default = 0) # number of drop categories
    diam: list = field(default_factory=list, metadata={'units': 'um'}) # drop diameters
    massfrac: list = field(default_factory=list) # drop volume fractions

@dataclass
class usdaArsNozzleModelsData:
    maxerrorlevel: int = field(default = 0) # max error level during run 0=none, 1=warn,, 2=err,cont, 3=err,stop
    namenoz: str = field(default = '') # nozzle name
    nametype: str = field(default = '') # nozzle body angle
    orifice: float = field(default = 0) # orifice identifier
    speed: float = field(default = 0, metadata = {'units':'m/s'}) # tunnel speed
    nozangle: float = field(default = 0) # nozzle angle or restrictor number
    pressure: float = field(default = 0, metadata={'units':'bar'}) # nozzle pressure
    spraytype: int = field(default = 0) # model output style: 0=spray quality, 1= DSD

@dataclass
class usdaFsRotaryAtomizerData:
    maxerrorlevel: int = field(default = 0) # max error level during run 0=none, 1=warn,, 2=err,cont, 3=err,stop
    rottype: int = field(default = 0) # atomizer type: 0=AU4000, 1=AU5000
    speed: float = field(default = 0, metadata = {'units':'m/s'}) # tunnel speed
    bladeangle: float = field(default = 0, metadata = {'units':'deg'}) # blade angle
    bladerpm: float = field(default = 0, metadata = {'units':'rpm'}) # rotation rate
    flowrate: float = field(default = 0, metadata={'units':'L/min'}) # nozzle flowrate
    spraytype: int = field(default = 0) # model output style: 0=spray quality, 1= DSD

@dataclass
class sprayMaterialData:
    type: int = field(default = 0) # 0=basic, 1=user defined, 2=library
    basictype: int = field(default = 0) # 0=oil, 1 = water
    name: str = field(default = '') # spray material library substance name
    calcinputselect: int = field(default = 0) # calculation input selection: 0=rates, 1=tank mix
    nvfrac: float = field(default = 0) # nonvolatile fraction
    acfrac: float = field(default = 0) # active fraction
    actsolfrac: float = field(default = 0) # fraction of tank mix that is active solution
    addsolfract: float = field(default = 0) # fraction of tank mix that is additive solution
    actnvfrac: float = field(default = 0) # fraction of active solution that is nonvolatile
    flowrate: float = field(default = 0, metadata={'units':'L/ha or L/min'}) # flowrate
    flowrateunits: int = field(default = 0) # flowrate units 0=L/ha, 1=L/min
    specgrav: float = field(default = 0) # specific gravity of carrier
    nonvgrav: float = field(default = 0) # specific gravity of active and additive
    evaprate: float = field(default = 0) # evaporation rate

@dataclass
class aircraftData:

    '''
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
    planarea = Planform area (m^2)
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

    names = list(actype.keys())

    type: int = field(default = 0)  # 0=basic, 1=user-def, 2=library  -- Fairly certain 0 is default
    # For now will assume that the default type=0 is used to set default aircraft data
    name: str = field(default = '')# Can be defined by user, most likely will come from library
    wingtype: int = field(default=0) # 3=fixed, 4=helicopter - met data inputs converts to 1 and 2
    semispan: float = field(default=0, metadata={'unit': 'm'}) # Semispan or rotor radius
    typspeed: float = field(default=0, metadata={'unit':'m/s'}) # Typical spraying speed
    biplsep: float = field(default=0, metadata={'unit':'m'}) # Biplane distance between wings
    weight: float = field(default=0, metadata={'unit':'kg'}) # Weight of aircraft
    planarea: float = field(default=0, metadata={'units':'m^2'}) # Planform area
    proprpm: float = field(default=0, metadata={'units':'rpm'}) # Propeller or rotor RPM
    proprad: float = field(default=0, metadata={'units':'m'}) # Propeller or rotor radius
    propeff: float = field(default=0, metadata={'units':'fraction'}) # propeller efficiency
    engvert: float = field(default=0, metadata={'units':'m'}) # Engine vertical position
    engfwd: float = field(default=0, metadata={'units':'m'}) # Engine forward position
    numeng: int = field(default=0) # Number of engines
    enghoriz: list = field(default_factory=list, metadata={'units':'m'})
    wingvert: float = field(default=0, metadata={'units':'m'}) # Distance vertical from trailing edge to wingtip
    boomvert: float = field(default=0, metadata={'units':'m'}) # Distance vertical from trailing edge to boom
    boomfwd: float = field(default=0, metadata={'units':'m'}) # Distance forward from trailing edge to boom
    dragcoeff: float = field(default=0) # Drag coefficient

@dataclass
class nozzleData:
    




















