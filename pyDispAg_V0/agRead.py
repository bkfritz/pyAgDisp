'''
Code to reproduce AGREAD.for

AGREAD processes all input data and does unit conversions and error checking.

iunit = unit flag: 0 = English, 1 = metric
ier = error flag:   0 = no warning
                    1 = write warning information
                    2 = write error information and continue
                    3 = write error information and stop
iwr = write flag:   0 = no write to screen
                    1 = string only to screen
                    2 = string plus real value to screen
                    3 = string plus integer value to screen
realwd = real data array (value, minimum, maximum
chstr = character string
jchstr = length of character string

The initial call to agread is agread(iunit, ier, iwr, realwd, chstr, jchstr)

'''

from dataclasses import dataclass, field
import numpy as np
import math

@dataclass
class agReadData:

    tpi = float = 6.2831853
    cstab = ['Strong Solar Insolation', 'Moderate Solar Insolation', 'Slight Solar Insolation',
             'Weak Solar Insolation', 'Overcast Cloud Cover', 'Thinly Overcast Cloud Cover',
             '< 3/8 Cloud Cover']
    jstab = [23, 25, 23, 21, 20, 27, 19]
    cfrep = ['1', '2', '3', '4', '5', '6', '7', '8', '9', '10']

    ier = 0
    iwr = 0
    jchstr = 0
    icard = icard + 1
    i =


    windtype: int = field(default=0)  # Wind input type: 0=single height, 1 = wind table
    windspd: float = field(default=2.24, metadata={'unit': 'm/s'})  # Default starting value Agdisp