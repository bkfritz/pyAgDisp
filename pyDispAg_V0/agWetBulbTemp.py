import math
'''
SUBROUTINE AGWTB(TMPR,RHUM,PRES,WETB)

AGWTB computes the wet bulb temperature depression

TMPR   - Temperature (deg C)
RHUM   - Relative humidity (%)
PRES   - Pressure (mb)
WETB   - Wet bulb temperature depression (deg C)
'''

def fpres(temp):
    theta = (temp + 459.67) / 1165.14
    omt = 1.0 - theta
    sumt = (-omt * (7.691234564+omt * (26.08023696+omt *
      (168.1706546+omt * (-64.23285504+omt * 118.9646225)))))
    sumt = (sumt/theta/(1.0+omt * (4.16711732+omt *
     20.9750676))-omt/(omt*omt*1.0e+09+6.0))
    fpres=math.exp(sumt)*3208.235
    return fpres

def wetBulbTemp(tmpr, rhum, pres):

    tdry = 1.8 * tmpr + 32.0
    pamb = 14.7 * pres / 1013.0

    pdry = fpres(tdry)
    psat = 0.01 * rhum * pdry
    tmin = 0.0
    tmax = tdry

    for iter in range(20):
        temp = 0.5 * (tmin + tmax)
        ptem = fpres(temp)
        pnew = ptem - (pamb - ptem) * (tdry - temp) / (2800.0 - 1.3 * temp)
        if (abs(pnew-psat) < 0.001):
            break
        if (pnew < psat):
            tmin = temp
        else:
            tmax = temp

    wetb = (tdry - temp) / 1.8

    return wetb

