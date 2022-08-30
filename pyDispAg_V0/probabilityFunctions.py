'''
Author: Brad Fritz
Date: January 2022
Project: AgDISP Fortran to Python

Description:  Porting AgDISP to Python.

This function captures the probability function fx and fp used in the AgDISP DSD
recovery functions as well as an interpolate function.
'''

import math

def fx(p):
    # Probability Argument (Abramowitz and Stegun 26.2.23)
    c0 = 2.515517
    c1 = 0.802853
    c2 = 0.010328
    d1 = 1.432788
    d2 = 0.189269
    d3 = 0.001308
    x = p
    sign = -1
    if x < 0.5:
        x = 1 - x
        sign = 1
    t = math.sqrt(math.log(1/x**2))
    fx_val = sign * (t - (c0 + t * (c1 + t * c2)) / (1 + t * (d1 + t * (d2 + t * d3))))
    return fx_val

def fp(x):
    # Probability Argument (Abramowitz and Stegun 26.2.18)
    c1 = 0.196854
    c2 = 0.115194
    c3 = 0.000344
    c4 = 0.019527

    p = abs(x)
    fp_val = 1 - 0.5/(1 + p * (c1 + p * (c2 + p * (c3 + p * c4))))**4
    if x < 0:
        fp_val = 1 - fp_val
    return fp_val

def interpolate(frac, dsd, xcol, ycol):
    x1 = dsd.loc[dsd[xcol] < frac][-1:][xcol].values[0]
    x2 = dsd.loc[dsd[xcol] > frac][0:][xcol].values[0]
    y1 = dsd.loc[dsd[xcol] < frac][-1:][ycol].values[0]
    y2 = dsd.loc[dsd[xcol] > frac][0:][ycol].values[0]

    return y1 + (frac - x1) / (x2 - x1) * (y2 - y1)