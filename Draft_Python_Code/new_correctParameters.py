import math
import numpy as np

def correctParameters(rund, uo, nprp):
    # Correct Model Parameters
    rund.xo = rund.xo + uo * rund.dt # uo is aircraft typical speed

    if nprp != 0:
        for n in range(nprp):
            rund.cpxi[n] = rund.cpxi[n] + uo * rund.dt
            rn = rund.cpxi[n] / 11.785
            rund.vprp[n] = rund.vprp[n] * (rund.rprp[n] / rn) ** 2
            rund.rprp[n] = rn

    # Helicopter stuff here as well lines 288-298 Ageqn
