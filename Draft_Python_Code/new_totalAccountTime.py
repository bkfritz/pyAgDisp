import math
import numpy as np
''' 
Replicates lines 155-171 AgEqn.for.
Total accountancy in Time
'''

def totalAccountTime(i, rund, totacct, xnv, etem, ctem, cnew, ygrid2):
    # Total accountancy in time
    t = rund.t
    nt = 0  # This is 1 in fortran
    while t > totacct.tattv[nt] and nt < totacct.natt:
        nt = nt + 1

    if xnv[1, i] < ygrid2:
        totacct.inmax = max(totacct.inmax, nt)

    for n in range(nt, totacct.natt):  # acct.natt may need a +1 or -1 either here or in Class code
        totacct.tatfv[0, n] = totacct.tatfv[0, n] + etem
        totacct.tatfv[1, n] = totacct.tatfv[1, n] + ctem
        if rund.isw[i] < 0:
            totacct.tatfv[2, n] = totacct.tatfv[2, n] + rund.ymass * cnew * (rund.ednv[i] / rund.diam) ** 3