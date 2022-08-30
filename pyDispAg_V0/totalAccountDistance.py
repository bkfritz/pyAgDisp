
''' 
Replicates lines 139-150 AgEqn.for.
Total accountancy in Distance
'''

def totalAccountDistance(i, rund, totacct, xnv, etem, ctem, cnew):
    # Total accountancy in Distance

    nd = 0 # This is a 1 in fortran
    while xnv[1, i] > totacct.taddv[nd] and nd < totacct.nadd:  # This is nested if loops and a goto in FORTRAN
        nd = nd + 1  # I think this can be done easier with something like np.where

    totacct.tadfv[0, nd] = totacct.tadfv[0, nd] + etem
    totacct.tadfv[1, nd] = totacct.tadfv[1, nd] + ctem
    if rund.isw[i] < 0:
        totacct.tadfv[2, nd] = totacct.tadfv[2, nd] + rund.ymass * cnew * (rund.ednv[i] / rund.diam) ** 3