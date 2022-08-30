import math

def correctCirculationDecay(rund, xnv, nvor, gdk, gdko, bstab, nvar):
    # Correct circulation decay
    if rund.xo > 0.0:
        for n in range(nvor):
            if abs(rund.gdkv[n] > 1e-10):
                tem = max(1, rund.ybar[n] - rund.ybal[n])
                htem = 2.0 * rund.zbar[n] / tem

                if htem > 2.0:
                    gdkt = gdko + (gdk - gdko) / 0.102167 / (htem ** 3.291)
                else:
                    gdkt = gdk

                rund.gdkv[n] = rund.gdkv[n] * math.exp(-1 * min(abs(gdkt * bstab * rund.dt / tem), 25))

                if rund.gdkv[n] > 0.001:
                    for i in range(nvar):
                        if rund.isw[i] != 0:
                            rund.ygaus1 = max(rund.ygaus1, xnv[1, i])