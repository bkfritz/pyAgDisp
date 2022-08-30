import math
import numpy as np

def correctCircDecayTraj(traj, nvor, gdk, gdko, bstab):
    # Correct circulation decay
    if traj.xo > 0.0:
        for n in range(nvor):
            if abs(traj.gdkv[n] > 1e-10):
                tem = max(1.0, (traj.ybar[n] - traj.ybal[n]))
                #print(traj.t, tem)
                htem = 2.0 * traj.zbar[n] / tem
                #print('tem and htem', tem, htem)
                if htem > 2.0:
                    gdkt = gdko + (gdk - gdko) / 0.102167 / (htem ** 3.291)
                else:
                    gdkt = gdk

                # print(math.exp(-1.0 * min(abs(gdkt * bstab * traj.dt / tem), 25.0)))

                traj.gdkv[n] = traj.gdkv[n] * math.exp(-1.0 * min(abs(gdkt * bstab * traj.dt / tem), 25.0))
                #print(traj.t, traj.gdkv[n])