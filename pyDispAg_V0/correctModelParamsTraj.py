def correctModelParamsTraj(traj, uo, nprp):
    # Correct Model Parameters
    traj.xo = traj.xo + uo * traj.dt # uo is aircraft typical speed

    if nprp != 0:
        for n in range(nprp):
            traj.cpxi[n] = traj.cpxi[n] + uo * traj.dt
            rn = traj.cpxi[n] / 11.785
            traj.vprp[n] = traj.vprp[n] * ((traj.rprp[n] / rn) ** 2)
            traj.rprp[n] = rn

    # Helicopter stuff here as well lines 288-298 Ageqn
