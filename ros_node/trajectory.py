import numpy as np
import kinematics as kin

def cubic_trajectory_planning(q0 , qf, qd0, qdf, m=100):

    n = len(q0)

    a0 = np.copy(q0)
    a1 = np.zeros(n)
    a2 = 3 * (qf - q0) - 2 * qd0 - qdf
    a3 = -2 * (qf - q0) + qd0 + qdf

    timesteps = np.linspace(0, 1, m)

    q = np.zeros((n, m))
    qd = np.zeros((n, m))
    qdd = np.zeros((n, m))

    for i in range(len(timesteps)):
        t = timesteps[i]
        t_2 = t**2
        t_3 = t**3
        q[:, i] = a0 + a1 * t + a2 * t_2 + a3 * t_3
        qd[:, i] = a1 + 2 * a2 * t + 3 * a3 * t_2
        qdd[:, i] = 2 * a2 + 6 * a3 * t

    return q, qd, qdd