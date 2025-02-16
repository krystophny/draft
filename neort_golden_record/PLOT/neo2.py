import numpy as np
from scipy.interpolate import interp1d

qe = 4.8032e-10  # elementary charge
c = 2.9979e10  # speed of light
Zi = 1  # ion charge number (deuterium)
mi = 3.3436e-24  # ion mass (deuterium)
me = 9.1094e-28  # electron mass


class Neo2Data:
    def __init__(self, filename):
        data = np.loadtxt(filename, comments="%")
        stor = data[:, 0]
        order = np.argsort(stor)
        stor = stor[order]
        data = data[order, :]

        self.stor = stor
        self.iota = interp1d(stor, data[:, 10])
        self.R0 = data[0, 11]
        self.q = interp1d(stor, 1.0 / self.iota(stor))
        self.Bref = interp1d(stor, data[:, 12])
        self.avnabpsi = interp1d(stor, data[:, 14])
        self.sqrtgBth = interp1d(stor, data[:, 15])


class Neo2ExtraProfiles:
    def __init__(self, filename):
        data = np.loadtxt(filename, comments="%")
        stor = data[:, 1]

        self.stor = stor
        self.spol = interp1d(stor, data[:, 0])
        self.ne = interp1d(stor, data[:, 9])
        self.Ti = interp1d(stor, data[:, 11] * qe)
        self.dlogneds = interp1d(stor, data[:, 12])
        self.dlogTids = interp1d(stor, data[:, 14])


def vTi(data, extra, stor):
    return np.sqrt(2.0 * extra.Ti(stor) / mi)


def rholi(data, extra, stor):
    return (mi * c * vTi(data, extra, stor)) / (qe * data.Bref(stor))


def Dp(data, extra, stor):
    return (
        np.pi
        * data.q(stor)
        * vTi(data, extra, stor)
        * (rholi(data, extra, stor) ** 2)
        / (16.0 * data.R0)
    )


def Er(data, extra, Mt, stor):
    return (
        Mt
        * vTi(data, extra, stor)
        * data.sqrtgBth(stor)
        / (data.R0 * c)
    )


def A1(data, extra, Mt, stor):
    return (
        data.avnabpsi(stor) * extra.dlogneds(stor)
        - qe * Er(data, extra, Mt, stor) / extra.Ti(stor)
        - 1.5 * data.avnabpsi(stor) * extra.dlogTids(stor)
    )


def A2(data, extra, stor):
    return data.avnabpsi(stor) * extra.dlogTids(stor)


# %% Testing
data = Neo2Data("ntv_out_nonlocal.dat")
extra = Neo2ExtraProfiles("kappa_profile_asdex30835_axi.dat")
stor = 0.39919420999999999
Mt = 5.4868750000000001E-002

np.testing.assert_almost_equal(
    extra.dlogneds(stor), -1.0151482746599974, decimal=3
)

np.testing.assert_almost_equal(
    - 1.5 * extra.dlogTids(stor), 1.5445109225307794, decimal=3
)

np.testing.assert_almost_equal(
    A2(data, extra, stor) / neo2_data.avnabpsi(stor),
    -1.0296739483538528,
    decimal=3
)

np.testing.assert_almost_equal(
    A1(data, extra, Mt, stor) / neo2_data.avnabpsi(stor),
    -0.94744307657575477,
    decimal=2
)
# %%
