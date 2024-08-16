import numpy as np
from numba import jit

@jit(nopython=True)
def spl_five_reg(n, h, a, b, c, d, e, f):
    rhop = 13.0 + np.sqrt(105.0)
    rhom = 13.0 - np.sqrt(105.0)

    a11, a12, a13 = 1.0, 1.0/4.0, 1.0/16.0
    a21, a22, a23 = 3.0, 27.0/4.0, 9.0 * 27.0/16.0
    a31, a32, a33 = 5.0, 125.0/4.0, 5.0**5/16.0
    det = (a11 * a22 * a33 + a12 * a23 * a31 + a13 * a21 * a32
           - a12 * a21 * a33 - a13 * a22 * a31 - a11 * a23 * a32)

    # Initial boundaries
    b1, b2, b3 = a[3] - a[2], a[4] - a[1], a[5] - a[0]
    bbeg = (b1 * a22 * a33 + a12 * a23 * b3 + a13 * b2 * a32
            - a12 * b2 * a33 - a13 * a22 * b3 - b1 * a23 * a32) / det
    dbeg = (a11 * b2 * a33 + b1 * a23 * a31 + a13 * a21 * b3
            - b1 * a21 * a33 - a13 * b2 * a31 - a11 * a23 * b3) / det
    fbeg = (a11 * a22 * b3 + a12 * b2 * a31 + b1 * a21 * a32
            - a12 * a21 * b3 - b1 * a22 * a31 - a11 * b2 * a32) / det

    b1, b2, b3 = a[n-3] - a[n-4], a[n-2] - a[n-5], a[n-1] - a[n-6]
    bend = (b1 * a22 * a33 + a12 * a23 * b3 + a13 * b2 * a32
            - a12 * b2 * a33 - a13 * a22 * b3 - b1 * a23 * a32) / det
    dend = (a11 * b2 * a33 + b1 * a23 * a31 + a13 * a21 * b3
            - b1 * a21 * a33 - a13 * b2 * a31 - a11 * a23 * b3) / det
    fend = (a11 * a22 * b3 + a12 * b2 * a31 + b1 * a21 * a32
            - a12 * a21 * b3 - b1 * a22 * a31 - a11 * b2 * a32) / det

    a11, a12, a13 = 2.0, 1.0/2.0, 1.0/8.0
    a21, a22, a23 = 2.0, 9.0/2.0, 81.0/8.0
    a31, a32, a33 = 2.0, 25.0/2.0, 625.0/8.0
    det = (a11 * a22 * a33 + a12 * a23 * a31 + a13 * a21 * a32
           - a12 * a21 * a33 - a13 * a22 * a31 - a11 * a23 * a32)

    b1, b2, b3 = a[3] + a[2], a[4] + a[1], a[5] + a[0]
    abeg = (b1 * a22 * a33 + a12 * a23 * b3 + a13 * b2 * a32
            - a12 * b2 * a33 - a13 * a22 * b3 - b1 * a23 * a32) / det
    cbeg = (a11 * b2 * a33 + b1 * a23 * a31 + a13 * a21 * b3
            - b1 * a21 * a33 - a13 * b2 * a31 - a11 * a23 * b3) / det
    ebeg = (a11 * a22 * b3 + a12 * b2 * a31 + b1 * a21 * a32
            - a12 * a21 * b3 - b1 * a22 * a31 - a11 * b2 * a32) / det

    b1, b2, b3 = a[n-3] + a[n-4], a[n-2] + a[n-5], a[n-1] + a[n-6]
    aend = (b1 * a22 * a33 + a12 * a23 * b3 + a13 * b2 * a32
            - a12 * b2 * a33 - a13 * a22 * b3 - b1 * a23 * a32) / det
    cend = (a11 * b2 * a33 + b1 * a23 * a31 + a13 * a21 * b3
            - b1 * a21 * a33 - a13 * b2 * a31 - a11 * a23 * b3) / det
    eend = (a11 * a22 * b3 + a12 * b2 * a31 + b1 * a21 * a32
            - a12 * a21 * b3 - b1 * a22 * a31 - a11 * b2 * a32) / det

    alp = np.zeros(n)
    bet = np.zeros(n)
    gam = np.zeros(n)

    alp[0] = 0.0
    bet[0] = ebeg * (2.0 + rhom) - 5.0 * fbeg * (3.0 + 1.5 * rhom)

    for i in range(n-4):
        ip1 = i + 1
        alp[ip1] = -1.0 / (rhop + alp[i])
        bet[ip1] = alp[ip1] * (bet[i] - 5.0 * (a[i+3] - 4.0 * a[i+2] + 6.0 * a[i+1] - 4.0 * a[ip1] + a[i]))

    gam[n-3] = eend * (2.0 + rhom) + 5.0 * fend * (3.0 + 1.5 * rhom)
    for i in range(n-4, -1, -1):
        gam[i] = gam[i+1] * alp[i] + bet[i]

    alp[0] = 0.0
    bet[0] = ebeg - 2.5 * 5.0 * fbeg

    for i in range(n-2):
        ip1 = i + 1
        alp[ip1] = -1.0 / (rhom + alp[i])
        bet[ip1] = alp[ip1] * (bet[i] - gam[i])

    e[n-1] = eend + 2.5 * 5.0 * fend
    e[n-2] = e[n-1] * alp[n-2] + bet[n-2]
    f[n-2] = (e[n-1] - e[n-2]) / 5.0
    e[n-3] = e[n-2] * alp[n-3] + bet[n-3]
    f[n-3] = (e[n-2] - e[n-3]) / 5.0
    d[n-3] = dend + 1.5 * 4.0 * eend + 1.5**2 * 10.0 * fend

    for i in range(n-4, -1, -1):
        e[i] = e[i+1] * alp[i] + bet[i]
        f[i] = (e[i+1] - e[i]) / 5.0
        d[i] = (a[i+2] - 3.0 * a[i+1] + 3.0 * a[i] - a[i-1]) / 6.0 \
               - (e[i+2] + 27.0 * e[i+1] + 93.0 * e[i] + 59.0 * e[i-1]) / 30.0
        c[i] = 0.5 * (a[i+1] + a[i-1]) - a[i] - 0.5 * d[i+1] - 2.5 * d[i] \
               - 0.1 * (e[i+2] + 18.0 * e[i+1] + 31.0 * e[i])
        b[i] = a[i] - a[i-1] - c[i] - d[i] - 0.2 * (4.0 * e[i] + e[i+1])

    for i in range(n-4, n):
        b[i] = b[i-1] + 2.0 * c[i-1] + 3.0 * d[i-1] + 4.0 * e[i-1] + 5.0 * f[i-1]
        c[i] = c[i-1] + 3.0 * d[i-1] + 6.0 * e[i-1] + 10.0 * f[i-1]
        d[i] = d[i-1] + 4.0 * e[i-1] + 10.0 * f[i-1]
        if i != n-1:
            f[i] = a[i+1] - a[i] - b[i] - c[i] - d[i] - e[i]

    f[n-1] = f[n-2]

    fac = 1.0 / h
    b *= fac
    fac /= h
    c *= fac
    fac /= h
    d *= fac
    fac /= h
    e *= fac
    fac /= h
    f *= fac

# Example usage
n = 10
h = 1.0
a = np.random.rand(n)
b = np.zeros(n)
c = np.zeros(n)
d = np.zeros(n)
e = np.zeros(n)
f = np.zeros(n)

print("===================================")
print(a, b, c, d, e, f)
print("===================================")
spl_five_reg(n, h, a, b, c, d, e, f)
print(a, b, c, d, e, f)
print("===================================")
