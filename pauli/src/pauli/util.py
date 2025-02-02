from numba import njit


@njit
def legendre_p(n, x):
    """ "
    Returns the value of the Legendre polynomial P_n(x).

    Parameters:
    n (int): The degree of the polynomial.
    x (float): The point at which to evaluate the polynomial. Must satisfy -1 <= x <= 1.
    """
    if n == 0:
        return 1.0
    if n == 1:
        return x

    p_prev2 = 1.0
    p_prev1 = x
    for n in range(2, n + 1):
        p = ((2 * n - 1) * x * p_prev1 - (n - 1) * p_prev2) / n
        p_prev2 = p_prev1
        p_prev1 = p
    return p_prev1


@njit
def assoc_legendre_p(n, m, x):
    """
    Returns the value of the associated Legendre polynomial P_n^m(x).

    Parameters:
    n (int): The degree of the polynomial. Must be greater than or equal to m.
    m (int): The order of the polynomial. Must satisfy 0 <= m <= n.
    x (float): The point at which to evaluate the polynomial. Must satisfy -1 <= x <= 1.
    """
    if m < 0 or m > n:
        return 0.0

    if m == n:
        fact = 1.0
        for k in range(n):
            fact *= -(2 * k + 1)
        return fact * (1 - x * x) ** (n / 2)

    pmm = 1.0
    fact = 1.0
    for k in range(m):
        pmm *= -(2 * k + 1) * (1 - x * x) ** (0.5)

    if n == m:
        return pmm

    pmmp1 = x * (2 * m + 1) * pmm

    if n == m + 1:
        return pmmp1

    for n in range(m + 2, n + 1):
        pmn = (x * (2 * n - 1) * pmmp1 - (n + m - 1) * pmm) / (n - m)
        pmm = pmmp1
        pmmp1 = pmn

    return pmmp1
