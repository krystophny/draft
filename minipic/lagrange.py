from numba import njit

@njit
def basis(x, i):
    value = 1.0
    for j in range(order + 1):
        if i != j:
            value *= (x - nodes[j]) / (nodes[i] - nodes[j])
    return value


@njit
def basis_derivative(x, i):
    derivative = 0.0
    for j in range(order + 1):
        if i != j:
            term = 1.0 / (nodes[i] - nodes[j])
            for k in range(order + 1):
                if k != i and k != j:
                    term *= (x - nodes[k]) / (nodes[i] - nodes[k])
            derivative += term
    return derivative
