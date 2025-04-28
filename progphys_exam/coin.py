import numpy as np

def coin_flip(n):
    return np.random.randint(0, 2, size=n)

print(coin_flip(1))
