from numba import njit
from numba.experimental import jitclass

@jitclass
class A:
    def __init__(self):
        pass
    def doit(self):
        return 123

@jitclass
class B:
    def __init__(self):
        pass
    def doit(self):
        return 456.3

@njit(debug=True)
def call_doit(obj):
    return obj.doit()

print(call_doit(A()))
print(call_doit(B()))
