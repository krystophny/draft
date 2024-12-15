from numba import njit
from numba.experimental import jitclass

class Base():
    def doit(self):
        raise NotImplementedError

@jitclass
class A(Base):
    x: float
    y: float

    def __init__(self, x: float, y: float):
        self.x = x
        self.y = y

    def doit(self):
        return self.x + self.y


@jitclass
class B(Base):
    s: str

    def __init__(self, s: str):
        self.s = s

    def doit(self):
        return self.s


@njit
def call_doit(obj):
    return obj.doit()


def factory(kind: str, x: float, y: float):
    if kind == 'A':
        return A(x, y)
    elif kind == 'B':
        return B(f'{x} {y}')


a = A(1.0, 2.0)
b = B('hello')
c = factory('A', 3.0, 4.0)
d = factory('B', 5.0, 6.0)


print(call_doit(a))
print(call_doit(b))
print(call_doit(c))
print(call_doit(d))
