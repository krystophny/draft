from numba import njit

@njit
def associated_legendre_p(m, v, x):
   if v < 0 or v > m:
       return 0.0

   if v == m:
       fact = 1.0
       for i in range(m):
           fact *= -(2*i + 1)
       return fact * (1 - x*x)**(m/2)

   pmm = 1.0
   fact = 1.0
   for i in range(v):
       pmm *= -(2*i + 1)*(1 - x*x)**(0.5)

   if m == v:
       return pmm

   pmmp1 = x*(2*v + 1)*pmm

   if m == v + 1:
       return pmmp1

   for n in range(v + 2, m + 1):
       pmn = (x*(2*n - 1)*pmmp1 - (n + v - 1)*pmm)/(n - v)
       pmm = pmmp1
       pmmp1 = pmn

   return pmmp1
