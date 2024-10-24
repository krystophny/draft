#%%
from simsopt.mhd import Spec
from simsopt.util import MpiPartition
from simsopt.objectives import LeastSquaresProblem
from simsopt.solve import least_squares_serial_solve

mpi = MpiPartition(1)
equil = Spec('2DOF_targetIotaAndVolume.sp', mpi=mpi)

surf = equil.boundary
surf.fix_all()
surf.unfix('rc(1,1)')
surf.unfix('zs(1,1)')

# Create or load CoilSet
# Create CoilNormalField
#equil.normal_field = normal_field

# %%
equil.run()

# %%
import matplotlib.pyplot as plt
fig, ax = plt.subplots()
equil.results.plot_kam_surface(ax=ax)
equil.results.plot_poincare(ax=ax)
plt.show()

# %%
