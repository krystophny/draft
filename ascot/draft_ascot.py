import numpy as np
from a5py import Ascot

a5 = Ascot("ascot.h5", create=True)
print("File created")

# Use pre-existing template to create some input data
a5.data.create_input("options tutorial")
a5.data.create_input("bfield analytical iter circular")
a5.data.create_input("wall rectangular")
a5.data.create_input("plasma flat")

# Create electric field and markers by giving input parameters explicitly
from a5py.ascot5io.marker import Marker
mrk = Marker.generate("gc", n=100, species="alpha")
mrk["energy"][:] = 3.5e6
mrk["pitch"][:]  = 0.99 - 1.98 * np.random.rand(100,)
mrk["r"][:]      = np.linspace(6.2, 8.2, 100)
a5.data.create_input("gc", **mrk)
a5.data.create_input("E_TC", exyz=np.array([0,0,0])) # Zero electric field

# Create dummy input for the rest
a5.data.create_input("N0_3D")
a5.data.create_input("Boozer")
a5.data.create_input("MHD_STAT")
a5.data.create_input("asigma_loc")
print("Inputs initialized")

name = a5.data.options.active.new(ENDCOND_MAX_MILEAGE=0.5e-2, desc="Fast")
a5.data.options[name].activate()
print("Options updated")

import subprocess
subprocess.run([os.expanduser("$CODE/external/ascot5/build/ascot5_main"), "--d=\"Hello world!\""])
print("Simulation completed")
