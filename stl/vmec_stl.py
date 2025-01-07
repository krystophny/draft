import os
from pathlib import Path
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

from simsopt.geo import SurfaceRZFourier
import gmsh

data_path = Path(os.path.expandvars("$DATA/TESTS/VMEC/SQuID_2024"))
file_path = data_path / "wout_6A5KTqRRQThnywp3eZnV.nc"

surf = SurfaceRZFourier.from_wout(file_path)

surf.plot(close=True, show=True)

XYZ = surf.gamma()
fig, ax = plt.subplots(subplot_kw={"projection": "3d"})
plt.plot(XYZ[:, :, 0], XYZ[:, :, 1], XYZ[:, :, 2], '.')

# Initialize Gmsh
gmsh.initialize()
gmsh.model.add("ClosedSurfaceMesh")

# Extract points from XYZ
points = XYZ.reshape(-1, 3)  # Reshape to list of (x, y, z)
point_ids = []

# Add points to Gmsh
for i, (x, y, z) in enumerate(points):
    point_id = gmsh.model.geo.addPoint(x, y, z, meshSize=1.0)  # Adjust meshSize as needed
    point_ids.append(point_id)

# Connect points to form lines and a periodic surface
num_u, num_v, _ = XYZ.shape
for i in range(num_u):
    for j in range(num_v):
        # Identify the points, wrapping around in u and v directions
        p1 = point_ids[i * num_v + j]
        p2 = point_ids[i * num_v + (j + 1) % num_v]
        p3 = point_ids[((i + 1) % num_u) * num_v + (j + 1) % num_v]
        p4 = point_ids[((i + 1) % num_u) * num_v + j]

        # Create lines for the quad
        l1 = gmsh.model.geo.addLine(p1, p2)
        l2 = gmsh.model.geo.addLine(p2, p3)
        l3 = gmsh.model.geo.addLine(p3, p4)
        l4 = gmsh.model.geo.addLine(p4, p1)

        # Create a curve loop and plane surface
        cl = gmsh.model.geo.addCurveLoop([l1, l2, l3, l4])
        gmsh.model.geo.addPlaneSurface([cl])

# Synchronize and generate the mesh
gmsh.model.geo.synchronize()
gmsh.model.mesh.generate(2)

# Save the mesh as STL
stl_file = "closed_surface_mesh.stl"
gmsh.write(stl_file)
print(f"STL file saved to {stl_file}")

# Optional: Visualize in Gmsh
gmsh.fltk.run()

# Finalize Gmsh
gmsh.finalize()
