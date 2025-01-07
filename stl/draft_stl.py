import gmsh

# Initialize Gmsh
gmsh.initialize()
gmsh.model.add("surface_mesh")

# Create a sphere (as an example surface)
radius = 1.0
sphere_center = (0, 0, 0)

# Add a sphere surface
gmsh.model.occ.addSphere(*sphere_center, radius)
gmsh.model.occ.synchronize()

# Generate a 3D mesh (triangulated surface)
gmsh.model.mesh.generate(2)

# Write the mesh to an STL file
output_filename = "surface_mesh.stl"
gmsh.write(output_filename)
print(f"Mesh written to {output_filename}")

# Finalize Gmsh
gmsh.finalize()
