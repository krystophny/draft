# https://jsdokken.com/src/pygmsh_tutorial.html
import os
import sys
import numpy as np
import matplotlib.pyplot as plt
import gmsh
import pygmsh

def main():
    wall_file = sys.argv[1]
    mesh_file = sys.argv[2]

    if not mesh_file.endswith(".msh"):
        raise ValueError("Mesh file should end with .msh")

    wall_data = np.loadtxt(wall_file)

    geometry = pygmsh.geo.Geometry()
    model = geometry.__enter__()

    wall_points = [
        model.add_point(wall_data[i])
        for i in range(len(wall_data) - 1)
    ]

    wall_lines = [
        model.add_line(wall_points[i], wall_points[i + 1])
        for i in range(-1, len(wall_points) - 1)
    ]

    # Create a line loop and plane surface for meshing
    wall_loop = model.add_curve_loop(wall_lines)
    plane_surface = model.add_plane_surface(wall_loop)

    # Call gmsh kernel before add physical entities
    model.synchronize()

    # volume_marker = 6
    # model.add_physical([plane_surface], "Volume")

    geometry.generate_mesh(dim=2)
    gmsh.write(mesh_file)
    gmsh.clear()
    geometry.__exit__()

if __name__ == '__main__':
    main()
