# https://jsdokken.com/src/pygmsh_tutorial.html
import os
import sys
import numpy as np
import gmsh
import pygmsh

def main():
    convexwall_file = sys.argv[1]
    mesh_file = sys.argv[2]

    if not mesh_file.endswith(".msh"):
        raise ValueError("Mesh file should end with .msh")

    convexwall_data = np.loadtxt(convexwall_file)

    resolution = 0.01
    # Channel parameters
    L = 2.2
    H = 0.41
    c = [0.2, 0.2, 0]
    r = 0.05

    # Initialize empty geometry using the build in kernel in GMSH
    geometry = pygmsh.geo.Geometry()
    # Fetch model we would like to add data to
    model = geometry.__enter__()
    # Add circle
    circle = model.add_circle(c, r, mesh_size=resolution)

    # Add points with finer resolution on left side
    points = [
        model.add_point((0, 0, 0), mesh_size=resolution),
        model.add_point((L, 0, 0), mesh_size=5 * resolution),
        model.add_point((L, H, 0), mesh_size=5 * resolution),
        model.add_point((0, H, 0), mesh_size=resolution),
    ]

    # Add lines between all points creating the rectangle
    channel_lines = [
        model.add_line(points[i], points[i + 1]) for i in range(-1, len(points) - 1)
    ]

    # Create a line loop and plane surface for meshing
    channel_loop = model.add_curve_loop(channel_lines)
    plane_surface = model.add_plane_surface(channel_loop, holes=[circle.curve_loop])

    # Call gmsh kernel before add physical entities
    model.synchronize()

    volume_marker = 6
    model.add_physical([plane_surface], "Volume")
    model.add_physical([channel_lines[0]], "Inflow")
    model.add_physical([channel_lines[2]], "Outflow")
    model.add_physical([channel_lines[1], channel_lines[3]], "Walls")
    model.add_physical(circle.curve_loop.curves, "Obstacle")

    geometry.generate_mesh(dim=2)
    gmsh.write(mesh_file)
    gmsh.clear()
    geometry.__exit__()



if __name__ == '__main__':
    main()
