#include <iostream>
#include <optional> // for std::optional
#include <variant>  // for std::variant, std::get_if

// 1. CGAL Kernel
#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Surface_mesh.h>

// 2. AABB Tree (use the newer AABB_traits_3.h)
#include <CGAL/AABB_tree.h>
#include <CGAL/AABB_traits_3.h>
#include <CGAL/AABB_face_graph_triangle_primitive.h>

// 3. Polygon mesh I/O for reading STL
#include <CGAL/IO/polygon_mesh_io.h>

int main()
{
    // Kernel typedefs
    using Kernel = CGAL::Exact_predicates_inexact_constructions_kernel;
    using Point = Kernel::Point_3;
    using Segment = Kernel::Segment_3;
    using Ray = Kernel::Ray_3;

    // Surface mesh
    using SurfaceMesh = CGAL::Surface_mesh<Point>;
    double xsource[3];
    double xdirection[3];

    // Load the STL
    SurfaceMesh mesh;
    if (!CGAL::IO::read_polygon_mesh("../surface_mesh.stl", mesh))
    {
        std::cerr << "Error: cannot read STL mesh from file.\n";
        return EXIT_FAILURE;
    }
    std::cout << "Loaded mesh with "
              << num_faces(mesh) << " faces and "
              << num_vertices(mesh) << " vertices.\n";

    std::cout << "Enter the source point coordinates: ";
    std::cin >> xsource[0] >> xsource[1] >> xsource[2];
    std::cout << "Enter the direction vector coordinates: ";
    std::cin >> xdirection[0] >> xdirection[1] >> xdirection[2];

    // AABB tree typedefs
    using Primitive = CGAL::AABB_face_graph_triangle_primitive<SurfaceMesh>;
    using AABBTraits = CGAL::AABB_traits_3<Kernel, Primitive>;
    using AABBTree = CGAL::AABB_tree<AABBTraits>;

    // Build the AABB tree
    AABBTree tree(faces(mesh).begin(), faces(mesh).end(), mesh);
    tree.accelerate_distance_queries(); // optional performance tweak

    // Define a ray for intersection
    Point source(0.0, 0.0, 0.0);
    Point direction(1.0, 0.0, 0.0);
    Ray ray(source, direction);

    // Query the first intersection
    auto result = tree.any_intersection(ray);
    auto start = std::chrono::high_resolution_clock::now();
    Point source2(0.0, 0.0, 0.0);
    Point direction2(1.0, 0.00001, 0.0);
    Ray ray2(source, direction2);
    result = tree.any_intersection(ray2);
    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end - start;
    std::cout << "Intersection query took " << elapsed.count() << " seconds.\n";

    if (result)
    {
        // Extract the pair
        const auto &[variant_geom, face_idx] = *result;

        // Check which type is in the variant
        if (const Point *ipoint = std::get_if<Point>(&variant_geom))
        {
            std::cout << "First intersection is a POINT at "
                      << *ipoint << "\n";
            std::cout << "Hit face index: " << face_idx << "\n";
        }
        else if (const Segment *iseg = std::get_if<Segment>(&variant_geom))
        {
            std::cout << "First intersection is a SEGMENT from "
                      << iseg->source() << " to "
                      << iseg->target() << "\n";
            std::cout << "Hit face index: " << face_idx << "\n";
        }
    }
    else
    {
        std::cout << "No intersection found along the ray.\n";
    }

    return 0;
}
