#include "mfem.hpp"
using namespace mfem;
using namespace std;

int main(int argc, char *argv[])
{
    // 1. Define the coordinates of the points.
    vector<Vector> points;
    points.push_back(Vector({0.0, 0.0}));
    points.push_back(Vector({1.0, 0.0}));
    points.push_back(Vector({0.0, 1.0}));
    points.push_back(Vector({1.0, 1.0}));

    int num_points = points.size();
    int dim = points[0].Size();

    // 2. Create a mesh from the points.
    Mesh mesh(dim, num_points, 0, 0);

    for (const auto &point : points)
    {
        mesh.AddVertex(point.GetData());
    }

    // 3. Define a finite element space on the mesh.
    int order = 1;
    FiniteElementCollection *fec = new H1_FECollection(order, dim);
    FiniteElementSpace *fespace = new FiniteElementSpace(&mesh, fec);

    // 4. Define a grid function and project some function into the FEM space.
    GridFunction gf(fespace);
    FunctionCoefficient initial_solution([](const Vector &x) { return sin(M_PI * x[0]) * sin(M_PI * x[1]); });
    gf.ProjectCoefficient(initial_solution);

    // 5. Define a callable function using the grid function.
    auto eval_function = [&gf, fespace](const Vector &x)
    {
        // Create a DenseMatrix for the point
        DenseMatrix point_mat(x.Size(), 1);
        for (int i = 0; i < x.Size(); ++i)
        {
            point_mat(i, 0) = x[i];
        }

        // Prepare storage for the element ID and integration point
        Array<int> elem_ids(1);
        Array<IntegrationPoint> ips(1);

        // Find the element containing the point
        fespace->GetMesh()->FindPoints(point_mat, elem_ids, ips);

        if (elem_ids[0] < 0)
        {
            cerr << "Point not found in the mesh." << endl;
            return 0.0;
        }

        // Get the transformation of the element
        ElementTransformation *T = fespace->GetElementTransformation(elem_ids[0]);
        return gf.GetValue(*T, ips[0]);
    };

    // Example usage of eval_function
    Vector point(2);
    point[0] = 0.25;
    point[1] = 0.5;
    double value = eval_function(point);
    cout << "Function value at (" << point[0] << ", " << point[1] << ") is " << value << endl;

    // 6. Clean up
    delete fespace;
    delete fec;

    return 0;
}
