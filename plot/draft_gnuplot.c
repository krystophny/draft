#include <stdio.h>
#include <math.h>
#include "gnuplot_i.h"

int main() {
    gnuplot_ctrl *h; // Gnuplot handle
    double x, y;
    int i;

    // Open a Gnuplot session
    h = gnuplot_init();

    // Set plot title and labels
    gnuplot_cmd(h, "set title 'Sine Wave'");
    gnuplot_cmd(h, "set xlabel 'x'");
    gnuplot_cmd(h, "set ylabel 'sin(x)'");

    // Plot a sine wave
    gnuplot_cmd(h, "plot '-' with lines title 'sin(x)'");
    for (i = 0; i < 100; i++) {
        x = i * 0.1;
        y = sin(x);
        gnuplot_cmd(h, "%f %f", x, y);
    }
    gnuplot_cmd(h, "e"); // End of data

    // Pause to view the plot
    printf("Press Enter to continue...\n");
    getchar();

    // Close the Gnuplot session
    gnuplot_close(h);

    return 0;
}
