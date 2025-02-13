#include <stdio.h>
#include <stdlib.h>
#include <time.h>

// Function to perform arithmetic on a double
double processDouble(double value) {
    return value / 2.0; // Example: divide the double by 2
}

int main() {
    // Initialize the value
    double value = 2.718281828459045; // Example double value

    // Start the clock
    clock_t start = clock();

    // Process the value 1,000,000,000 times
    const int iterations = 1000000000;
    for (int i = 0; i < iterations; i++) {
        value = processDouble(value);
    }

    // Stop the clock
    clock_t end = clock();

    // Calculate the elapsed time in seconds
    double elapsedTime = (double)(end - start) / CLOCKS_PER_SEC;

    // Print the result and the time taken
    printf("Processed double: %lf\n", value);
    printf("Time taken for %d iterations: %.6f seconds\n", iterations, elapsedTime);

    return 0;
}
