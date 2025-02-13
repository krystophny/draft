#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

// Define an enum to represent the type of data stored in the union
typedef enum {
    TYPE_INT,
    TYPE_FLOAT,
    TYPE_DOUBLE,
    TYPE_UNKNOWN
} DataType;

// Define a union to hold different types of data
typedef union {
    int intValue;
    float floatValue;
    double doubleValue;
} Data;

// Define a struct that holds the union and the type of data it contains
typedef struct {
    DataType type;
    Data data;
} Value;

// Function to perform arithmetic on an int
int processInt(int value) {
    return value + 10; // Example: add 10 to the int
}

// Function to perform arithmetic on a float
float processFloat(float value) {
    return value * 2.0f; // Example: multiply the float by 2
}

// Function to perform arithmetic on a double
double processDouble(double value) {
    return value / 2.0; // Example: divide the double by 2
}

// Function to process the value based on its type
Data processValue(Value *value) {
    switch (value->type) {
        case TYPE_INT:
            value->data.intValue = processInt(value->data.intValue);
            break;
        case TYPE_FLOAT:
            value->data.floatValue = processFloat(value->data.floatValue);
            break;
        case TYPE_DOUBLE:
            value->data.doubleValue = processDouble(value->data.doubleValue);
            break;
        default:
            printf("Unsupported type\n");
            break;
    }
}

// Function to read the data type from the config file
DataType readConfig(const char* filename) {
    FILE* file = fopen(filename, "r");
    if (!file) {
        perror("Failed to open config file");
        return TYPE_UNKNOWN;
    }

    char type[16];
    if (fscanf(file, "%15s", type) != 1) {
        fclose(file);
        return TYPE_UNKNOWN;
    }

    fclose(file);

    if (strcmp(type, "int") == 0) {
        return TYPE_INT;
    } else if (strcmp(type, "float") == 0) {
        return TYPE_FLOAT;
    } else if (strcmp(type, "double") == 0) {
        return TYPE_DOUBLE;
    } else {
        return TYPE_UNKNOWN;
    }
}

int main() {
    // Read the data type from the config file
    const char* configFile = "config.txt";
    DataType type = readConfig(configFile);

    if (type == TYPE_UNKNOWN) {
        fprintf(stderr, "Invalid or unsupported data type in config file\n");
        return 1;
    }

    // Initialize the value based on the type
    Value value;
    value.type = type;

    switch (type) {
        case TYPE_INT:
            value.data.intValue = 42; // Example int value
            break;
        case TYPE_FLOAT:
            value.data.floatValue = 3.14f; // Example float value
            break;
        case TYPE_DOUBLE:
            value.data.doubleValue = 2.718281828459045; // Example double value
            break;
        default:
            break;
    }

    // Start the clock
    clock_t start = clock();

    // Process the value 1,000,000,000 times
    const int iterations = 1000000000;
    for (int i = 0; i < iterations; i++) {
        processValue(&value);
    }

    // Stop the clock
    clock_t end = clock();

    // Calculate the elapsed time in seconds
    double elapsedTime = (double)(end - start) / CLOCKS_PER_SEC;

    // Print the result and the time taken
    switch (type) {
        case TYPE_INT:
            printf("Processed int: %d\n", value.data.intValue);
            break;
        case TYPE_FLOAT:
            printf("Processed float: %f\n", value.data.floatValue);
            break;
        case TYPE_DOUBLE:
            printf("Processed double: %lf\n", value.data.doubleValue);
            break;
        default:
            break;
    }
    printf("Time taken for %d iterations: %.6f seconds\n", iterations, elapsedTime);

    return 0;
}
