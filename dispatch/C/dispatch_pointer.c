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

// Define function pointer types for processing different data types
typedef int (*IntProcessor)(int);
typedef float (*FloatProcessor)(float);
typedef double (*DoubleProcessor)(double);

// Define a struct to hold processing functions for each type
typedef struct {
    IntProcessor processInt;
    FloatProcessor processFloat;
    DoubleProcessor processDouble;
} Processors;

// Define a struct that holds the union, type, and print function
typedef struct {
    DataType type;
    Data data;
    void (*print)(const Data*);
    Data (*process)(Data*, const Processors*);
} Value;

// Processing functions
int processInt(int value) {
    return value + 10;
}

float processFloat(float value) {
    return value * 2.0f;
}

double processDouble(double value) {
    return value / 2.0;
}

// Print functions for each type
void printInt(const Data* data) {
    printf("Processed int: %d\n", data->intValue);
}

void printFloat(const Data* data) {
    printf("Processed float: %f\n", data->floatValue);
}

void printDouble(const Data* data) {
    printf("Processed double: %lf\n", data->doubleValue);
}

// Processing functions that work with Data union
Data processIntData(Data* data, const Processors* procs) {
    data->intValue = procs->processInt(data->intValue);
}

Data processFloatData(Data* data, const Processors* procs) {
    data->floatValue = procs->processFloat(data->floatValue);
}

Data processDoubleData(Data* data, const Processors* procs) {
    data->doubleValue = procs->processDouble(data->doubleValue);
}

// Function to initialize a Value struct based on type
Value createValue(DataType type) {
    Value value = {.type = type};

    switch (type) {
        case TYPE_INT:
            value.data.intValue = 42;
            value.print = printInt;
            value.process = processIntData;
            break;
        case TYPE_FLOAT:
            value.data.floatValue = 3.14f;
            value.print = printFloat;
            value.process = processFloatData;
            break;
        case TYPE_DOUBLE:
            value.data.doubleValue = 2.718281828459045;
            value.print = printDouble;
            value.process = processDoubleData;
            break;
        default:
            value.print = NULL;
            value.process = NULL;
            break;
    }

    return value;
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

    if (strcmp(type, "int") == 0) return TYPE_INT;
    if (strcmp(type, "float") == 0) return TYPE_FLOAT;
    if (strcmp(type, "double") == 0) return TYPE_DOUBLE;
    return TYPE_UNKNOWN;
}

int main() {
    // Initialize processors
    Processors procs = {
        .processInt = processInt,
        .processFloat = processFloat,
        .processDouble = processDouble
    };

    // Read the data type from the config file
    const char* configFile = "config.txt";
    DataType type = readConfig(configFile);
    if (type == TYPE_UNKNOWN) {
        fprintf(stderr, "Invalid or unsupported data type in config file\n");
        return 1;
    }

    // Initialize the value based on the type
    Value value = createValue(type);
    if (!value.process || !value.print) {
        fprintf(stderr, "Failed to initialize value processors\n");
        return 1;
    }

    // Start the clock
    clock_t start = clock();

    // Process the value 1,000,000,000 times
    const int iterations = 1000000000;
    for (int i = 0; i < iterations; i++) {
        value.process(&value.data, &procs);
    }

    // Stop the clock
    clock_t end = clock();
    double elapsedTime = (double)(end - start) / CLOCKS_PER_SEC;

    // Print the result and the time taken
    value.print(&value.data);
    printf("Time taken for %d iterations: %.6f seconds\n", iterations, elapsedTime);

    return 0;
}
