using System;
using System.Diagnostics;
using System.IO;

// Enum to represent the type of data stored in the struct
enum DataType {
    INT,
    FLOAT,
    DOUBLE,
    UNKNOWN
}

// Struct to hold different types of data
struct Value {
    public DataType Type;
    public double Data;
}

class Program {
    static int ProcessInt(double value) {
        return (int)(value + 10.0); // Example: add 10 to the int
    }

    static float ProcessFloat(double value) {
        return (float)(value * 2.0); // Example: multiply the float by 2
    }

    static double ProcessDouble(double value) {
        return value / 2.0; // Example: divide the double by 2
    }

    static void ProcessValue(ref Value value) {
        switch (value.Type) {
            case DataType.INT:
                value.Data = ProcessInt(value.Data);
                break;
            case DataType.FLOAT:
                value.Data = ProcessFloat(value.Data);
                break;
            case DataType.DOUBLE:
                value.Data = ProcessDouble(value.Data);
                break;
            default:
                Console.WriteLine("Unsupported type");
                break;
        }
    }

    static DataType ReadConfig(string filename) {
        if (!File.Exists(filename)) {
            Console.WriteLine("Failed to open config file");
            return DataType.UNKNOWN;
        }

        string type = File.ReadAllText(filename).Trim();

        return type switch {
            "int" => DataType.INT,
            "float" => DataType.FLOAT,
            "double" => DataType.DOUBLE,
            _ => DataType.UNKNOWN
        };
    }

    static void Main() {
        const string configFile = "config.txt";
        DataType type = ReadConfig(configFile);

        if (type == DataType.UNKNOWN) {
            Console.WriteLine("Invalid or unsupported data type in config file");
            return;
        }

        Value value = new Value { Type = type };

        switch (type) {
            case DataType.INT:
                value.Data = 42.0; // Example int value
                break;
            case DataType.FLOAT:
                value.Data = 3.14; // Example float value
                break;
            case DataType.DOUBLE:
                value.Data = 2.718281828459045; // Example double value
                break;
        }

        Stopwatch stopwatch = Stopwatch.StartNew();

        const int iterations = 1000000000;
        for (int i = 0; i < iterations; i++) {
            ProcessValue(ref value);
        }

        stopwatch.Stop();

        switch (type) {
            case DataType.INT:
                Console.WriteLine($"Processed int: {(int)value.Data}");
                break;
            case DataType.FLOAT:
                Console.WriteLine($"Processed float: {(float)value.Data:F6}");
                break;
            case DataType.DOUBLE:
                Console.WriteLine($"Processed double: {value.Data:F6}");
                break;
        }

        Console.WriteLine($"Time taken for {iterations} iterations: {stopwatch.Elapsed.TotalSeconds:F6} seconds");
    }
}
