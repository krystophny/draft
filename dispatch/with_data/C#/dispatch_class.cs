using System;
using System.IO;
using System.Diagnostics;

// Interface for values
interface IValue {
    void Process();
    void Print();
}

// Implementation for int values
class IntValue : IValue {
    private int _value;

    public IntValue(int initial = 42) {
        _value = initial;
    }

    public void Process() {
        _value += 10; // Example: add 10 to int
    }

    public void Print() {
        Console.WriteLine($"Processed int: {_value}");
    }
}

// Implementation for float values
class FloatValue : IValue {
    private float _value;

    public FloatValue(float initial = 3.14f) {
        _value = initial;
    }

    public void Process() {
        _value *= 2.0f; // Example: multiply float by 2
    }

    public void Print() {
        Console.WriteLine($"Processed float: {_value}");
    }
}

// Implementation for double values
class DoubleValue : IValue {
    private double _value;

    public DoubleValue(double initial = 2.718281828459045) {
        _value = initial;
    }

    public void Process() {
        _value /= 2.0; // Example: divide double by 2
    }

    public void Print() {
        Console.WriteLine($"Processed double: {_value}");
    }
}

// Factory class to create appropriate value objects
static class ValueFactory {
    public static IValue CreateValue(string type) => type switch {
        "int" => new IntValue(),
        "float" => new FloatValue(),
        "double" => new DoubleValue(),
        _ => throw new ArgumentException($"Unsupported type: {type}")
    };
}

// Configuration reader class
static class Config {
    public static string ReadType(string filename) {
        if (!File.Exists(filename)) {
            throw new FileNotFoundException($"Failed to open config file: {filename}");
        }

        string type = File.ReadAllText(filename).Trim();
        return type;
    }
}

class Program {
    static void Main() {
        try {
            // Read configuration
            const string configFile = "config.txt";
            string type = Config.ReadType(configFile);

            // Create value object based on configuration
            IValue value = ValueFactory.CreateValue(type);

            // Start timing
            const int iterations = 1000000000;
            var stopwatch = Stopwatch.StartNew();

            // Process the value multiple times
            for (int i = 0; i < iterations; i++) {
                value.Process();
            }

            // Stop timing
            stopwatch.Stop();

            // Print results
            value.Print();
            Console.WriteLine($"Time taken for {iterations} iterations: {stopwatch.Elapsed.TotalSeconds:F6} seconds");

        } catch (Exception e) {
            Console.Error.WriteLine($"Error: {e.Message}");
            Environment.Exit(1);
        }
    }
}
