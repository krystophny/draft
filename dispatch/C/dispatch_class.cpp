#include <iostream>
#include <fstream>
#include <memory>
#include <string>
#include <chrono>
#include <stdexcept>

// Abstract base class for values
class Value {
public:
    virtual ~Value() = default;
    virtual void process() = 0;
    virtual void print() const = 0;
};

// Concrete implementation for int values
class IntValue : public Value {
private:
    int value;

public:
    IntValue(int initial = 42) : value(initial) {}

    void process() override {
        value = value + 10;  // Example: add 10 to int
    }

    void print() const override {
        std::cout << "Processed int: " << value << std::endl;
    }
};

// Concrete implementation for float values
class FloatValue : public Value {
private:
    float value;

public:
    FloatValue(float initial = 3.14f) : value(initial) {}

    void process() override {
        value = value * 2.0f;  // Example: multiply float by 2
    }

    void print() const override {
        std::cout << "Processed float: " << value << std::endl;
    }
};

// Concrete implementation for double values
class DoubleValue : public Value {
private:
    double value;

public:
    DoubleValue(double initial = 2.718281828459045) : value(initial) {}

    void process() override {
        value = value / 2.0;  // Example: divide double by 2
    }

    void print() const override {
        std::cout << "Processed double: " << value << std::endl;
    }
};

// Factory class to create appropriate value objects
class ValueFactory {
public:
    static std::unique_ptr<Value> createValue(const std::string& type) {
        if (type == "int") return std::make_unique<IntValue>();
        if (type == "float") return std::make_unique<FloatValue>();
        if (type == "double") return std::make_unique<DoubleValue>();
        throw std::runtime_error("Unsupported type: " + type);
    }
};

// Configuration reader class
class Config {
public:
    static std::string readType(const std::string& filename) {
        std::ifstream file(filename);
        if (!file) {
            throw std::runtime_error("Failed to open config file: " + filename);
        }

        std::string type;
        if (!(file >> type)) {
            throw std::runtime_error("Failed to read type from config file");
        }

        return type;
    }
};

int main() {
    try {
        // Read configuration
        const std::string configFile = "config.txt";
        std::string type = Config::readType(configFile);

        // Create value object based on configuration
        auto value = ValueFactory::createValue(type);

        // Start timing
        const int iterations = 1000000000;
        auto start = std::chrono::high_resolution_clock::now();

        // Process the value multiple times
        for (int i = 0; i < iterations; i++) {
            value->process();
        }

        // Stop timing
        auto end = std::chrono::high_resolution_clock::now();
        std::chrono::duration<double> elapsed = end - start;

        // Print results
        value->print();
        std::cout << "Time taken for " << iterations
                  << " iterations: " << elapsed.count()
                  << " seconds" << std::endl;

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }

    return 0;
}
