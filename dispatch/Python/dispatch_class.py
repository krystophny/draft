import time
import numba
from numba import int32, float32, float64
from numba.experimental import jitclass
from abc import ABC, abstractmethod

# Define the structure of each class with Numba-supported types
int_spec = [("value", int32)]
float_spec = [("value", float32)]
double_spec = [("value", float64)]

# Abstract base class (not needed for jitclass)
class Value(ABC):
    @abstractmethod
    def process(self, iterations: int):
        pass

    @abstractmethod
    def print(self):
        pass

# Concrete implementation for int values
@jitclass(int_spec)
class IntValue:
    def __init__(self, initial=42):
        self.value = initial

    def process(self, iterations: int):
        for _ in range(iterations):
            self.value += 10

    def print(self):
        print(f"Processed int: {self.value}")

# Concrete implementation for float values
@jitclass(float_spec)
class FloatValue:
    def __init__(self, initial=3.14):
        self.value = initial

    def process(self, iterations: int):
        for _ in range(iterations):
            self.value *= 2.0

    def print(self):
        print(f"Processed float: {self.value}")

# Concrete implementation for double values
@jitclass(double_spec)
class DoubleValue:
    def __init__(self, initial=2.718281828459045):
        self.value = initial

    def process(self, iterations: int):
        for _ in range(iterations):
            self.value /= 2.0

    def print(self):
        print(f"Processed double: {self.value}")

# Factory class to create appropriate value objects
class ValueFactory:
    @staticmethod
    def create_value(value_type: str):
        if value_type == "int":
            return IntValue()
        elif value_type == "float":
            return FloatValue()
        elif value_type == "double":
            return DoubleValue()
        else:
            raise ValueError(f"Unsupported type: {value_type}")

# Configuration reader class
class Config:
    @staticmethod
    def read_type(filename: str) -> str:
        with open(filename, 'r') as file:
            value_type = file.readline().strip()
        if not value_type:
            raise ValueError("Failed to read type from config file")
        return value_type

# Main execution
if __name__ == "__main__":
    try:
        # Read configuration
        config_file = "config.txt"
        value_type = Config.read_type(config_file)

        # Create value object based on configuration
        value = ValueFactory.create_value(value_type)

        # Start timing
        iterations = 1_000_000_000
        start_time = time.time()

        # Process the value multiple times
        value.process(iterations)

        # Stop timing
        elapsed_time = time.time() - start_time

        # Print results
        value.print()
        print(f"Time taken for {iterations} iterations: {elapsed_time:.6f} seconds")

    except Exception as e:
        print(f"Error: {e}")
