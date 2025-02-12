using Printf
using Dates

# Abstract base type for values
abstract type Value end

# Concrete implementation for Int values
mutable struct IntValue <: Value
    value::Int
    IntValue(initial::Int = 42) = new(initial)
end

function process(v::IntValue)
    v.value += 10  # Example: add 10 to int
end

function print_value(v::IntValue)
    @printf("Processed int: %d\n", v.value)
end

function clone(v::IntValue)
    return IntValue(v.value)
end

# Concrete implementation for Float values
mutable struct FloatValue <: Value
    value::Float32
    FloatValue(initial::Float32 = 3.14f0) = new(initial)
end

function process(v::FloatValue)
    v.value *= 2.0f0  # Example: multiply float by 2
end

function print_value(v::FloatValue)
    @printf("Processed float: %.2f\n", v.value)
end

function clone(v::FloatValue)
    return FloatValue(v.value)
end

# Concrete implementation for Double values
mutable struct DoubleValue <: Value
    value::Float64
    DoubleValue(initial::Float64 = 2.718281828459045) = new(initial)
end

function process(v::DoubleValue)
    v.value /= 2.0  # Example: divide double by 2
end

function print_value(v::DoubleValue)
    @printf("Processed double: %.15f\n", v.value)
end

function clone(v::DoubleValue)
    return DoubleValue(v.value)
end

# Factory function to create appropriate value objects
function create_value(type::String)
    if type == "int"
        return IntValue()
    elseif type == "float"
        return FloatValue()
    elseif type == "double"
        return DoubleValue()
    else
        throw(ArgumentError("Unsupported type: $type"))
    end
end

# Configuration reader function
function read_type(filename::String)
    try
        open(filename, "r") do file
            type = readline(file)
            return type
        end
    catch e
        throw(ArgumentError("Failed to open or read config file: $filename"))
    end
end

function main()
    try
        # Read configuration
        config_file = "config.txt"
        type = read_type(config_file)

        # Create value object based on configuration
        value = create_value(type)

        # Start timing
        iterations = 1000000000
        start_time = now()

        # Process the value multiple times
        for _ in 1:iterations
            process(value)
        end

        # Stop timing
        end_time = now()
        elapsed = end_time - start_time

        # Print results
        print_value(value)
        @printf("Time taken for %d iterations: %.6f seconds\n", iterations, elapsed.value / 1000)

    catch e
        println("Error: ", e.msg)
        return 1
    end

    return 0
end

main()
