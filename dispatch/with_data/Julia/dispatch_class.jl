using Dates

# Abstract base type for values
abstract type Value end

# Concrete implementation for Int values
mutable struct IntValue <: Value
    value::Int
    IntValue(initial::Int = 42) = new(initial)
end

function process!(v::IntValue)
    v.value += 10
end

function print_value(v::IntValue)
    println("Processed int: ", v.value)
end

# Concrete implementation for Float values
mutable struct FloatValue <: Value
    value::Float32
    FloatValue(initial::Float32 = 3.14f0) = new(initial)
end

function process!(v::FloatValue)
    v.value *= 2.0f0
end

function print_value(v::FloatValue)
    println("Processed float: ", v.value)
end

# Concrete implementation for Double values
mutable struct DoubleValue <: Value
    value::Float64
    DoubleValue(initial::Float64 = 2.718281828459045) = new(initial)
end

function process!(v::DoubleValue)
    v.value /= 2.0
end

function print_value(v::DoubleValue)
    println("Processed double: ", v.value)
end

# Factory function to create value objects
function create_value(type::String)::Value
    if type == "int"
        return IntValue()
    elseif type == "float"
        return FloatValue()
    elseif type == "double"
        return DoubleValue()
    else
        error("Unsupported type: " * type)
    end
end

# Read type from configuration file
function read_type(filename::String)::String
    open(filename, "r") do file
        line = readline(file)
        return strip(line)
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
        iterations = 1_000_000_000
        start_time = now()

        # Process the value multiple times
        for _ in 1:iterations
            process!(value)
        end

        # Stop timing
        elapsed_time = now() - start_time

        # Print results
        print_value(value)
        println("Time taken for ", iterations, " iterations: ", elapsed_time)
    catch e
        println("Error: ", e)
        return 1
    end
    return 0
end

main()
