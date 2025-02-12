# Define an enum to represent the type of data stored
@enum DataType TYPE_INT TYPE_FLOAT TYPE_DOUBLE TYPE_UNKNOWN

# Define a struct to hold the data and its type
mutable struct Value
    type::DataType
    data::Union{Int, Float32, Float64}
end

# Function to perform arithmetic on an int
process_int(value::Int) = value + 10

# Function to perform arithmetic on a float
process_float(value::Float32) = value * 2.0f0

# Function to perform arithmetic on a double
process_double(value::Float64) = value / 2.0

# Function to process the value based on its type
function process_value(value::Value)
    if value.type == TYPE_INT
        value.data = process_int(value.data)
    elseif value.type == TYPE_FLOAT
        value.data = process_float(value.data)
    elseif value.type == TYPE_DOUBLE
        value.data = process_double(value.data)
    else
        println("Unsupported type")
    end
    return value
end

# Function to read the data type from the config file
function read_config(filename::String)
    try
        type_str = readline(filename)
        if type_str == "int"
            return TYPE_INT
        elseif type_str == "float"
            return TYPE_FLOAT
        elseif type_str == "double"
            return TYPE_DOUBLE
        else
            return TYPE_UNKNOWN
        end
    catch e
        println("Failed to read config file: ", e)
        return TYPE_UNKNOWN
    end
end

function main()
    # Read the data type from the config file
    config_file = "config.txt"
    type = read_config(config_file)

    if type == TYPE_UNKNOWN
        println("Invalid or unsupported data type in config file")
        return
    end

    # Initialize the value based on the type
    value = if type == TYPE_INT
        Value(type, 42)
    elseif type == TYPE_FLOAT
        Value(type, 3.14f0)
    elseif type == TYPE_DOUBLE
        Value(type, 2.718281828459045)
    else
        error("Unsupported type")
    end

    process_value(value)

    # Start the clock
    start_time = time()

    # Process the value 1,000,000,000 times
    iterations = 1_000_000_000
    for _ in 1:iterations
        value = process_value(value)
    end

    # Stop the clock
    end_time = time()

    # Calculate the elapsed time in seconds
    elapsed_time = end_time - start_time

    # Print the result and the time taken
    println("Processed ", value.type, ": ", value.data)
    println("Time taken for ", iterations, " iterations: ", elapsed_time, " seconds")
end

# Run the program
main()
