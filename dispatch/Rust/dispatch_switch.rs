use std::fs;
use std::time::Instant;
use std::error::Error;
use std::fmt;

// Define an enum to represent different types of values
#[derive(Clone)]
enum Value {
    Int(i32),
    Float(f32),
    Double(f64),
}

impl Value {
    // Process the value based on its type
    fn process(&mut self) {
        match self {
            Value::Int(v) => *v += 10,       // Example: add 10 to int
            Value::Float(v) => *v *= 2.0,    // Example: multiply float by 2
            Value::Double(v) => *v /= 2.0,   // Example: divide double by 2
        }
    }
}

// Implement Display for pretty-printing
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(v) => write!(f, "Processed int: {}", v),
            Value::Float(v) => write!(f, "Processed float: {}", v),
            Value::Double(v) => write!(f, "Processed double: {}", v),
        }
    }
}

// Factory to create value objects
struct ValueFactory;

impl ValueFactory {
    fn create_value(type_name: &str) -> Result<Value, Box<dyn Error>> {
        match type_name {
            "int" => Ok(Value::Int(42)),          // Default int value
            "float" => Ok(Value::Float(3.14)),    // Default float value
            "double" => Ok(Value::Double(2.718281828459045)), // Default double value
            _ => Err(format!("Unsupported type: {}", type_name).into()),
        }
    }
}

// Configuration reader
struct Config;

impl Config {
    fn read_type(filename: &str) -> Result<String, Box<dyn Error>> {
        let content = fs::read_to_string(filename)?;
        let type_name = content.trim().to_string();
        Ok(type_name)
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    // Read configuration
    let config_file = "config.txt";
    let type_name = Config::read_type(config_file)?;

    // Create value object based on configuration
    let mut value = ValueFactory::create_value(&type_name)?;

    // Start timing
    let iterations = 1_000_000_000;
    let start = Instant::now();

    // Process the value multiple times
    for _ in 0..iterations {
        value.process();
    }

    // Stop timing
    let elapsed = start.elapsed();

    // Print results
    println!("{}", value);
    println!(
        "Time taken for {} iterations: {:.2} seconds",
        iterations,
        elapsed.as_secs_f64()
    );

    Ok(())
}
