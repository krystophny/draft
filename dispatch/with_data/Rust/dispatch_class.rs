use std::fs;
use std::time::Instant;
use std::error::Error;
use std::fmt;

// Define a trait for values
trait Value: fmt::Display {
    fn process(&mut self);
}

// Implement the trait for different types
struct IntValue {
    value: i32,
}

impl IntValue {
    fn new(initial: i32) -> Self {
        IntValue { value: initial }
    }
}

impl Value for IntValue {
    fn process(&mut self) {
        self.value += 10; // Example: add 10 to int
    }
}

impl fmt::Display for IntValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Processed int: {}", self.value)
    }
}

struct FloatValue {
    value: f32,
}

impl FloatValue {
    fn new(initial: f32) -> Self {
        FloatValue { value: initial }
    }
}

impl Value for FloatValue {
    fn process(&mut self) {
        self.value *= 2.0; // Example: multiply float by 2
    }
}

impl fmt::Display for FloatValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Processed float: {}", self.value)
    }
}

struct DoubleValue {
    value: f64,
}

impl DoubleValue {
    fn new(initial: f64) -> Self {
        DoubleValue { value: initial }
    }
}

impl Value for DoubleValue {
    fn process(&mut self) {
        self.value /= 2.0; // Example: divide double by 2
    }
}

impl fmt::Display for DoubleValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Processed double: {}", self.value)
    }
}

// Factory to create value objects
struct ValueFactory;

impl ValueFactory {
    fn create_value(type_name: &str) -> Result<Box<dyn Value>, Box<dyn Error>> {
        match type_name {
            "int" => Ok(Box::new(IntValue::new(42))),
            "float" => Ok(Box::new(FloatValue::new(3.14))),
            "double" => Ok(Box::new(DoubleValue::new(2.718281828459045))),
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
