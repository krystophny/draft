package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"time"
)

// DataType enum
type DataType int

const (
	TypeInt DataType = iota
	TypeFloat
	TypeDouble
	TypeUnknown
)

// Value struct holds the type and data
type Value struct {
	dataType    DataType
	doubleValue float64
}

// Process functions for each type
func processInt(value float64) float64 {
	return value + 10.0 // Example: add 10 to the int
}

func processFloat(value float64) float64 {
	return value * 2.0 // Example: multiply the float by 2
}

func processDouble(value float64) float64 {
	return value / 2.0 // Example: divide the double by 2
}

// processValue processes the value based on its type using switch
func processValue(v *Value) {
	switch v.dataType {
	case TypeInt:
		v.doubleValue = processInt(v.doubleValue)
	case TypeFloat:
		v.doubleValue = processFloat(v.doubleValue)
	case TypeDouble:
		v.doubleValue = processDouble(v.doubleValue)
	default:
		fmt.Println("Unsupported type")
	}
}

// readConfig reads the data type from config file
func readConfig(filename string) (DataType, error) {
	file, err := os.Open(filename)
	if err != nil {
		return TypeUnknown, fmt.Errorf("failed to open config file: %w", err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	if scanner.Scan() {
		typeStr := strings.TrimSpace(scanner.Text())
		switch typeStr {
		case "int":
			return TypeInt, nil
		case "float":
			return TypeFloat, nil
		case "double":
			return TypeDouble, nil
		default:
			return TypeUnknown, fmt.Errorf("unknown type: %s", typeStr)
		}
	}

	if err := scanner.Err(); err != nil {
		return TypeUnknown, err
	}

	return TypeUnknown, fmt.Errorf("empty config file")
}

func main() {
	// Read the data type from the config file
	configFile := "config.txt"
	dataType, err := readConfig(configFile)

	if err != nil || dataType == TypeUnknown {
		fmt.Fprintf(os.Stderr, "Invalid or unsupported data type in config file\n")
		os.Exit(1)
	}

	// Initialize the value based on the type
	value := Value{dataType: dataType}

	switch dataType {
	case TypeInt:
		value.doubleValue = 42.0 // Example int value
	case TypeFloat:
		value.doubleValue = 3.14 // Example float value
	case TypeDouble:
		value.doubleValue = 2.718281828459045 // Example double value
	}

	// Start the clock
	iterations := 1_000_000_000
	start := time.Now()

	// Process the value iterations times
	for i := 0; i < iterations; i++ {
		processValue(&value)
	}

	// Stop the clock
	elapsed := time.Since(start)

	// Print the result and the time taken
	switch dataType {
	case TypeInt:
		fmt.Printf("Processed int: %.0f\n", value.doubleValue)
	case TypeFloat:
		fmt.Printf("Processed float: %f\n", value.doubleValue)
	case TypeDouble:
		fmt.Printf("Processed double: %f\n", value.doubleValue)
	}
	fmt.Printf("Time taken for %d iterations: %.6f seconds\n", iterations, elapsed.Seconds())
}
