package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"time"
)

// Value interface for polymorphic dispatch
type Value interface {
	Process()
	Print()
}

// IntValue implementation
type IntValue struct {
	value int32
}

func NewIntValue(initial int32) *IntValue {
	return &IntValue{value: initial}
}

func (v *IntValue) Process() {
	v.value += 10 // Example: add 10 to int
}

func (v *IntValue) Print() {
	fmt.Printf("Processed int: %d\n", v.value)
}

// FloatValue implementation
type FloatValue struct {
	value float32
}

func NewFloatValue(initial float32) *FloatValue {
	return &FloatValue{value: initial}
}

func (v *FloatValue) Process() {
	v.value *= 2.0 // Example: multiply float by 2
}

func (v *FloatValue) Print() {
	fmt.Printf("Processed float: %f\n", v.value)
}

// DoubleValue implementation
type DoubleValue struct {
	value float64
}

func NewDoubleValue(initial float64) *DoubleValue {
	return &DoubleValue{value: initial}
}

func (v *DoubleValue) Process() {
	v.value /= 2.0 // Example: divide double by 2
}

func (v *DoubleValue) Print() {
	fmt.Printf("Processed double: %f\n", v.value)
}

// ValueFactory creates value objects based on type string
func CreateValue(typeName string) (Value, error) {
	switch typeName {
	case "int":
		return NewIntValue(42), nil
	case "float":
		return NewFloatValue(3.14), nil
	case "double":
		return NewDoubleValue(2.718281828459045), nil
	default:
		return nil, fmt.Errorf("unsupported type: %s", typeName)
	}
}

// ReadType reads the type from config file
func ReadType(filename string) (string, error) {
	file, err := os.Open(filename)
	if err != nil {
		return "", fmt.Errorf("failed to open config file: %w", err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	if scanner.Scan() {
		return strings.TrimSpace(scanner.Text()), nil
	}

	if err := scanner.Err(); err != nil {
		return "", err
	}

	return "", fmt.Errorf("failed to read type from config file")
}

func main() {
	// Read configuration
	configFile := "config.txt"
	typeName, err := ReadType(configFile)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	// Create value object based on configuration
	value, err := CreateValue(typeName)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}

	// Start timing
	iterations := 1_000_000_000
	start := time.Now()

	// Process the value multiple times
	for i := 0; i < iterations; i++ {
		value.Process()
	}

	// Stop timing
	elapsed := time.Since(start)

	// Print results
	value.Print()
	fmt.Printf("Time taken for %d iterations: %.6f seconds\n", iterations, elapsed.Seconds())
}
