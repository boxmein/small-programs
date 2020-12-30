package model

// Represents a single check to be performed by the checc binary.
type Check interface {
	// Run() is to be called once to perform a check
	Run() Run
}

// Represents the full configuration of the checc run.
type Config struct {
	// Array of checks to perform in this checc run
	Checks []Check
}
