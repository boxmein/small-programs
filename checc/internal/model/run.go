package model

// Run represents one check's one Run() invocation.
// A run most include a success/failure status and might include some logs.
type Run interface {
	Successful() bool
	Logs() string
}
