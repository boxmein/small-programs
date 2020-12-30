package command_check

import (
	"fmt"
	"github.com/boxmein/small-programs/checc/internal/model"
	"os/exec"
)

// @implements model.Check
type CommandCheck struct {
	command string
}

// @implements model.Run
type commandRun struct {
	success bool
	output  string
}

func (c commandRun) Successful() bool {
	return c.success
}

func (c commandRun) Output() string {
	return c.output
}

func (c CommandCheck) Run() model.Run {
	commandObj := exec.Command("bash", "-c", c.command)
	output, err := commandObj.CombinedOutput()

	return commandRun{
		success: err == nil,
		output:  string(output),
	}
}

func (c CommandCheck) String() string {
	return fmt.Sprintf("Command: %s", c.command)
}
