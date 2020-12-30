package config

import (
	"github.com/boxmein/small-programs/checc/internal/checks/command_check"
	"github.com/boxmein/small-programs/checc/internal/model"
	"io/ioutil"
	"strings"
)

func parseCheccListFile(filename string) ([]string, error) {
	data, err := ioutil.ReadFile(filename)

	if err != nil {
		return []string{}, err
	}

	dataStr := string(data)

	return strings.Split(dataStr, "\n"), nil
}

// GetConfig loads the configuration of "checc" from a file
func GetConfig(filePath string) (*model.Config, error) {
	var config model.Config

	checclist, err := parseCheccListFile(filePath)

	if err != nil {
		return nil, err
	}

	for _, command := range checclist {
		config.Checks = append(config.Checks, command_check.CommandCheck{
			Command: command,
		})
	}

	return &config, nil
}
