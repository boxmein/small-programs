package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"strings"
)

func loadConfig(filename string) ([]string, error) {
	data, err := ioutil.ReadFile(filename)

	if err != nil {
		return []string{}, err
	}

	dataStr := string(data)

	return strings.Split(dataStr, "\n"), nil
}

func main() {
	fmt.Println("checcing")

	filename := "./.checc.lst"
	if len(os.Args) == 2 {
		filename = os.Args[1]
	}

	commands, err := loadConfig(filename)

	if err != nil {
		_ = fmt.Errorf("%s", err.Error())
		panic("file did not load - create a ./.checc.lst file")
	}

	for _, command := range commands {
		if strings.TrimSpace(command) == "" {
			continue
		}
		commandObj := exec.Command("bash", "-c", command)
		// NOTE: _ is the command's output
		_, err := commandObj.CombinedOutput()
		if err != nil {
			fmt.Printf("\x1b[31merror\x1b[0m   %s\n", command)
		} else {
			fmt.Printf("\x1b[32msuccess\x1b[0m %s\n", command)
		}
	}
}
