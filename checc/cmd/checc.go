package main

import (
	"fmt"
	"io/ioutil"
	"os/exec"
	"strings"
)

func loadConfig() ([]string, error) {
	data, err := ioutil.ReadFile("./checc.lst")

	if err != nil {
		return []string{}, err
	}

	dataStr := string(data)

	return strings.Split(dataStr, "\n"), nil
}

func main() {
	fmt.Println("checcing")

	commands, err := loadConfig()

	if err != nil {
		fmt.Errorf("%s", err.Error())
		panic("file did not load - create a ./checc.lst file")
	}

	for _, command := range commands {
		if strings.TrimSpace(command) == "" {
			continue
		}
		commandObj := exec.Command("bash", "-c", command)
		err := commandObj.Run()
		if err != nil {
			fmt.Printf("\x1b[31merror\x1b[0m   %s\n", command)
		} else {
			fmt.Printf("\x1b[32msuccess\x1b[0m %s\n", command)
		}
	}
}
