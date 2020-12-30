package main

import (
	"fmt"
	"github.com/boxmein/small-programs/checc/internal/config"
	"os"
)

const DEFAULT_PATH = "./.checc.lst"

func main() {
	fmt.Println("checcing")

	filename := DEFAULT_PATH
	if len(os.Args) == 2 {
		filename = os.Args[1]
	}

	config, err := config.GetConfig(filename)

	if err != nil {
		_ = fmt.Errorf("%s", err.Error())
		panic("file did not load - create a ./.checc.lst file")
	}

	for _, check := range config.Checks {
		result := check.Run()
		if result.Successful() {
			fmt.Printf("\x1b[32msuccess\x1b[0m %s\n", check)
		} else {
			fmt.Printf("\x1b[31merror\x1b[0m   %s\n", check)
			fmt.Printf("         Logs: %s\n", result.Logs())
		}
	}
}
