package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"os"

	"go.spiff.io/mtcl"
	"go.spiff.io/mtcl/lexer"
)

func main() {
	log.SetPrefix("# ")
	log.SetFlags(0)

	dump := flag.Bool("j", false, "dump JSON")
	logged := flag.Bool("v", false, "write debug logs")
	parseOnly := flag.Bool("p", false, "parse without eval")
	flag.Parse()

	lexer := lexer.NewLexer(os.Stdin)
	lexer.Name = "stdin"

	parser := mtcl.NewParser(lexer)
	if *logged {
		parser.LogFunc = log.Print
	}
	src, err := parser.Parse()
	if err != nil {
		log.Fatalf("%+v", err)
	}

	if *dump {
		enc := json.NewEncoder(os.Stdout)
		enc.SetIndent("", "  ")
		_ = enc.Encode(src)
	}

	if *parseOnly && *dump {
		return
	}

	interp := mtcl.NewInterp()
	for _, cmd := range src {
		line := cmd.Token().Start.Line
		if *parseOnly {
			fmt.Printf("%03d: %v\n", line, cmd)
			continue
		}
		vals, err := interp.Do(cmd)
		if err != nil {
			fmt.Printf("%03d: %v # ERR => %v\n", line, cmd, err)
		} else {
			fmt.Printf("%03d: %v # => %v\n", line, cmd, vals)
		}
	}
}
