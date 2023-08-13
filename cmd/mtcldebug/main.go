package main

import (
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"os/exec"

	"go.spiff.io/mtcl"
	"go.spiff.io/mtcl/lexer"
)

var newline = []byte("\n")

func main() {
	log.SetPrefix("# ")
	log.SetFlags(0)

	dump := flag.Bool("j", false, "dump JSON")
	logged := flag.Bool("v", false, "write debug logs")
	parseOnly := flag.Bool("p", false, "parse without eval")
	flag.Parse()

	interp := mtcl.NewInterp()
	interp.SetPrelude(mtcl.Prelude())

	interp.Bind(mtcl.UnknownCmd, mtcl.CmdFunc(func(tcl *mtcl.Interp, args mtcl.Values) (mtcl.Values, error) {
		argv := mtcl.Strings(args)
		cmd := exec.Command(argv[0], argv[1:]...)
		cmd.Stderr = os.Stderr
		out, err := cmd.Output()
		out = bytes.TrimSpace(out)
		results := make(mtcl.Values, 0, bytes.Count(out, newline))
		for {
			line, rest, ok := bytes.Cut(out, newline)
			line = bytes.TrimSpace(line)
			if len(line) > 0 {
				results = append(results, mtcl.String(line))
			}
			out = rest
			if !ok {
				break
			}
		}

		return results, err
	}))

	inputs := []string{"-"}
	if flag.NArg() > 0 {
		inputs = flag.Args()
	}

	for _, input := range inputs {
		r, err := file(input)
		if err != nil {
			log.Fatalf("Error reading input %v: %v", input, err)
		}

		lexer := lexer.NewLexer(r)
		lexer.Name = "stdin"

		parser := mtcl.NewParser(lexer)
		if *logged {
			parser.LogFunc = log.Print
		}

		for {
			cmd, err := parser.ParseCommand()
			if err == io.EOF {
				break
			} else if err != nil {
				log.Printf("PARSE ERR: %+v", err)
			}

			if *dump {
				enc := json.NewEncoder(os.Stdout)
				enc.SetIndent("", "  ")
				_ = enc.Encode(cmd)
			}

			if *parseOnly && *dump {
				break
			}

			line := cmd.Token().Start.Line
			fmt.Printf("%03d: %v\n", line, cmd)
			if *parseOnly {
				continue
			}
			vals, err := interp.Do(cmd)
			if err != nil {
				fmt.Printf("ERR: %v\n", err)
			} else {
				fmt.Printf("=> %v\n", vals)
			}
		}

		r.Close()
	}
}

func file(name string) (io.ReadCloser, error) {
	if name == "-" {
		return io.NopCloser(os.Stdin), nil
	}
	return os.Open(name)
}
