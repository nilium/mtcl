package main

import (
	"encoding/json"
	"log"
	"os"

	"go.spiff.io/mtcl"
	"go.spiff.io/mtcl/lexer"
)

func main() {
	log.SetPrefix("# ")
	log.SetFlags(0)
	lexer := lexer.NewLexer(os.Stdin)
	lexer.Name = "stdin"

	parser := mtcl.NewParser(lexer)
	parser.LogFunc = log.Print
	src, err := parser.Parse()
	if err != nil {
		log.Fatalf("%+v", err)
	}

	enc := json.NewEncoder(os.Stdout)
	enc.SetIndent("", "  ")
	_ = enc.Encode(src)

	// for i, line := range src.Lines {
	// fmt.Printf("% 3d: %v\n", i+1, line)
	// }

	// interp := mtcl.NewInterp()
	// _, err = src.Eval(interp)
	// if err != nil {
	// panic(err)
	// }
	// for _, line := range src.Lines {

	// }
}
