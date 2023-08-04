package main

import (
	"fmt"
	"os"

	"go.spiff.io/mtcl"
)

func main() {
	lexer := mtcl.NewLexer(os.Stdin)
	lexer.Name = "stdin"

	src := &mtcl.Source{}
	parser := mtcl.NewParser(lexer, src)
	src, err := parser.Parse()
	if err != nil {
		panic(err)
	}

	for i, line := range src.Lines {
		fmt.Printf("% 3d: %v\n", i+1, line)
	}

	interp := mtcl.NewInterp()
	_, err = src.Eval(interp)
	if err != nil {
		panic(err)
	}
	// for _, line := range src.Lines {

	// }
}
