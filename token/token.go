package token

import (
	"fmt"
	"strconv"
)

// TokenKind is an enumeration of the kinds of tokens produced by a Lexer and consumed by a Parser.
type TokenKind uint

func (t TokenKind) String() string {
	i := int(t)
	if i < 0 || len(tokenNames) <= i {
		return "invalid"
	}
	return tokenNames[t]
}

// Lex-able Token kinds encountered in mtcl.
const (
	Invalid TokenKind = iota

	EOF // !.

	// BarewordRune := ![;{}\[\]"'`] Unicode(L,M,N,P,S)

	Whitespace   // ( '\\' '\\n' )? [ \r\t]+
	Comment      // '#' { !EOL . } ( EOL | EOF )
	Word         // BarewordRune {BarewordRune}
	Stop         // ';' | '\n'
	BracketOpen  // '['
	BracketClose // ']'
	ParenOpen    // '('
	ParenClose   // ')'
	QuotedString // '"' ( Escape | [^"] )* '"'
	RawString    // '{' ( Escape | [^}] )* '}'
)

var tokenNames = []string{
	Invalid: "invalid",

	EOF: "eof",

	Whitespace: "whitespace",
	Comment:    "comment",

	Word: "word",

	Stop:         "stop",
	BracketOpen:  "open bracket",
	BracketClose: "close bracket",
	ParenOpen:    "open paren",
	ParenClose:   "close paren",

	QuotedString: `quoted string`,
	RawString:    "raw string",
}

// Token is a token with a kind and a start and end location. Start, end,
// and raw fields are considered metadata and should not be used by a parser
// except to provide information to the user. The value string may be used
// by an interpreter.
type Token struct {
	Start, End Location
	Kind       TokenKind
	Raw        []byte
	Value      string
}

func (t Token) String() string {
	if t.Raw != nil {
		return fmt.Sprintf("[%v: %v =%q]", t.Kind, t.Start, t.Raw)
	}
	return fmt.Sprintf("[%v: %v]", t.Kind, t.Start)
}

// Location describes a location in an input byte sequence.
type Location struct {
	Name   string // Name is an identifier, usually a file path, for the location.
	Offset int    // A byte offset into an input sequence. Starts at 0.
	Line   int    // A line number, delimited by '\n'. Starts at 1.
	Column int    // A column number. Starts at 1.
}

func (l Location) String() string {
	pos := strconv.Itoa(l.Line) + ":" + strconv.Itoa(l.Column) + ":" + strconv.Itoa(l.Offset)
	if l.Name != "" {
		return l.Name + ":" + pos
	}
	return pos
}

func (l Location) AdvancedBy(r rune, size int) Location {
	l.Offset += size
	l.Column++
	if r == '\n' {
		l.Line++
		l.Column = 1
	}
	return l
}
