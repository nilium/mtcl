package mtcl

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"io"
	"log"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"
)

var (
	ErrNotFound    = errors.New("not found")
	ErrVarNotFound = fmt.Errorf("variable %w", ErrNotFound)
	ErrCmdNotFound = fmt.Errorf("command %w", ErrNotFound)
)

type CmdFunc func(interp *Interp, args []string) (string, error)

func (cmd CmdFunc) Call(interp *Interp, args []string) (string, error) {
	return cmd(interp, args)
}

type Cmd interface {
	Call(interp *Interp, args []string) (string, error)
}

type Interp struct {
	root  interpScope
	scope *interpScope
}

func NewInterp() *Interp {
	tr := &Interp{
		root: interpScope{
			cmds: map[string]Cmd{
				"set": CmdFunc(func(tr *Interp, args []string) (string, error) {
					if len(args) != 2 {
						return "", fmt.Errorf("expected 'set name value', got %d args", len(args))
					}
					tr.scope.vars[args[0]] = args[1]
					return args[1], nil
				}),
				"puts": CmdFunc(func(tr *Interp, args []string) (string, error) {
					_, err := fmt.Println(strings.Join(args, "\t"))
					return "", err
				}),
				"concat": CmdFunc(func(tr *Interp, args []string) (string, error) {
					return strings.Join(args, ""), nil
				}),
				"list": CmdFunc(func(tr *Interp, args []string) (string, error) {
					return strings.Join(args, "\t"), nil
				}),
			},
			vars:   map[string]string{},
			parent: &interpScope{},
			interp: &Interp{},
		},
	}
	tr.scope = &tr.root
	return tr
}

func (tr *Interp) Cmd(name string) Cmd {
	scope := tr.scope
	for scope != nil {
		cmd, ok := scope.cmds[name]
		if ok {
			return cmd
		}
		scope = scope.parent
	}
	return nil
}

func (tr *Interp) Eval(command []Word) (string, error) {
	if len(command) == 0 {
		return "", nil
	}

	cmdName, err := command[0].Eval(tr)
	if err != nil {
		return "", fmt.Errorf("unable to evaluate command name %v: %w", command[0], err)
	}

	cmd := tr.Cmd(cmdName)
	if cmd == nil {
		return "", fmt.Errorf("%w: %q", ErrCmdNotFound, cmdName)
	}

	args := make([]string, len(command)-1)
	for i, arg := range command[1:] {
		args[i], err = arg.Eval(tr)
		if err != nil {
			nth := i + 1
			return "", fmt.Errorf("cannot call %q: cannot evaluate arg %d: %w", cmdName, nth, err)
		}
	}

	return cmd.Call(tr, args)
}

type interpScope struct {
	cmds   map[string]Cmd
	vars   map[string]string
	parent *interpScope
	interp *Interp
}

func (s *interpScope) expand(name string) (string, error) {
	for s != nil {
		val, ok := s.vars[name]
		if ok {
			return val, nil
		}
		s = s.parent
	}
	return "", fmt.Errorf("%w: %q", ErrVarNotFound, name)
}

type Word interface {
	Eval(*Interp) (string, error)
	String() string
}

type Literal string // {a curly bracket string}

func (l Literal) Eval(*Interp) (string, error) {
	return string(l), nil
}

func (l Literal) String() string { return string(l) }

type Bareword string // A_${word}_without_spaces

func (s Bareword) Eval(interp *Interp) (string, error) {
	str := string(s)
	var out strings.Builder
	out.Grow(len(str))
	escape := false
	for str != "" {
		adv := 0
		for i, r := range str {
			if escape {
				escape = false
			} else if r == rEscape {
				escape = true
				continue
			} else if r == rDollar { // Not bothering to allow ${...}
				adv = i + 1
				break
			}
			n, _ := out.WriteRune(r)
			adv = i + n
		}
		str = str[adv:]

		if str == "$" {
			out.WriteString(str)
			break
		}

		if str != "" && str[0] == rEscape {
			escape, str = true, str[1:]
			continue
		}

		if str == "" {
			break
		}

		log.Printf("EXPAND %q", str)

		var name string
		if str[0] == rCurlOpen {
			end := -1
			for i, r := range str[1:] {
				end = i
				if escape {
					escape = false
					continue
				} else if r == rEscape {
					escape = true
				} else if r == rCurlClose {
					break
				}
			}
			if end == -1 {
				out.WriteString(str)
				break
			}
			name, str = str[1:1+end], str[2+end:]
		} else if end := strings.IndexFunc(str, firstNonVar); end >= 0 {
			name, str = str[:end], str[end:]
		} else if end == -1 {
			name, str = str, ""
		}

		log.Printf("EXPAND NAME=%q REST=%q", name, str)
		if val, err := interp.scope.expand(name); err == nil && val != "" {
			out.WriteString(val)
		} else if err != nil {
			return out.String(), err
		}
	}

	return out.String(), nil
}

func firstNonVar(r rune) bool {
	return r == rEscape || unicode.IsSpace(r)
}

func (s Bareword) String() string { return string(s) }

type Bundle []Word // One or more barewords, expressions, or quoted strings run together.

func (b Bundle) Eval(interp *Interp) (string, error) {
	var out strings.Builder
	for _, chunk := range b {
		str, err := chunk.Eval(interp)
		if err != nil {
			return "", fmt.Errorf("failed to evaluate expression %q: %w", b, err)
		}
		_, _ = out.WriteString(str)
	}
	return out.String(), nil
}

func (b Bundle) String() string {
	var out strings.Builder
	for _, chunk := range b {
		_, _ = out.WriteString(chunk.String())
	}
	return out.String()
}

type Quote []Word // "quoted $string [with expressions]"

func (q Quote) Eval(tr *Interp) (string, error) {
	var out strings.Builder
	for _, exp := range q {
		str, err := exp.Eval(tr)
		if err != nil {
			return out.String(), err
		}
		_, _ = out.WriteString(str)
	}
	return out.String(), nil
}

func (q Quote) String() string {
	var out strings.Builder
	_ = out.WriteByte('"')
	for _, chunk := range q {
		_, _ = out.WriteString(chunk.String())
	}
	_ = out.WriteByte('"')
	return out.String()
}

type Expr []Line // [expression inside brackets]

func (e Expr) Eval(tr *Interp) (last string, err error) {
	for _, line := range e {
		last, err = tr.Eval([]Word(line))
		if err != nil {
			return
		}
	}
	return
}

func (e Expr) String() string {
	var out strings.Builder
	_ = out.WriteByte('[')
	for i, chunk := range e {
		if i > 0 {
			_, _ = out.WriteString("; ")
		}
		_, _ = out.WriteString(chunk.String())
	}
	_ = out.WriteByte(']')
	return out.String()
}

type Line []Word // A command or line

func (s Line) String() string {
	var out strings.Builder
	for i, chunk := range s {
		if i > 0 {
			_ = out.WriteByte(' ')
		}
		_, _ = out.WriteString(chunk.String())
	}
	return out.String()
}

type Source struct {
	Name   string
	Parent *Source
	Lines  []Line
}

func (s *Source) Eval(tr *Interp) (last string, err error) {
	for _, line := range s.Lines {
		log.Printf("Evaluating line: %v", line)
		last, err = tr.Eval([]Word(line))
		if err != nil {
			return
		}
	}
	return
}

func (s *Source) String() string {
	var out strings.Builder
	for i, chunk := range s.Lines {
		if i > 0 {
			_ = out.WriteByte('\n')
		}
		_, _ = out.WriteString(chunk.String())
	}
	return out.String()
}

type Parser struct {
	lex     *Lexer
	src     *Source
	lasttok Token

	subexpr bool
	depth   uint64
}

func NewParser(in *Lexer, out *Source) *Parser {
	return &Parser{lex: in, src: out}
}

func parseSeq(src *Source, seq []Token) (result Word, rest []Token, err error) {
	if len(seq) == 0 {
		panic("attempt to call parseSeq with empty sequence")
	}

	where := seq[0].Start
	switch seq[0].Kind {
	case TBracketOpen:
		// parse expr
		var e Expr
		var l Line
		rest = seq[1:]
		for len(rest) > 0 {
			log.Print(rest)
			switch rest[0].Kind {
			case TBracketClose:
				if len(l) > 0 {
					e = append(e, l)
				}
				return e, rest[1:], nil
			case TSemicolon, TComment, TNewline:
				if len(l) > 0 {
					e = append(e, l)
				}
				l = nil
			}
			result, rest, err = parseSeq(src, rest)
			if err != nil {
				return nil, seq, fmt.Errorf("error parsing [expr] at %v: %w", where, err)
			}
			l = append(l, result)
		}
		if len(rest) == 0 {
			return nil, seq, fmt.Errorf("missing closing ] for [expr] at %v", where)
		}
		return e, rest[1:], nil
	case TSimpleString:
		return Literal(seq[0].Value), seq[1:], nil
	case TWord:
		return Bareword(seq[0].Value), seq[1:], nil
	case TString:
		var q Quote
		str := seq[0].Value
		for str != "" {
			adv := 0
			skip := 0
			escape := false
			for i, r := range str {
				if r == rEscape {
					escape = true
				} else if escape {
					escape = false
				} else if r == rBracketOpen {
					adv, skip = i, 1
					break
				}
				adv = i + utf8.RuneLen(r)
			}

			if adv > 0 {
				log.Printf("substring: %q", str[:adv])
				q = append(q, Bareword(str[:adv]))
			}
			str = str[adv+skip:]

			if str == "" {
				break
			}

			log.Printf("subexpr: [%s", str)
			lexer := NewLexer(strings.NewReader(str))
			subsrc := &Source{Name: src.Name, Parent: src}
			parser := NewParser(lexer, subsrc)
			parser.subexpr = true

			subsrc, err = parser.Parse()
			if err != nil {
				return nil, seq, fmt.Errorf("unable to parse quoted [subexpression] in string at %v: %w", where, err)
			}

			q = append(q, Expr(subsrc.Lines))
			for n, line := range subsrc.Lines {
				log.Printf("%q -> %2d %v", seq[0].Value, n, line)
			}
			str = str[parser.lasttok.End.Offset:]
			log.Printf("remainder: %q", str)
		}

		return q, seq[1:], nil
	default:
		panic(fmt.Errorf("invalid token type for parseSeq: %v", seq[0].Kind))
	}
}

func (p *Parser) Parse() (*Source, error) {
	var line Line
	var seq []Token

	endseq := func() error {
		var out []Word
		input := seq
		for len(input) > 0 {
			w, rest, err := parseSeq(p.src, input)
			if err != nil {
				return err
			}
			input, out = rest, append(out, w)
		}
		switch len(out) {
		case 0:
		case 1:
			line = append(line, out[0])
		default:
			line = append(line, Bundle(out))
		}
		seq = seq[:0]
		return nil
	}

	endline := func() error {
		if err := endseq(); err != nil {
			return err
		}
		if len(line) > 0 {
			p.src.Lines = append(p.src.Lines, line)
			line = nil
		}
		return nil
	}

	for {
		// TODO: Going to need to rewrite this as a proper parser
		// since this doesn't handle [expr]s correctly.
		tok, err := p.lex.ReadToken()
		if tok.Kind == TEOF || errors.Is(err, io.EOF) {
			return p.src, endline()
		} else if err != nil {
			return p.src, err
		}
		p.lasttok = tok

		switch tok.Kind {
		case TWhitespace, TContinuation:
			if err := endseq(); err != nil {
				return p.src, err
			}
			continue
		case TComment, TNewline, TSemicolon:
			if p.depth == 0 {
				if err := endline(); err != nil {
					return p.src, err
				}
				continue
			}
			tok.Kind = TSemicolon
		case TBracketOpen:
			p.depth++
		case TBracketClose:
			if !p.subexpr {
				break
			}
			if p.subexpr && p.depth == 0 {
				return p.src, endline()
			}
			if p.depth == 0 {
				return p.src, fmt.Errorf("invalid closing bracket at %v", tok.Start)
			}
			p.depth--
		}

		seq = append(seq, tok)
	}
}

// ErrUnexpectedEOF is returned by the Lexer when EOF is encountered mid-token where a valid token
// cannot be cut off.
var ErrUnexpectedEOF = errors.New("unexpected EOF")

const eof rune = -1

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
	tEmpty = TokenKind(iota)

	TEOF // !.

	// BarewordRune := ![;{}\[\]"'`] Unicode(L,M,N,P,S)

	TWhitespace   // [ \n\r\t]+
	TComment      // '//' { !EOL . } ( EOL | EOF )
	TWord         // BarewordRune {BarewordRune}
	TNewline      // '\n'
	TContinuation // '\\' '\n' -- to be treated same as whitespace.
	TSemicolon    // ';'
	TBracketOpen  // '['
	TBracketClose // ']'

	// A string is any double-quoted string
	TString       // '"' ( Escape | [^"] )* '"'
	TSimpleString // '{' ( Escape | [^}] )* '}'
)

var tokenNames = []string{
	tEmpty: "empty",

	TEOF: "EOF",

	TWhitespace: "whitespace",
	TComment:    "comment",

	TWord: "word",

	TNewline:      "newline",
	TContinuation: "continuation",
	TSemicolon:    "semicolon",
	TBracketOpen:  "open bracket",
	TBracketClose: "close bracket",

	TString:       `"string"`,
	TSimpleString: "{string}",
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

func (l Location) add(r rune, size int) Location {
	l.Offset += size
	l.Column++
	if r == rNewline {
		l.Line++
		l.Column = 1
	}
	return l
}

type scanResult struct {
	r    rune
	size int
	err  error
}

// NamedReader is an optional interface that an io.Reader can implement to provide a name for its
// data source.
type NamedReader interface {
	io.Reader

	// Name returns a non-empty string identifying the reader's data source. This may be a file,
	// URL, resource ID, or some other thing. If the returned string is empty, it will be
	// treated as unnamed.
	Name() string
}

var noToken Token

// Special lexer runes
const (
	rNewline      = '\n'
	rSentinel     = ';'
	rCurlOpen     = '{'
	rCurlClose    = '}'
	rBracketOpen  = '['
	rBracketClose = ']'
	rDoubleQuote  = '"'
	rComment      = '#'
	rEscape       = '\\'
	rDollar       = '$'
)

// Lexer takes an input sequence of runes and constructs Tokens from it.
type Lexer struct {
	// Name is the name of the token source currently being lexed. It is used to identify the
	// source of a location by name. It is not necessarily a filename, but usually is.
	//
	// If the scanner provided to the Lexer implements NamedScanner, the scanner's name takes
	// priority.
	Name string

	scanner io.RuneReader

	pending  bool
	lastScan scanResult
	lastPos  Location

	startPos Location
	pos      Location

	next consumerFunc

	buf    bytes.Buffer
	strbuf bytes.Buffer
}

// NewLexer allocates a new Lexer that reads runes from r.
func NewLexer(r io.Reader) *Lexer {
	rr := runeReader(r)

	le := &Lexer{
		scanner: rr,
		pos:     Location{Line: 1, Column: 1},
	}
	return le
}

type nameRuneReader struct {
	*bufio.Reader
	namefn func() string
}

func (n nameRuneReader) Name() string {
	return n.namefn()
}

func runeReader(r io.Reader) io.RuneReader {
	switch r := r.(type) {
	case io.RuneReader:
		return r
	case NamedReader:
		return nameRuneReader{bufio.NewReader(r), r.Name}
	default:
		return bufio.NewReader(r)
	}
}

// ReadToken returns a token or an error. If EOF occurs, a TEOF token is returned without an error,
// and will be returned by all subsequent calls to ReadToken.
func (l *Lexer) ReadToken() (tok Token, err error) {
	l.reset()
	if l.next == nil {
		l.next = l.lexSegment
	}

	if l.pos == (Location{Line: 1, Column: 1}) {
		l.pos.Name = l.posName()
	}
	l.startPos = l.scanPos()

	var r rune
	for {
		r, err = l.readRune()
		if err != nil {
			return tok, err
		}

		tok, l.next, err = l.next(r)
		if err != nil || tok.Kind != tEmpty {
			return tok, err
		}
	}
}

type convertFunc func(Token) (Token, error)

func (l *Lexer) valueToken(kind TokenKind, convert convertFunc) (tok Token, err error) {
	tok = l.token(kind, true)
	if convert != nil {
		tok, err = convert(tok)
	}
	return tok, err
}

var (
	rawContinuation = []byte{'\\', '\n'}
	rawNewline      = []byte{'\n'}
	rawBracketOpen  = []byte{'['}
	rawBracketClose = []byte{']'}
	rawSemicolon    = []byte{';'}
)

func (l *Lexer) token(kind TokenKind, takeBuffer bool) Token {
	var txt []byte
	if buflen := l.buf.Len(); buflen > 0 && takeBuffer {
		txt = make([]byte, buflen)
		copy(txt, l.buf.Bytes())
	} else if takeBuffer {
		txt = []byte{}
	} else {
		switch kind {
		case TNewline:
			txt = rawNewline
		case TBracketOpen:
			txt = rawBracketOpen
		case TBracketClose:
			txt = rawBracketClose
		case TSemicolon:
			txt = rawSemicolon
		case TContinuation:
			txt = rawContinuation
		}
	}
	l.buf.Reset()
	tok := Token{
		Start: l.startPos,
		End:   l.scanPos(),
		Kind:  kind,
		Raw:   txt,
	}
	if takeBuffer {
		tok.Value = l.strbuf.String()
		l.strbuf.Reset()
	}
	return tok
}

func (l *Lexer) readRune() (r rune, err error) {
	const invalid rune = '\uFFFD'

	if l.pending {
		l.pending = false
		return l.lastScan.r, l.lastScan.err
	}

	var size int
	l.pos.Name = l.posName()
	r, size, err = l.scanner.ReadRune()
	if err == io.EOF {
		r, size, err = eof, 0, nil
	}
	res := scanResult{r: r, size: size, err: err}
	l.lastScan, l.lastPos = res, l.pos
	if size > 0 {
		l.pos = l.pos.add(r, size)
	}

	if r == invalid && err == nil {
		err = fmt.Errorf("invalid UTF-8 at %v", l.pos)
	}

	return
}

func (l *Lexer) posName() string {
	if named, ok := l.scanner.(NamedReader); ok {
		if name := named.Name(); name != "" {
			return name
		}
	}
	return l.Name
}

// unread takes the last-scanned rune and tells the lexer to return it on the next call to readRune.
// This can be used to walk back a single readRune call.
func (l *Lexer) unread() {
	if l.pending {
		panic("unread() called with pending rune")
	}
	l.pending = true
}

func (l *Lexer) reset() {
	l.buf.Reset()
	l.strbuf.Reset()
}

func (l *Lexer) buffer(raw, str rune) {
	if raw >= 0 {
		l.buf.WriteRune(raw)
	}
	if str >= 0 {
		l.strbuf.WriteRune(str)
	}
}

func (l *Lexer) scanPos() Location {
	if l.pending {
		return l.lastPos
	}
	return l.pos
}

// Rune cases

var barewordTables = []*unicode.RangeTable{
	unicode.L, // Letters
	unicode.M, // Marks
	unicode.N, // Numbers
	unicode.P, // Punctuation
	unicode.S, // Symbols
}

func isSpace(r rune) bool {
	return r != rNewline && unicode.IsSpace(r)
}

func isWordSep(r rune) bool {
	return r == eof ||
		r == rSentinel ||
		r == rBracketOpen ||
		r == rBracketClose ||
		unicode.IsSpace(r)
}

// Branches

type consumerFunc func(rune) (Token, consumerFunc, error)

func (l *Lexer) lexSpace(r rune, next consumerFunc) consumerFunc {
	var spaceConsumer consumerFunc
	l.buffer(r, -1)
	spaceConsumer = func(r rune) (Token, consumerFunc, error) {
		if !isSpace(r) {
			l.unread()
			return l.token(TWhitespace, true), next, nil
		}
		l.buffer(r, -1)
		return noToken, spaceConsumer, nil
	}
	return spaceConsumer
}

func (l *Lexer) lexComment(next consumerFunc) consumerFunc {
	var commentConsumer consumerFunc
	l.buffer(rComment, -1)
	commentConsumer = func(r rune) (Token, consumerFunc, error) {
		if r == rNewline || r == eof {
			l.unread()
			return l.token(TComment, true), next, nil
		}
		l.buffer(r, r)
		return noToken, commentConsumer, nil
	}
	return commentConsumer
}

func (l *Lexer) lexSegment(r rune) (Token, consumerFunc, error) {
	switch {
	// EOF
	case r == eof:
		return l.token(TEOF, false), l.lexSegment, nil

	case r == rNewline:
		return l.token(TNewline, false), l.lexSegment, nil

	// Whitespace (word separator)
	case isSpace(r):
		return noToken, l.lexSpace(r, l.lexSegment), nil

	// Semicolon (sentinel)
	case r == rSentinel:
		return l.token(TSemicolon, false), l.lexSegment, nil

	case r == rEscape:
		l.buffer(r, -1)
		return noToken, l.lexEscape, nil

	// {String}
	case r == rCurlOpen:
		return l.lexCurlString(r)

	// "String"
	case r == rDoubleQuote:
		return l.lexString(r)

	// [ ... ]
	case r == rBracketOpen:
		return l.token(TBracketOpen, false), l.lexSegment, nil
	case r == rBracketClose:
		return l.token(TBracketClose, false), l.lexSegment, nil

	// Comment (comments may only be found at the start of a segment,
	// #s are valid inside words)
	case r == rComment:
		return noToken, l.lexComment(l.lexSegment), nil

	// Word -- IsGraphic includes spaces, but we're ignoring that because
	// we capture spaces above.
	case unicode.IsOneOf(barewordTables, r):
		return l.lexWord(r)
	}

	return noToken, nil, fmt.Errorf("unexpected character %q at %v", r, l.pos)
}

func (l *Lexer) lexWord(r rune) (Token, consumerFunc, error) {
	if r == eof {
		return l.token(TWord, true), l.lexSegment, nil
	} else if isWordSep(r) {
		l.unread()
		return l.token(TWord, true), l.lexSegment, nil
	} else if !unicode.IsOneOf(barewordTables, r) {
		l.unread()
		return l.token(TWord, true), l.lexSegment, nil
	}

	l.buffer(r, r)
	return noToken, l.lexWord, nil
}

type stringLexer struct {
	*Lexer

	stack  []rune
	escape bool
}

func (ls *stringLexer) lex(r rune) (Token, consumerFunc, error) {
	if r == eof {
		return noToken, ls.lex, fmt.Errorf("expected closing \" of string: %w", ErrUnexpectedEOF)
	}

	if ls.escape {
		ls.buffer(r, r)
		ls.escape = false
		return noToken, ls.lex, nil
	}

	if depth := len(ls.stack) - 1; depth >= 0 && ls.stack[depth] == r {
		ls.buffer(r, r)
		ls.stack = ls.stack[:depth]
		return noToken, ls.lex, nil
	}

	switch r {
	case rEscape:
		ls.escape = true
		ls.buffer(r, r)
		return noToken, ls.lex, nil

	case rBracketOpen:
		ls.stack = append(ls.stack, ']')

	case rDoubleQuote:
		if len(ls.stack) > 0 {
			ls.stack = append(ls.stack, r)
			ls.buffer(r, r)
			return noToken, ls.lex, nil
		}

		ls.buffer(r, -1)
		return ls.token(TString, true), ls.Lexer.lexSegment, nil
	}

	ls.buffer(r, r)

	return noToken, ls.lex, nil
}

func (l *Lexer) lexString(r rune) (Token, consumerFunc, error) {
	l.buffer(r, -1)
	return noToken, (&stringLexer{Lexer: l}).lex, nil
}

type curlStringLexer struct {
	*Lexer

	escape bool
}

func (ls *curlStringLexer) lex(r rune) (Token, consumerFunc, error) {
	if r == eof {
		return noToken, ls.lex, fmt.Errorf("expected closing } of string: %w", ErrUnexpectedEOF)
	}

	if r == rEscape {
		ls.escape = true
		ls.buffer(r, r)
		return noToken, ls.lex, nil
	}

	if ls.escape {
		ls.escape = false
		ls.buffer(r, r)
		return noToken, ls.lex, nil
	}

	if r == rCurlClose {
		ls.buffer(r, -1)
		return ls.token(TSimpleString, true), ls.Lexer.lexSegment, nil
	}

	ls.buffer(r, r)
	return noToken, ls.lex, nil
}

func (l *Lexer) lexCurlString(r rune) (Token, consumerFunc, error) {
	l.buffer(r, -1)
	return noToken, (&curlStringLexer{Lexer: l}).lex, nil
}

func (l *Lexer) lexEscape(r rune) (Token, consumerFunc, error) {
	if r == eof {
		return noToken, l.lexEscape, fmt.Errorf("expected newline or word: %w", ErrUnexpectedEOF)
	}

	if r == rNewline {
		return l.token(TContinuation, false), l.lexSegment, nil
	}

	l.buffer(-1, '\\')
	l.unread()
	return noToken, l.lexWord, nil
}
