package lexer

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"io"
	"unicode"

	. "go.spiff.io/mtcl/token"
)

// ErrUnexpectedEOF is returned by the Lexer when EOF is encountered mid-token where a valid token
// cannot be cut off.
var ErrUnexpectedEOF = errors.New("unexpected EOF")

const eof rune = -1

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
	rSemicolon    = ';'
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

func (l *Lexer) SetPos(pos Location) error {
	if l.pos != (Location{Line: 1, Column: 1}) {
		return errors.New("cannot set position after scanning has started")
	}
	l.Name, l.startPos, l.pos, l.lastPos = pos.Name, pos, pos, pos
	return nil
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

func (l *Lexer) Pos() Location {
	return l.pos
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
		if err != nil || tok.Kind != Invalid {
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
	rawBracketOpen  = []byte{rBracketOpen}
	rawBracketClose = []byte{rBracketClose}
	rawNewline      = []byte{rNewline}   // Stop
	rawSemicolon    = []byte{rSemicolon} // Stop
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
		case BracketOpen:
			txt = rawBracketOpen
		case BracketClose:
			txt = rawBracketClose
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
		l.pos = l.pos.AdvancedBy(r, size)
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
		r == rSemicolon ||
		r == rBracketOpen ||
		r == rBracketClose ||
		r == rDoubleQuote ||
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
			return l.token(Whitespace, true), next, nil
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
			return l.token(Comment, true), next, nil
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
		return l.token(EOF, false), l.lexSegment, nil

	// Newline (stop)
	case r == rNewline:
		l.buffer(r, -1)
		return l.token(Stop, true), l.lexSegment, nil

	// Whitespace (word separator)
	case isSpace(r):
		return noToken, l.lexSpace(r, l.lexSegment), nil

	// Semicolon (stop)
	case r == rSemicolon:
		l.buffer(r, -1)
		return l.token(Stop, true), l.lexSegment, nil

	// Escape -- may lead into continuation or word.
	case r == rEscape:
		l.buffer(r, -1)
		return noToken, l.lexEscape, nil

	// {String}
	case r == rCurlOpen:
		return l.lexCurlyString(r)

	// "String"
	case r == rDoubleQuote:
		return l.lexString(r)

	// [ ... ]
	case r == rBracketOpen:
		return l.token(BracketOpen, false), l.lexSegment, nil
	case r == rBracketClose:
		return l.token(BracketClose, false), l.lexSegment, nil

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
		return l.token(Word, true), l.lexSegment, nil
	} else if isWordSep(r) {
		l.unread()
		return l.token(Word, true), l.lexSegment, nil
	} else if !unicode.IsOneOf(barewordTables, r) {
		l.unread()
		return l.token(Word, true), l.lexSegment, nil
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
		return noToken, ls.lex, fmt.Errorf(`expected closing "quoted" of string: %w`, ErrUnexpectedEOF)
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
		ls.stack = append(ls.stack, rBracketClose)

	case rDoubleQuote:
		if len(ls.stack) > 0 {
			ls.stack = append(ls.stack, r)
			ls.buffer(r, r)
			return noToken, ls.lex, nil
		}

		ls.buffer(r, -1)
		return ls.token(QuotedString, true), ls.Lexer.lexSegment, nil
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
	depth  uint64
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

	if r == rCurlOpen {
		ls.depth++
	} else if r == rCurlClose && ls.depth == 0 {
		ls.buffer(r, -1)
		return ls.token(RawString, true), ls.Lexer.lexSegment, nil
	} else if r == rCurlClose {
		ls.depth--
	}

	ls.buffer(r, r)
	return noToken, ls.lex, nil
}

func (l *Lexer) lexCurlyString(r rune) (Token, consumerFunc, error) {
	l.buffer(r, -1)
	return noToken, (&curlStringLexer{Lexer: l}).lex, nil
}

func (l *Lexer) lexEscape(r rune) (Token, consumerFunc, error) {
	if r == eof {
		return noToken, l.lexEscape, fmt.Errorf("expected newline or word: %w", ErrUnexpectedEOF)
	}

	if r == rNewline {
		return noToken, l.lexSpace(r, l.lexSegment), nil
	}

	l.buffer(-1, rEscape)
	l.unread()
	return noToken, l.lexWord, nil
}
