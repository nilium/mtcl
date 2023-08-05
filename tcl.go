package mtcl

import (
	"errors"
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"

	perrors "github.com/pkg/errors"
	"go.spiff.io/mtcl/lexer"
	"go.spiff.io/mtcl/token"
)

type TokenReader interface {
	Pos() token.Location
	ReadToken() (token.Token, error)
}

type Parser struct {
	stack []TokenReader
	tok   token.Token
	last  token.Token

	subexpr bool
	depth   uint64

	ldepth  int
	LogFunc func(...any)
}

func NewParser(r TokenReader) *Parser {
	return &Parser{stack: []TokenReader{r}}
}

func (p *Parser) logf(format string, args ...any) {
	if p.LogFunc == nil {
		return
	}
	p.LogFunc(
		strings.Repeat("  ", p.ldepth) + "[" + p.lexer().Pos().String() + "]: " + fmt.Sprintf(format, args...),
	)
}

func (p *Parser) inc(sect string) func() {
	depth := p.ldepth
	p.logf("Start -> %s", sect)
	p.ldepth = depth + 1
	return func() {
		p.ldepth = depth
		p.logf("End -> %s", sect)
	}
}

type Expr interface {
	Token() token.Token
}

// A Block is a set of Commands enclosed in square brackets, such as
// `[concat a b]`.
type Block struct {
	Block []*Command `json:"block"`

	StartTok token.Token `json:"-"` // '[' token
	EndTok   token.Token `json:"-"` // ']' token
}

func (b *Block) Token() token.Token {
	return b.StartTok
}

// Access is a variable access denoted as `$var` or `${var}`.
type Access struct {
	Access string `json:"access"`

	Braced bool        `json:"braced,omitempty"` // True if variable name enclosed in curly braces.
	Tok    token.Token `json:"-"`
}

func (a *Access) Token() token.Token {
	return a.Tok
}

// A Literal is literal text that has no evaluated form other than itself.
type Literal struct {
	Literal string      `json:"literal"`
	Tok     token.Token `json:"-"`
}

func (l *Literal) Token() token.Token {
	return l.Tok
}

// A Word is any set of expressions that run together without whitespace
// breaking them up.
type Word struct {
	Word []Expr      `json:"word"`
	Tok  token.Token `json:"-"`
}

func (w *Word) Token() token.Token {
	return w.Tok
}

// A QuoteString is a string starting and ending in double quotes made up
// of Words, Accesses, and Blocks.
type QuoteString struct {
	QuoteString *Word `json:"quote_string"`
}

func (qs *QuoteString) Token() token.Token {
	return qs.QuoteString.Token()
}

// A RawString is a string wrapped in curly braces that has no interpreted
// value other than itself, minus escaped curly braces.
type RawString struct {
	RawString string      `json:"raw_string"`
	Tok       token.Token `json:"-"`
}

func (rs *RawString) Token() token.Token {
	return rs.Tok
}

type Command struct { // Expr+ Stop
	Command Expr   `json:"command"`
	Params  []Expr `json:"params"`
}

func (c *Command) Token() token.Token {
	return c.Command.Token()
}

func (p *Parser) Parse() (cmds []*Command, err error) {
	defer captureErr(&err)
	if p.tok.Kind == token.Invalid {
		p.logf("Grabbing first token (%v)", p.tok)
		p.next()
	}
	p.logf("Starting parser")
	return p.parseCommands(func(tk token.TokenKind) bool {
		return tk == token.EOF
	}), nil
}

func (p *Parser) parseCommands(end func(token.TokenKind) bool) (cmds []*Command) {
	defer p.inc("command list")()
	p.skipLeading()
	for !end(p.tok.Kind) {
		p.logf("Parsing command (%v)", p.tok)
		if cmd := p.parseCommand(); cmd != nil {
			cmds = append(cmds, cmd)
		}
	}

	return cmds

}

func (p *Parser) parseCommand() *Command {
	defer p.inc("command")()
	p.skipLeading()
	if p.tok.Kind == token.EOF {
		return nil
	}
	p.logf("Parsing name => %v", p.tok)
	cmd := &Command{
		Command: p.parseExpr(),
	}
	for !p.stop() {
		_ = p.ws() // Skip whitespace.
		p.logf("Parsing param %d => %v", len(cmd.Params), p.tok)
		param := p.parseExpr()
		if param == nil {
			break
		}
		cmd.Params = append(cmd.Params, param)
	}
	return cmd
}

func (p *Parser) parseExpr() Expr {
	defer p.inc("expr")()
	var parts []Expr

loop:
	for {
		tok := p.tok
		switch tok.Kind {
		case token.BracketOpen:
			parts = append(parts, p.parseBlock())
		case token.Word:
			parts = append(parts, p.parseString())
		case token.RawString:
			parts = append(parts, &RawString{RawString: tok.Value, Tok: tok})
			p.next()
		case token.QuotedString:
			// Pretending:
			parts = append(parts, &QuoteString{
				QuoteString: p.parseString(),
			})
		default:
			break loop
		}
	}

	switch len(parts) {
	case 0:
		return nil
	case 1:
		return parts[0]
	default:
		return &Word{Word: parts}
	}
}

func (p *Parser) parseString() *Word {
	defer p.inc("string content")()
	tok := p.expectOne(token.Word, token.QuotedString)
	word := &Word{
		Tok: tok,
	}

	if tok.Value == "" {
		return word
	}

	const (
		rEscape      = '\\'
		rBracketOpen = '['
		rDollar      = '$'
		rCurlOpen    = '{'
		rCurlClose   = '}'
	)

	pos := tok.Start
	if tok.Kind == token.QuotedString { // Skip leading quote.
		pos = pos.AdvancedBy('"', 1)
	}

	str := tok.Value
	take := func(r byte) bool {
		if str != "" && str[0] == r {
			str = str[1:]
			pos = pos.AdvancedBy(rune(r), 1)
			return true
		}
		return false
	}

	for str != "" {
		var lit strings.Builder

		p.logf("String content: %q", str)
		switch str[0] { // Non-ASCII catches down in the literal splitting below.
		case '[':
			p.logf("[expr] in string: %s", str)
			lexer := lexer.NewLexer(strings.NewReader(str))
			lexer.SetPos(pos)
			block := pushWith(p, lexer, func() *Block {
				defer p.inc("[block] in string")()
				return p.parseBlock()
			})
			word.Word = append(word.Word, block)
			diff := block.EndTok.End.Offset - block.StartTok.Start.Offset
			pos = block.EndTok.End
			str = str[diff:]
			p.logf("String content remaining after block (%v -> %v :: +%d): %q", block.StartTok.Start, block.EndTok.End, diff, str)
			continue

		case '$':
			start := pos
			take('$')

			if str == "" {
				// Treat as a literal if the $ was the only
				// remaining text.
				word.Word = append(word.Word, &Literal{Literal: "$", Tok: tok})
				return word
			}

			if strings.HasPrefix(str, `\`) {
				// Treat as literal if the $ is immediately
				// followed by a backslash.
				lit.WriteRune('$')
				break
			}

			p.logf("$access in string: %s", str)

			// Skip over $
			braced := take('{')
			if braced {
				p.logf("braced access")
				// No escapes allowed at this point, all
				// raw text for the var name.
				name, rest, ok := strings.Cut(str, "}")
				if !ok {
					exitf("expected closing } for variable %v, found none", start)
				}

				str = rest
				word.Word = append(word.Word, &Access{
					Access: name,
					Braced: braced,
					Tok:    tok,
				})

				for _, r := range name {
					pos = pos.AdvancedBy(r, utf8.RuneLen(r))
				}
				pos = pos.AdvancedBy('}', 1)
			} else {
				p.logf("bare access")
				stop := strings.IndexFunc(str, func(r rune) bool {
					switch r {
					case '[', ']', '{', '}', '$', '*', '(', ')', '`', '\'', '"', '\\':
						return true
					}
					return unicode.IsSpace(r)
				})

				name := str
				if stop > -1 {
					name, str = str[:stop], str[stop:]
				} else {
					str = ""
				}

				word.Word = append(word.Word, &Access{
					Access: name,
					Tok:    tok,
				})

				for _, r := range name {
					pos = pos.AdvancedBy(r, utf8.RuneLen(r))
				}
			}
		}

		// Default -- parse literal:
		adv := 0
		escape := false
		for i, r := range str {
			raw := r
			if r == '\\' {
				escape = true
			} else if escape {
				switch r {
				case '0':
					r = 0
				case 'n':
					r = '\n'
				case 'r':
					r = '\r'
				case 't':
					r = '\t'
				case 'a':
					r = '\a'
				case 'b':
					r = '\b'
				case 'f':
					r = '\f'
				case 'v':
					r = '\v'
				case 'e':
					r = 0x1b
				}
				escape = false
			} else if r == '[' || r == '$' {
				// Break out of literal and start lexer /
				// parser until block is consumed.
				p.logf("Encountered %q -- splitting literal", r)
				adv = i
				break
			}

			size := utf8.RuneLen(raw)
			p.logf("advance by %q :: %d", raw, size)
			pos = pos.AdvancedBy(raw, size)
			adv = i + size
			if !escape {
				lit.WriteRune(r)
			}
		}

		str = str[adv:]
		if lit.Len() > 0 {
			literal := &Literal{Literal: lit.String(), Tok: tok}
			word.Word = append(word.Word, literal)
			p.logf("Literal chunk: %q", literal.Literal)
		}
	}

	return word
}

func (p *Parser) parseBlock() *Block {
	defer p.inc("[block]")()
	start := p.expect(token.BracketOpen, "")
	p.logf("Parsing block => %v", start)
	p.skipLeading()

	end, empty := p.maybe(token.BracketClose)
	if empty {
		p.logf("Empty block expression")
		return &Block{
			StartTok: start,
			EndTok:   end,
		}
	}

	cmds := p.parseCommands(func(tk token.TokenKind) bool {
		return tk == token.BracketClose || (len(p.stack) > 1 && tk == token.EOF)
	})
	end = p.expect(token.BracketClose, "")

	return &Block{
		Block:    cmds,
		StartTok: start,
		EndTok:   end,
	}
}

// Token consumption:

func pushWith[T any](p *Parser, r TokenReader, fn func() T) T {
	done := p.inc("nested lexer")
	p.push(r)
	defer func(last token.Token) {
		p.last, p.tok = p.tok, last
		p.pop()
		done()
	}(p.tok)
	p.next()
	return fn()
}

func (p *Parser) lexer() TokenReader {
	return p.stack[len(p.stack)-1]
}

func (p *Parser) push(r TokenReader) {
	p.stack = append(p.stack, r)
}

func (p *Parser) pop() {
	top := len(p.stack) - 1
	if top == 0 {
		exitf("attempt to pop bottom lexer from stack")
		return
	}
	p.stack[top] = nil
	p.stack = p.stack[:top]
}

func (p *Parser) next() token.TokenKind {
	for {
		t, err := p.lexer().ReadToken()
		if err != nil {
			p.bailf(err, "error reading token")
		}
		if t.Kind == token.Comment {
			p.logf("Skipping comment %v", t)
			continue
		}
		p.logf("Next token: %v", t)
		p.last, p.tok = p.tok, t
		return t.Kind
	}
}

func (p *Parser) maybe(kind token.TokenKind) (match token.Token, ok bool) {
	t := p.tok
	if t.Kind == kind {
		p.logf("maybe: matched %v, advancing", kind)
		p.next()
		return t, true
	}
	return t, false
}

func (p *Parser) expectOne(kinds ...token.TokenKind) token.Token {
	t := p.tok
	if len(kinds) == 0 {
		exit(errors.New("empty input to expectOne"))
	}
	for _, k := range kinds {
		if t.Kind == k {
			p.logf("expectOne: matched %v, advancing", k)
			p.next() // Advance to next token.
			return t
		}
	}
	p.bailf(unexpectedf(t, "expected one of %v", kinds), "error parsing input")
	panic("unreachable")
}

func (p *Parser) expect(k token.TokenKind, literal string) token.Token {
	t := p.tok
	if t.Kind != k || (literal != "" && lit(t) != literal) {
		var err error
		if literal != "" {
			err = unexpectedf(t, "expected %q (%v)", k, literal)
		} else {
			err = unexpectedf(t, "expected %v", k)
		}
		p.bailf(err, "error parsing input")
		panic("unreachable")
	}
	p.logf("expect: matched %v, advancing", k)
	p.next() // Advance to next token.
	return t
}

func isLeading(kind token.TokenKind) bool {
	return kind == token.Whitespace ||
		kind == token.Stop ||
		kind == token.Comment
}

func (p *Parser) skipLeading() {
	_ = p.run(isLeading)
}

func (p *Parser) run(matching func(token.TokenKind) bool) bool {
	if !matching(p.tok.Kind) {
		return false
	}
	if p.tok.Kind == token.EOF {
		return true
	}
	p.logf("run: matched %v", p.tok.Kind)
	p.next()
	for matching(p.tok.Kind) && p.tok.Kind != token.EOF {
		p.logf("run: matched %v", p.tok.Kind)
		p.next()
	}
	return true
}

func (p *Parser) stop() bool {
	return p.run(func(kind token.TokenKind) bool {
		return kind == token.Stop || kind == token.Comment || kind == token.EOF
	})
}

func (p *Parser) ws() bool {
	return p.run(func(kind token.TokenKind) bool {
		return kind == token.Whitespace
	})
}

// lit returns the literal text of a token (i.e., its raw text, not its
// evaluated form).
func lit(t token.Token) string {
	switch t.Kind {
	case token.EOF:
		return ""
	case token.BracketOpen:
		return "["
	case token.BracketClose:
		return "]"
	default:
		return string(t.Raw)
	}
}

func wordBreak(kind token.TokenKind) bool {
	switch kind {
	case token.Comment, token.Stop, token.Whitespace, token.EOF:
		return true
	}
	return false
}

// Errors:

func (p *Parser) bailf(err error, format string, args ...any) {
	if err == nil {
		return
	}
	exitf("%v %s: %+v", p.tok.Start, fmt.Sprintf(format, args...), err)
}

type failure struct {
	err error
}

var _ error = (*failure)(nil)

func (f *failure) Unwrap() error {
	return f.err
}

func (f *failure) Error() string {
	return fmt.Sprintf("uncaught failure panic: %v", f.err)
}

func exit(err error) {
	if err == nil {
		return
	}
	panic(&failure{err: err})
}

func exitf(format string, args ...any) {
	panic(&failure{err: perrors.WithStack(fmt.Errorf(format, args...))})
}

var badErrPtr = errors.New("passed nil *error to captureErr")

func captureErr(err *error) {
	if err == nil {
		panic(badErrPtr)
	}
	switch rc := recover().(type) {
	case nil:
	case *failure:
		if *err != nil {
			*err = errors.Join(*err, rc.err)
		}
		*err = rc.err
	default:
		panic(rc)
	}
}

type UnexpectedTokenError struct {
	Token token.Token
	Err   error
}

func wrapUnexpected(tok token.Token, err error) *UnexpectedTokenError {
	return &UnexpectedTokenError{
		Token: tok,
		Err:   perrors.WithStack(err),
	}
}

func unexpected(tok token.Token, msg string) *UnexpectedTokenError {
	return wrapUnexpected(tok, errors.New(msg))
}

func unexpectedf(tok token.Token, format string, args ...any) *UnexpectedTokenError {
	return wrapUnexpected(tok, fmt.Errorf(format, args...))
}

func (e *UnexpectedTokenError) Error() string {
	raw := lit(e.Token)
	if raw == "" {
		return fmt.Sprintf("%v unexpected token %v: %v",
			e.Token.Start, e.Token.Kind, e.Err)
	}
	return fmt.Sprintf("%v unexpected token %v %q: %v",
		e.Token.Start, e.Token.Kind, raw, e.Err)
}
