package mtcl

import (
	"errors"
	"fmt"
	"io"
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
	r    TokenReader
	tok  token.Token
	last token.Token

	subexpr bool
	depth   uint64

	ldepth  int
	LogFunc func(...any)
}

func NewParser(r TokenReader) *Parser {
	return &Parser{r: r}
}

func (p *Parser) logf(format string, args ...any) {
	if p.LogFunc == nil {
		return
	}
	p.LogFunc(
		strings.Repeat("  ", p.ldepth) + "[" + p.lexer().Pos().String() + "]: " + fmt.Sprintf(format, args...),
	)
}

func (p *Parser) inc(sect string, args ...any) func() {
	depth := p.ldepth
	desc := fmt.Sprintf(sect, args...)
	p.logf("Start -> %s", desc)
	p.ldepth = depth + 1
	return func() {
		p.ldepth = depth
		p.logf("End -> %s", desc)
	}
}

type Expr interface {
	// Token returns the token from the first part of the Expr. The
	// concrete type of the Expr may contain additional tokens identifying
	// the remaining tokens making up the Expr.
	Token() token.Token
	// String returns the Expr as a string that is equivalent in value
	// to the source tokens (with differences allowed for comments
	// and whitespace).
	String() string
}

func strs[E Expr](exprs ...E) []string {
	vals := make([]string, len(exprs))
	for i, e := range exprs {
		vals[i] = e.String()
	}
	return vals
}

// A ListExpr is a sequence of expressions that results in a list. This is
// functionally equivalent to calling [list exprs...] in the source script,
// assuming [list] is provided..
type ListExpr struct {
	List []Expr `json:"list"`

	StartTok token.Token `json:"-"` // '(' token
	EndTok   token.Token `json:"-"` // ')' token
}

func (l *ListExpr) String() string {
	return "(" + strings.Join(strs(l.List...), " ") + ")"
}

func (l *ListExpr) Token() token.Token {
	return l.StartTok
}

// A Block is a set of Commands enclosed in square brackets, such as
// `[concat a b]`.
type Block struct {
	Block []*Command `json:"block"`

	StartTok token.Token `json:"-"` // '[' token
	EndTok   token.Token `json:"-"` // ']' token
}

func (b *Block) String() string {
	return "[" + strings.Join(strs(b.Block...), "; ") + "]"
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

func (a *Access) String() string {
	if a.Braced {
		return "${" + a.Access + "}"
	}
	return "$" + a.Access
}

func (a *Access) Token() token.Token {
	return a.Tok
}

// A Literal is literal text that has no evaluated form other than itself.
type Literal struct {
	Literal string      `json:"literal"`
	Tok     token.Token `json:"-"`
}

func (l *Literal) String() string {
	return l.Literal
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

func (w *Word) String() string {
	return strings.Join(strs(w.Word...), "")
}

func (w *Word) Token() token.Token {
	return w.Tok
}

// A QuoteString is a string starting and ending in double quotes made up
// of Words, Accesses, and Blocks.
type QuoteString struct {
	QuoteString *Word `json:"quote_string"`
}

func (qs *QuoteString) String() string {
	return string(qs.Token().Raw)
}

func (qs *QuoteString) Token() token.Token {
	return qs.QuoteString.Token()
}

// A RawString is a string wrapped in curly braces that has no interpreted
// value other than itself, minus escaped curly braces.
type RawString struct {
	RawString string      `json:"raw_string"`
	Tok       token.Token `json:"-"`

	// Exprs contains parsed commands from the RawString. May be empty if the string cannot be parsed as a set of expressions.
	Cmds []*Command `json:"cmds"`
}

func (rs *RawString) String() string {
	return string(rs.Token().Raw)
}

func (rs *RawString) Token() token.Token {
	return rs.Tok
}

type Command struct { // Expr+ Stop
	Command Expr   `json:"command"`
	Params  []Expr `json:"params"`
}

func (c *Command) String() string {
	if len(c.Params) == 0 {
		return c.Command.String()
	}
	return c.Command.String() + " " + strings.Join(strs(c.Params...), " ")
}

func (c *Command) Token() token.Token {
	return c.Command.Token()
}

func (p *Parser) ParseCommand() (cmd *Command, err error) {
	defer captureErr(&err)
	if p.tok.Kind == token.Invalid {
		p.logf("Grabbing first token (%v)", p.tok)
		p.next()
	}
	p.logf("Starting parser")
	cmds := p.parseCommands(1, func(tk token.TokenKind) bool {
		return tk == token.EOF
	})
	if len(cmds) == 0 {
		return nil, io.EOF
	}
	return cmds[0], nil
}

func (p *Parser) Parse() (cmds []*Command, err error) {
	defer captureErr(&err)
	if p.tok.Kind == token.Invalid {
		p.logf("Grabbing first token (%v)", p.tok)
		p.next()
	}
	p.logf("Starting parser")
	return p.parseCommands(-1, func(tk token.TokenKind) bool {
		return tk == token.EOF
	}), nil
}

func (p *Parser) parseCommands(limit int, end func(token.TokenKind) bool) (cmds []*Command) {
	defer p.inc("command list")()
	p.skipLeading()
	for !end(p.tok.Kind) && (limit == -1 || limit > len(cmds)) {
		p.logf("Parsing command (%v)", p.tok)
		if cmd := p.parseCommand(); cmd != nil {
			p.logf("Parsed command: %#+ v", cmd)
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
	p.stopsCommand()
	cmdExpr := p.parseExpr()
	params := p.parseExprSeq(nil, p.stopCommand, p.ws)
	cmd := &Command{
		Command: cmdExpr,
		Params:  params,
	}
	return cmd
}

func (p *Parser) parseExprSeq(exprs []Expr, stop, skip func() bool) []Expr {
	for {
		_ = skip() // Skip.
		if stop() {
			return exprs
		}
		p.logf("Parsing possible expr %d => %v", len(exprs), p.tok)
		param := p.parseExpr()
		if param == nil {
			return exprs
		}
		exprs = append(exprs, param)
	}
}

func (p *Parser) parseExpr() Expr {
	defer p.inc("expr")()
	var parts []Expr

loop:
	for {
		switch p.tok.Kind {
		case token.ParenOpen:
			parts = append(parts, p.parseList())
		case token.BracketOpen:
			parts = append(parts, p.parseBlock())
		case token.Word:
			word := p.parseString()
			if len(word.Word) == 1 {
				parts = append(parts, word.Word[0])
			} else {
				parts = append(parts, word)
			}
		case token.RawString:
			parts = append(parts, p.parseRawString())
		case token.QuotedString:
			// Pretending:
			parts = append(parts, &QuoteString{QuoteString: p.parseString()})
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

func (p *Parser) parseRawString() *RawString {
	tok := p.expect(token.RawString, "")
	lexer := lexer.NewLexer(strings.NewReader(tok.Value))
	lexer.SetPos(tok.Start)
	cmds := pushWith(p, lexer, func() []*Command {
		cmds, err := p.Parse()
		if err != nil {
			return nil
		}
		if cmds == nil {
			cmds = []*Command{}
		}
		return cmds
	})

	rs := &RawString{
		RawString: tok.Value,
		Tok:       tok,
		Cmds:      cmds,
	}

	return rs
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
					case '[', ']', '{', '}', '$', '(', ')', '`', '\'', '"', '\\':
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

func (p *Parser) parseList() *ListExpr {
	defer p.inc("(list)")()
	start := p.expect(token.ParenOpen, "")
	p.logf("Parsing list => %v", start)
	p.skipLeading()

	end, empty := p.maybe(token.ParenClose)
	if empty {
		p.logf("Empty list expression")
		return &ListExpr{
			StartTok: start,
			EndTok:   end,
		}
	}

	list := &ListExpr{
		List:     p.parseExprSeq(nil, p.stopList, p.skipList),
		StartTok: start,
	}
	list.EndTok = p.expect(token.ParenClose, "")

	return list
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

	cmds := p.parseCommands(-1, func(tk token.TokenKind) bool {
		return tk == token.BracketClose
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
	defer func(prev TokenReader, last token.Token) {
		p.r, p.last, p.tok = prev, p.tok, last
		done()
	}(p.lexer(), p.tok)
	p.r = r
	p.next()
	return fn()
}

func (p *Parser) lexer() TokenReader {
	return p.r
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
	defer p.inc("expect(%v, %q)", k, literal)()
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
	defer p.inc("skipLeading()")()
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

func (p *Parser) skipList() (ok bool) {
	defer p.inc("skipList()")()
	return p.run(func(kind token.TokenKind) bool {
		return kind == token.Whitespace ||
			kind == token.Stop ||
			kind == token.Comment
	})
}

func (p *Parser) stopList() bool {
	defer p.inc("stopList()")()
	kind := p.tok.Kind
	return kind == token.ParenClose || kind == token.EOF
}

func (p *Parser) stopCommand() bool {
	defer p.inc("stopCommand()")()
	kind := p.tok.Kind
	return kind == token.Stop || kind == token.Comment || kind == token.EOF
}

func (p *Parser) stopsCommand() {
	defer p.inc("stopsCommand()")()
	p.run(func(kind token.TokenKind) bool {
		return kind == token.Stop || kind == token.Comment || kind == token.EOF
	})
}

func (p *Parser) ws() bool {
	defer p.inc("ws()")()
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
	exitf("%v %s: %w", p.tok.Start, fmt.Sprintf(format, args...), err)
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

func (e *UnexpectedTokenError) Unwrap() error {
	return e.Err
}
