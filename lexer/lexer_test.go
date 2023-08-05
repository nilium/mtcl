package lexer

import (
	"bytes"
	"fmt"
	"testing"

	. "go.spiff.io/mtcl/token"
)

func TestInvalidTokenName(t *testing.T) {
	const want = "invalid"
	const tok32 = TokenKind(0xffffffff)
	if got := tok32.String(); got != want {
		t.Errorf("Token(%08x) = %q; want %q", tok32, got, want)
	}
}

func wordCase(word string) tokenCase {
	return tokenCase{
		Token: Token{
			Kind:  Word,
			Raw:   []byte(word),
			Value: word,
		},
	}
}

func reader(s string) *bytes.Buffer {
	return bytes.NewBuffer([]byte(s))
}

func requireEOF(t *testing.T, b *bytes.Buffer) {
	if b.Len() > 0 {
		t.Fatalf("expected EOF; %d bytes remaining", b.Len())
	}
}

func checkToken(t *testing.T, prefix string, got, want Token) {
	t.Helper()
	if got.Kind != want.Kind {
		t.Errorf("%stok.Kind = %v; want %v", prefix, got.Kind, want.Kind)
	}
	if want.Start.Column > 0 && got.Start != want.Start {
		t.Errorf("%stok.Start = %#v; want %#v", prefix, got.Start, want.Start)
	}
	if want.End.Column > 0 && got.End != want.End {
		t.Errorf("%stok.End = %#v; want %#v", prefix, got.End, want.End)
	}
	if want.Raw == nil {
		// Skip check
	} else if want.Raw != nil && got.Raw == nil {
		t.Errorf("%stok.Raw = nil; want %q", prefix, want.Raw)
	} else if !bytes.Equal(got.Raw, want.Raw) {
		t.Errorf("%stok.Raw = %q; want %q", prefix, got.Raw, want.Raw)
	}

	if want.Kind == Comment && want.Value == "" {
		// Ignore value.
	} else if want.Value != got.Value {
		t.Errorf("%stok.Value = %T(%#v); want %T(%#v)", prefix,
			got.Value, got.Value,
			want.Value, want.Value)
	}

	if t.Failed() {
		t.Logf("%stok.Raw = %q", prefix, got.Raw)
	}
}

type tokenCase struct {
	Token
	Err bool
}

// Common punctuation tokens
var (
	_error        = tokenCase{Err: true}
	_ws           = tokenCase{Token: Token{Kind: Whitespace}}
	_nl           = tokenCase{Token: Token{Kind: Stop, Raw: []byte{'\n'}}}
	_eof          = tokenCase{Token: Token{Kind: EOF}}
	_semicolon    = tokenCase{Token: Token{Kind: Stop, Raw: []byte{';'}}}
	_bracketopen  = tokenCase{Token: Token{Kind: BracketOpen, Raw: []byte{'['}}}
	_bracketclose = tokenCase{Token: Token{Kind: BracketClose, Raw: []byte{']'}}}
	_comment      = tokenCase{Token: Token{Kind: Comment}}
)

type tokenSeq []tokenCase

func (seq tokenSeq) Run(t *testing.T, input string) {
	t.Helper()
	buf := reader(input)
	lex := NewLexer(buf)
	lex.Name = "test.tcl"
	if seq.RunWithLexer(t, lex) {
		requireEOF(t, buf)
	}
}

func (seq tokenSeq) RunWithLexer(t *testing.T, lex *Lexer) bool {
	t.Helper()
	for i, want := range seq {
		prefix := fmt.Sprintf("%d: ", i+1)

		tok, err := lex.ReadToken()
		if want.Err && err == nil {
			t.Errorf("%sgot error = nil; want error", prefix)
		} else if !want.Err && err != nil {
			t.Errorf("%sgot error = %v; want %v", prefix, err, want.Kind)
		}

		if want.Err && err != nil {
			return false
		}

		checkToken(t, prefix, tok, want.Token)

		if t.Failed() {
			return false
		}
	}
	return true
}

type tokenSeqTest struct {
	Name  string
	Input string
	Seq   tokenSeq
}

func (tt *tokenSeqTest) Run(t *testing.T) {
	t.Run(tt.Name, func(t *testing.T) {
		tt.Seq.Run(t, tt.Input)
	})
}

func TestComment(t *testing.T) {
	tokenSeq{
		{Token: Token{Kind: Comment, Raw: []byte("#")}},
		{Token: Token{Kind: Stop}},
		{Token: Token{Kind: Comment, Raw: []byte("# foo bar baz"), Value: " foo bar baz"}},
		{Token: Token{Kind: Stop}},
		{Token: Token{Kind: Comment, Raw: []byte("#foo bar baz"), Value: "foo bar baz"}},
		_eof,
	}.Run(t, "#\n# foo bar baz\n#foo bar baz")
}

func TestBareword(t *testing.T) {
	tokenSeq{
		{Token: Token{
			Kind:  Whitespace,
			Start: Location{Name: "test.tcl", Offset: 0, Line: 1, Column: 1},
			End:   Location{Name: "test.tcl", Offset: 1, Line: 1, Column: 2},
		}},
		{Token: Token{
			Kind:  Word,
			Raw:   []byte("\\.foo$bar#baz=quux"),
			Start: Location{Name: "test.tcl", Offset: 1, Line: 1, Column: 2},
			End:   Location{Name: "test.tcl", Offset: 19, Line: 1, Column: 20},
			Value: "\\.foo$bar#baz=quux",
		}},
		_nl,
		_ws,
		{Token: Token{
			Kind:  Word,
			Raw:   []byte("10.0.0.0/8"),
			Start: Location{Name: "test.tcl", Offset: 21, Line: 2, Column: 2},
			End:   Location{Name: "test.tcl", Offset: 31, Line: 2, Column: 12},
			Value: "10.0.0.0/8",
		}},
		_ws,
		{Token: Token{
			Kind:  Comment,
			Raw:   []byte("# #f + -; // foo"),
			Start: Location{Name: "test.tcl", Offset: 32, Line: 2, Column: 13},
			End:   Location{Name: "test.tcl", Offset: 48, Line: 2, Column: 29},
			Value: " #f + -; // foo",
		}},
		_nl,
		wordCase("word"),
		_ws,
		_ws, // Continuation
		wordCase("continued"), _nl,
		_nl,
		wordCase("foo"),
		_bracketopen,
		wordCase("bar"),
		_ws,
		wordCase("baz"),
		_bracketclose,
		wordCase("wub"),
		_ws,
		wordCase("+f"),
		_ws, wordCase("1/1f"),
		_ws, wordCase("1nq"),
		_ws, wordCase("1s1.s"),
		_ws, _comment,
		_eof,
	}.Run(t, "\t\\.foo$bar#baz=quux\n"+
		"\t10.0.0.0/8 # #f + -; // foo\n"+
		"word \\\n"+
		"continued\n"+
		"\n"+
		"foo[bar baz]wub +f 1/1f 1nq 1s1.s #foo /foo/ #\"foo\"",
	)
}

func TestWhitespace(t *testing.T) {
	tokenSeq{
		{
			Token: Token{
				Start: Location{Name: "test.tcl", Column: 1, Line: 1, Offset: 0},
				End:   Location{Name: "test.tcl", Column: 2, Line: 1, Offset: 1},
				Kind:  Whitespace,
				Raw:   []byte(" "),
			},
		},
		{
			Token: Token{
				Start: Location{Name: "test.tcl", Column: 2, Line: 1, Offset: 1},
				End:   Location{Name: "test.tcl", Column: 1, Line: 2, Offset: 2},
				Kind:  Stop,
				Raw:   []byte{'\n'},
			},
		},
		{
			Token: Token{
				Start: Location{Name: "test.tcl", Column: 1, Line: 2, Offset: 2},
				End:   Location{Name: "test.tcl", Column: 2, Line: 2, Offset: 3},
				Kind:  Whitespace,
				Raw:   []byte("\r"),
			},
		},
		{
			Token: Token{
				Start: Location{Name: "test.tcl", Column: 2, Line: 2, Offset: 3},
				End:   Location{Name: "test.tcl", Column: 1, Line: 3, Offset: 4},
				Kind:  Stop,
				Raw:   []byte{'\n'},
			},
		},
		{
			Token: Token{
				Start: Location{Name: "test.tcl", Column: 1, Line: 3, Offset: 4},
				End:   Location{Name: "test.tcl", Column: 3, Line: 3, Offset: 6},
				Kind:  Whitespace,
				Raw:   []byte("\t "),
			},
		},
	}.Run(t, " \n\r\n\t ")
}

type flagTest struct {
	Seq string
	On  tokenSeq
	Off tokenSeq
}

func (f flagTest) Test(t *testing.T) {
	t.Run("On", func(t *testing.T) {
		f.On.Run(t, f.Seq)
	})
	t.Run("Off", func(t *testing.T) {
		f.Off.Run(t, f.Seq)
	})
}

func TestInvalidUTF8(t *testing.T) {
	tokenSeq{_error}.Run(t, "\xff")
}

func TestStatementInvalid(t *testing.T) {
	tokenSeq{
		{Token: Token{Kind: Word, Raw: []byte("a"), Value: "a"}},
		_eof,
	}.Run(t, `a`)
	tokenSeq{
		{Token: Token{Kind: Word, Raw: []byte("a"), Value: "a"}},
		_error,
	}.Run(t, "a\x00")
}

func TestString(t *testing.T) {
	tokenSeq{
		{Token: Token{Kind: Word, Raw: []byte("stmt"), Value: "stmt"}},
		_ws, {Token: Token{Kind: QuotedString, Raw: []byte(`""`), Value: ""}}, _nl,
		_ws, {Token: Token{Kind: QuotedString, Raw: []byte(`"simple string"`), Value: `simple string`}}, _nl,
		_ws, {Token: Token{Kind: QuotedString, Raw: []byte(`"string with [square brackets "quoted"] inside"`), Value: `string with [square brackets "quoted"] inside`}}, _nl,
		_ws,
		// Very weird quoting if you're not careful:
		{Token: Token{Kind: QuotedString, Raw: []byte(`"string with \[square brackets "`), Value: `string with \[square brackets `}},
		{Token: Token{Kind: Word, Raw: []byte(`quoted`), Value: `quoted`}},
		{Token: Token{Kind: QuotedString, Raw: []byte(`"] inside"`), Value: `] inside`}},
		_nl,
		// End very weird quoting.
		_ws, {Token: Token{Kind: QuotedString, Raw: []byte(`"\a\b\f\n\r\t\v\\\""`), Value: `\a\b\f\n\r\t\v\\\"`}}, _nl,
		_ws, {Token: Token{Kind: QuotedString, Raw: []byte(`"\123\xff\u7fff\U00001234"`), Value: `\123\xff\u7fff\U00001234`}}, _nl,
		_ws, {Token: Token{Kind: QuotedString, Raw: []byte(`"\xFF"`), Value: `\xFF`}}, _nl,
		_ws, {Token: Token{Kind: Word, Raw: []byte(`a`), Value: `a`}},
		{Token: Token{Kind: QuotedString, Raw: []byte(`"b"`), Value: `b`}},
		{Token: Token{Kind: Word, Raw: []byte(`c`), Value: `c`}}, _nl,
		_ws, {Token: Token{Kind: RawString, Raw: []byte(`{a b c}`), Value: "a b c"}}, _nl,
		_ws, {Token: Token{Kind: RawString, Raw: []byte(`{a {b\} c[}}`), Value: `a {b\} c[}`}}, _nl,
		_nl,
		_ws, _semicolon,
		_eof,
	}.Run(t,
		`stmt   ""
			"simple string"
			"string with [square brackets "quoted"] inside"
			"string with \[square brackets "quoted"] inside"
			"\a\b\f\n\r\t\v\\\""
			"\123\xff\u7fff\U00001234"
			"\xFF"
			a"b"c
			{a b c}
			{a {b\} c[}}

		;`)
}

func TestInvalidStrings(t *testing.T) {
	invalid := tokenSeq{
		{Token: Token{
			Kind:  Word,
			Raw:   []byte("stmt"),
			Value: "stmt",
		}},
		_ws, _error,
	}

	cases := []tokenSeqTest{
		{Name: "Quote/EOF", Input: `stmt "`},
		{Name: "Quote/EOF-Content", Input: `stmt "after`},
	}

	for _, c := range cases {
		if c.Seq == nil {
			c.Seq = invalid
		}
		t.Logf("Running %v", c.Name)
		c.Run(t)
	}
}

func TestLocationString(t *testing.T) {
	const (
		want      = "2:34:45"
		wantNamed = "Location Name:" + want
	)
	loc := Location{
		Line:   2,
		Column: 34,
		Offset: 45,
	}
	if got := loc.String(); got != want {
		t.Fatalf("%#+v.String() = %q; want %q", loc, got, want)
	}

	loc.Name = "Location Name"
	if got := loc.String(); got != wantNamed {
		t.Fatalf("%#+v.String() = %q; want %q", loc, got, wantNamed)
	}
}
