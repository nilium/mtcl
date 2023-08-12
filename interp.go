package mtcl

import (
	"errors"
	"fmt"
	"strconv"
	"strings"

	"go.spiff.io/mtcl/lexer"
	"golang.org/x/exp/maps"
	"golang.org/x/exp/slices"
)

const UnknownCmd = `*unknown*`

func IsEmpty(val Value) bool {
	switch val := val.(type) {
	case String:
		return len(val) == 0
	case Values:
		return len(val) == 0
	case Map:
		return len(val) == 0
	case nil:
		return true
	}
	return false
}

func Empty() Values {
	return nil
}

func EmptyIterator() Iterator {
	return func(Values) (bool, error) {
		return false, nil
	}
}

type OnceIterable struct {
	Value
}

func (o OnceIterable) Iterator() Iterator {
	if IsEmpty(o.Value) {
		return EmptyIterator()
	}
	return OnceIterator(o.Value)
}

func OnceIterator(val Value) Iterator {
	var iter Iterator
	iter = func(vals Values) (bool, error) {
		if len(vals) == 0 {
			return true, nil
		}
		iter = func(vals Values) (bool, error) { return false, nil }
		if len(vals) != 1 {
			return false, nil
		}
		vals[0] = val
		return true, nil
	}
	return func(vals Values) (bool, error) {
		return iter(vals)
	}
}

type String string

func (String) value()           {}
func (s String) String() string { return string(s) }

func (String) Type() string {
	return "string"
}

func (s String) Expand() Values {
	if s == "" {
		return nil
	}
	return Values{s}
}

type Error struct {
	Err error
}

func (*Error) value() {}

func (e *Error) String() string {
	return e.Err.Error()
}

func (e *Error) Error() string {
	return e.Err.Error()
}

func (e *Error) Unwrap() error {
	return e.Err
}

func (e *Error) Expand() Values {
	return Values{String(e.Error())}
}

func (e *Error) Type() string {
	return "error"
}

type Values []Value

func (Values) value() {}
func (vs Values) String() string {
	switch len(vs) {
	case 0:
		return ""
	case 1:
		return vs[0].String()
	default:
		var out strings.Builder
		for i, v := range vs {
			str := v.String()
			if str == "" {
				continue
			}
			out.Grow(len(str) + 1)
			if i > 0 && out.Len() > 0 {
				out.WriteByte(' ')
			}
			out.WriteString(str)
		}
		return out.String()
	}
}

func (vs Values) Expand() Values {
	return vs
}

func (vs Values) Type() string {
	return "vec"
}

func (vs Values) Iterator() Iterator {
	return func(vals Values) (bool, error) {
		if len(vals) == 0 {
			return len(vs) > 0, nil
		}
		if len(vs) < len(vals) {
			return false, nil
		}
		copy(vals, vs)
		vs = vs[len(vals):]
		return true, nil
	}
}

type Map map[String]Value

func (m Map) value() {}

func (m Map) Type() string {
	return "dict"
}

func (m Map) Expand() Values {
	keys := maps.Keys(m)
	slices.Sort(keys)
	values := make(Values, len(keys))
	for i, key := range keys {
		ki := i * 2
		vi := ki + 1
		values[ki], values[vi] = String(key), m[key]
	}
	return values
}

func (m Map) String() string {
	items := make([]string, 0, len(m)*2)
	for k, v := range m {
		items = append(items, string(k), v.String())
	}
	return strings.Join(items, " ")
}

func (m Map) Iterator() Iterator {
	keys := maps.Keys(m)
	slices.Sort(keys)
	return func(vals Values) (bool, error) {
		switch len(vals) {
		case 0:
			return len(keys) > 0, nil
		case 1, 2:
		default:
			return false, errors.New("map iterator only accepts one or two iterator parameters")
		}
		for i, key := range keys {
			val, ok := m[key]
			if !ok {
				continue
			}
			keys = keys[i+1:]
			switch len(vals) {
			case 2:
				vals[0], vals[1] = String(key), val
			case 1:
				vals[0] = String(key)
			}
			return true, nil
		}
		keys = nil
		return false, nil
	}
}

type Func struct {
	Fn    Cmd
	Binds Values
}

func (*Func) value()         {}
func (*Func) String() string { return "func" }
func (*Func) Type() string   { return "func" }

func (fn *Func) Expand() Values { return Values{fn} }

func (fn *Func) Call(tcl *Interp, args Values) (Values, error) {
	if len(fn.Binds) > 0 {
		args = append(append(make(Values, 0, len(args)+len(fn.Binds)), fn.Binds...), args...)
	}
	return fn.Fn.Call(tcl, args)
}

type Value interface {
	value()

	String() string
	Type() string
	Expand() Values
}

type Iterable interface {
	Value
	Iterator() Iterator
}

type Iterator func(vals Values) (bool, error)

type varTable map[string]*Values

func (vt varTable) set(name string, vals Values) {
	vals = slices.Clone(vals)
	vp, ok := vt[name]
	if !ok {
		vt[name] = &vals
	} else {
		*vp = vals
	}
}

func (vt varTable) val(name string) (Values, bool) {
	vp, ok := vt[name]
	if !ok {
		return nil, false
	}
	return *vp, true
}

func isReturn(err error) bool {
	rc, ok := returnCode(err)
	return ok && (rc == ReturnOK || rc == ReturnError)
}

func isContinue(err error) bool {
	return errors.Is(err, ReturnContinue)
}

func isBreak(err error) bool {
	return errors.Is(err, ReturnBreak)
}

func returnCode(err error) (code ReturnCode, ok bool) {
	ok = errors.As(err, &code)
	return code, ok
}

type ReturnCode int

const (
	ReturnOK       ReturnCode = 0
	ReturnError    ReturnCode = 1
	ReturnOuter    ReturnCode = 2
	ReturnBreak    ReturnCode = 3
	ReturnContinue ReturnCode = 4
)

func (rc ReturnCode) Error() string {
	return strconv.Itoa(int(rc))
}

var (
	ErrNotFound    = errors.New("not found")
	ErrVarNotFound = fmt.Errorf("variable %w", ErrNotFound)
	ErrCmdNotFound = fmt.Errorf("command %w", ErrNotFound)
)

type CmdFunc func(interp *Interp, args Values) (Values, error)

func (cmd CmdFunc) Call(interp *Interp, args Values) (Values, error) {
	return cmd(interp, args)
}

type CmdExprFunc func(interp *Interp, args []Expr) (Values, error)

func (cmd CmdExprFunc) Call(interp *Interp, args Values) (Values, error) {
	return nil, errors.New("command must be called using callExpr")
}

func (cmd CmdExprFunc) callExpr(interp *Interp, args []Expr) (Values, error) {
	return cmd(interp, args)
}

type Cmd interface {
	Call(interp *Interp, args Values) (Values, error)
}

type cmdExpr interface {
	Cmd
	callExpr(interp *Interp, args []Expr) (Values, error)
}

func doInContext[Exprs ~[]E, E Expr](exprScope, evalScope *Interp, args Exprs) (last Values, err error) {
	var cmds []*Command
	for _, arg := range args {
		cmds, err = commandsFor(exprScope, arg)
		if err != nil {
			return last, err
		}

		for _, cmd := range cmds {
			last, err = evalScope.Do(cmd)
			if err != nil {
				return last, err
			}
		}
	}
	return last, err
}

func commandsFor(tcl *Interp, e Expr) ([]*Command, error) {
	// Use pre-parsed commands.
	if rs, ok := e.(*RawString); ok && rs.Cmds != nil {
		return rs.Cmds, nil
	}
	// Parse on the fly.
	vals, err := tcl.Do(e)
	if err != nil {
		return nil, fmt.Errorf("cannot evaluate expression: %w", err)
	}
	lexer := lexer.NewLexer(strings.NewReader(strings.Join(Strings(vals), " ")))
	lexer.SetPos(e.Token().Start)
	parser := NewParser(lexer)
	return parser.Parse()
}

type Interp struct {
	vars     varTable       // read-write
	cmds     map[string]Cmd // read-write
	prelude  map[string]Cmd // read-only
	overlays []*Interp      // Should only be modified on creation
	parent   *Interp        // read-only
	root     *Interp        // read-only
}

func NewInterp() *Interp {
	tcl := &Interp{
		vars: varTable{},
		cmds: map[string]Cmd{},
	}
	tcl.root = tcl
	return tcl
}

func (tcl *Interp) SetPrelude(cmds map[string]Cmd) {
	tcl.prelude = cmds
}

func (tcl *Interp) BindCmds(cmds map[string]Cmd) {
	for name, cmd := range cmds {
		tcl.Bind(name, cmd)
	}
}

func (tcl *Interp) Do(e Expr) (result Values, err error) {
	switch e := e.(type) {
	case *Access:
		val, err := tcl.Var(e.Access)
		return val, err

	case *Block:
		return tcl.expandBlock(e)

	case *Command:
		return tcl.doCommand(e)

	case *RawString:
		return Values{String(e.RawString)}, nil

	case *QuoteString:
		return tcl.expandString(e)

	case *Word:
		return tcl.expandWord(e)

	case *Literal:
		return Values{String(e.Literal)}, nil

	default:
		return nil, fmt.Errorf("unsupported expression type %T", e)
	}
}

func (tcl *Interp) expandBlock(e *Block) (Values, error) {
	var last Values
	var err error
	for _, cmd := range e.Block {
		last, err = tcl.Do(cmd)
		if err != nil {
			break
		}
	}
	return last, err
}

func (tcl *Interp) expandString(e *QuoteString) (Values, error) {
	var out strings.Builder
	for _, chunk := range e.QuoteString.Word {
		vals, err := tcl.Do(chunk)
		if err != nil {
			return nil, err
		}
		n := out.Len()
		for i, val := range vals {
			str := val.String()
			if str == "" {
				continue
			}
			out.Grow(len(str) + 1)
			if out.Len() > n && i > 0 {
				out.WriteByte(' ')
			}
			out.WriteString(str)
		}
	}
	return Values{String(out.String())}, nil
}

func (tcl *Interp) expandWord(e *Word) (Values, error) {
	if len(e.Word) == 0 {
		return nil, nil
	} else if len(e.Word) == 1 {
		if lit, ok := e.Word[0].(*Literal); ok {
			return Values{String(lit.Literal)}, nil
		}
		return tcl.Do(e.Word[0])
	}

	var parts []Value
	for _, chunk := range e.Word {
		vals, err := tcl.Do(chunk)
		if err != nil {
			return nil, err
		}
		if len(parts) == 0 {
			parts = slices.Clone(vals)
			continue
		}
		switch len(vals) {
		case 0:
		case 1:
			val := vals[0]
			for i, part := range parts {
				parts[i] = String(part.String() + val.String())
			}
		default:
			mul := make(Values, 0, len(parts)*len(vals))
			for _, part := range parts {
				for _, val := range vals {
					mul = append(mul, String(part.String()+val.String()))
				}
			}
			parts = mul
		}
	}
	return parts, nil
}

func (tcl *Interp) doCommand(e *Command) (Values, error) {
	nameEval, err := tcl.Do(e.Command)
	if err != nil {
		return nil, err
	}
	if len(nameEval) == 0 {
		return nil, errors.New("command name does not evaluate to anything")
	}
	name := nameEval[0]
	unknown := 0

	cmd, ok := name.(Cmd)
	if !ok {
		cmd, err = tcl.Cmd(name.String())
	}
	if errors.Is(err, ErrCmdNotFound) {
		ucmd, uerr := tcl.Cmd(UnknownCmd)
		if uerr != nil {
			return nil, err
		}
		unknown, cmd, err = 1, ucmd, nil
	}
	if err != nil {
		return nil, fmt.Errorf("cannot find command %q: %w", name, err)
	}

	if ce, ok := cmd.(cmdExpr); ok {
		return ce.callExpr(tcl, slices.Clone(e.Params))
	}

	args := make(Values, len(nameEval)-1+unknown, len(e.Params)+(len(nameEval)-1+unknown))
	if unknown == 1 {
		args[0] = name
	}
	copy(args[unknown:], nameEval[1:])
	for i, param := range e.Params {
		vals, err := tcl.Do(param)
		if err != nil {
			return nil, fmt.Errorf("error evaluating param %d to command %q: %w", i, name, err)
		}
		args = append(args, vals...)
	}

	return cmd.Call(tcl, args)
}

func (tcl *Interp) Call(name string, args ...Value) (Values, error) {
	cmd, err := tcl.Cmd(name)
	if err != nil {
		return nil, err
	}
	return cmd.Call(tcl, Values(args))
}

func (tcl *Interp) Fork() *Interp {
	return tcl.fork()
}

func (tcl *Interp) fork() *Interp {
	return &Interp{
		cmds:     map[string]Cmd{},
		vars:     varTable{},
		parent:   tcl,
		overlays: slices.Clone(tcl.overlays),
		root:     tcl.root,
	}
}

func (tcl *Interp) cmd(name string, seen map[*Interp]struct{}) (Cmd, error) {
	if seen == nil {
		seen = map[*Interp]struct{}{}
	}
	for ; tcl != nil; tcl = tcl.parent {
		if _, ok := seen[tcl]; ok {
			continue
		}

		for i := len(tcl.overlays) - 1; i >= 0; i-- {
			if cmd, err := tcl.overlays[i].cmd(name, seen); err == nil {
				return cmd, nil
			}
		}

		seen[tcl] = struct{}{}
		cmd, ok := tcl.cmds[name]
		if ok {
			return cmd, nil
		}
	}
	return nil, fmt.Errorf("%w: %q", ErrCmdNotFound, name)
}

func (tcl *Interp) Cmd(name string) (Cmd, error) {
	cmd, err := tcl.cmd(name, nil)
	if err == nil {
		return cmd, nil
	}
	cmd, ok := tcl.root.prelude[name]
	if !ok {
		return nil, err
	}
	return cmd, nil
}

func (tcl *Interp) SetVar(name string, vals Values) {
	tcl.vars.set(name, vals)
}

func (tcl *Interp) VarStorage(name string) (*Values, error) {
	vals, ok := tcl.vars[name]
	if ok {
		return vals, nil
	}
	return nil, fmt.Errorf("%w: %q", ErrVarNotFound, name)
}

func (tcl *Interp) Var(name string) (Values, error) {
	vals, ok := tcl.vars.val(name)
	if ok {
		return slices.Clone(vals), nil
	}
	return nil, fmt.Errorf("%w: %q", ErrVarNotFound, name)
}

func (tcl *Interp) Unset(name string) {
	delete(tcl.vars, name)
}

func (tcl *Interp) Global(name string) error {
	return tcl.root.lift(name, tcl, nil)
}

func (tcl *Interp) lift(name string, into *Interp, seen map[*Interp]struct{}) error {
	if seen == nil {
		seen = map[*Interp]struct{}{}
	}
	for ; tcl != nil; tcl = tcl.parent {
		if _, ok := seen[tcl]; ok {
			continue
		}
		for i := len(tcl.overlays) - 1; i >= 0; i-- {
			tcl.overlays[i].lift(name, into, seen)
		}

		seen[tcl] = struct{}{}
		storage, err := tcl.VarStorage(name)
		if err == nil {
			into.vars[name] = storage
			return nil
		}
	}
	return fmt.Errorf("%w: %q", ErrVarNotFound, name)
}

func (tcl *Interp) Upvar(name string) error {
	return tcl.lift(name, tcl, nil)
}

func (tcl *Interp) Parent(nth uint) (*Interp, error) {
	depth := nth
	for ; tcl != nil && nth > 0; nth-- {
		tcl = tcl.parent
	}
	if tcl == nil {
		return nil, fmt.Errorf("no scope at depth %d", depth)
	}
	return tcl, nil
}

func (tcl *Interp) Return() (*Interp, error) {
	if tcl.parent == nil {
		return tcl, errors.New("cannot return from root scope")
	}
	return tcl.parent, nil
}

func (tcl *Interp) GlobalScope() *Interp {
	return tcl.root
}

func (tcl *Interp) Bind(name string, cmd Cmd) {
	tcl.cmds[name] = cmd
}

func Strings[Slice ~[]E, E fmt.Stringer](vals Slice) []string {
	strs := make([]string, len(vals))
	for i, e := range vals {
		strs[i] = e.String()
	}
	return strs
}
