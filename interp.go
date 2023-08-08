package mtcl

import (
	"errors"
	"fmt"
	"strconv"
	"strings"

	"go.spiff.io/mtcl/lexer"
	"golang.org/x/exp/slices"
)

type String string

func (String) value()           {}
func (s String) String() string { return string(s) }

func (String) Type() string {
	return "string"
}

func (s String) Expand() Values {
	return Values{s}
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

type Iterator func() []Value

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

type Return struct {
	Code ReturnCode
}

func (r *Return) Error() string {
	return "return{" + strconv.Itoa(int(r.Code)) + "}"
}

func isReturn(err error) bool {
	var r *Return
	return errors.As(err, &r)
}

type ReturnCode int

const (
	ReturnOK       ReturnCode = 0
	ReturnError    ReturnCode = 1
	ReturnOuter    ReturnCode = 2
	ReturnBreak    ReturnCode = 3
	ReturnContinue ReturnCode = 4
)

type Context interface {
	// Command lookup and calling.
	Cmd(name string) (Cmd, error)
	Call(name string, args ...Value) (Values, error)
	Bind(name string, cmd Cmd)

	// Expression evaluation.
	Do(expr Expr) (results Values, err error)

	// Variable assignment and lookup.
	SetVar(name string, vals Values)
	Var(name string) (values Values, err error)

	// Lift variables from a depth or global scope.
	Upvar(name string, depth uint) error
	Global(name string) error

	// Get to global scope.
	GlobalScope() Context

	// Fork creates a context below the receiver.
	Fork() Context
	Return() (Context, error) // Debatable.
	Parent(nth uint) (Context, error)
}

var (
	ErrNotFound    = errors.New("not found")
	ErrVarNotFound = fmt.Errorf("variable %w", ErrNotFound)
	ErrCmdNotFound = fmt.Errorf("command %w", ErrNotFound)
)

type CmdFunc func(interp Context, args Values) (Values, error)

func (cmd CmdFunc) Call(interp Context, args Values) (Values, error) {
	return cmd(interp, args)
}

type CmdExprFunc func(interp Context, args []Expr) (Values, error)

func (cmd CmdExprFunc) Call(interp Context, args Values) (Values, error) {
	return nil, errors.New("command must be called using CallExpr")
}

func (cmd CmdExprFunc) CallExpr(interp Context, args []Expr) (Values, error) {
	return cmd(interp, args)
}

type Cmd interface {
	Call(interp Context, args Values) (Values, error)
}

type CmdExpr interface {
	Cmd
	CallExpr(interp Context, args []Expr) (Values, error)
}

type Interp struct {
	root  interpScope
	scope *interpScope
}

var _ Context = (*Interp)(nil)

// Call implements Context
func (tr *Interp) Call(name string, args ...Value) (Values, error) {
	return tr.scope.Call(name, args...)
}

// Cmd implements Context
func (tr *Interp) Cmd(name string) (Cmd, error) {
	return tr.scope.Cmd(name)
}

func (tr *Interp) Bind(name string, cmd Cmd) {
	tr.GlobalScope().Bind(name, cmd)
}

// Do implements Context
func (tr *Interp) Do(expr Expr) (results Values, err error) {
	return tr.scope.Do(expr)
}

// Global implements Context
func (tr *Interp) Global(name string) error {
	return tr.scope.Global(name)
}

// SetVar implements Context
func (tr *Interp) SetVar(name string, vals Values) {
	tr.scope.SetVar(name, vals)
}

func (tr *Interp) Var(name string) (values Values, err error) {
	return tr.scope.Var(name)
}

// Upvar implements Context
func (tr *Interp) Upvar(name string, depth uint) error {
	return tr.scope.Upvar(name, depth)
}

func (tr *Interp) Parent(nth uint) (Context, error) {
	return tr.scope.Parent(nth)
}

func (tr *Interp) Fork() Context {
	return tr.scope.Fork()
}

func (tr *Interp) Return() (Context, error) {
	return tr, errors.New("cannot return from global scope")
}

func (tr *Interp) GlobalScope() Context {
	return tr.scope
}

func doInContext(tcl, scope Context, args []Expr) (Values, error) {
	var last Values
	var err error
	for _, arg := range args {
		cmds, err := commandsFor(tcl, arg)
		if err != nil {
			return last, err
		}

		for _, cmd := range cmds {
			last, err = scope.Do(cmd)
			if err != nil {
				return last, err
			}
		}
	}
	return last, err
}

var baseCmds = map[string]Cmd{
	"set": CmdFunc(func(tcl Context, args Values) (Values, error) {
		switch len(args) {
		case 0:
			return nil, errors.New("no arguments given, must have at least var name and zero or more values")
		case 1:
			name := args[0]
			return tcl.Var(name.String())
		default:
			name, vals := args[0], args[1:]
			tcl.SetVar(name.String(), vals)
			return slices.Clone(vals), nil
		}
	}),
	"...": CmdFunc(func(tcl Context, args Values) (Values, error) {
		vals := make(Values, 0, len(args))
		for _, arg := range args {
			vals = append(vals, arg.Expand()...)
		}
		return vals, nil
	}),
	"type": CmdFunc(func(tcl Context, args Values) (Values, error) {
		types := make(Values, len(args))
		for i, arg := range args {
			types[i] = String(arg.Type())
		}
		return types, nil
	}),
	"puts": CmdFunc(func(tr Context, args Values) (Values, error) {
		_, err := fmt.Println(strings.Join(Strings(args), "\t"))
		return nil, err
	}),
	"concat": CmdFunc(func(tr Context, args Values) (Values, error) {
		return Values{String(strings.Join(Strings(args), " "))}, nil
	}),
	"list": CmdFunc(func(tr Context, args Values) (Values, error) { // Doesn't really do anything.
		return Values{slices.Clone(args)}, nil
	}),
	"nth": CmdFunc(func(tr Context, args Values) (Values, error) {
		top := args[0]
		for i, indexVal := range args[1:] {
			index, err := strconv.Atoi(indexVal.String())
			if err != nil {
				return nil, fmt.Errorf("nth: index %q could not be interpreted as an integer: %w", indexVal, err)
			}

			list, ok := top.(Values)
			if !ok {
				if i == 0 {
					return nil, errors.New("nth: first argument must be a list")
				}
				return nil, fmt.Errorf("nth: value at path [%v] must be a list", strings.Join(Strings(args[1:1+i]), "]["))
			}
			if index < 0 || index >= len(list) {
				return nil, fmt.Errorf("nth: index %d out of bounds for list", index)
			}
			top = list[index]
		}
		if top == nil {
			return nil, nil
		}
		return Values{top}, nil
	}),
	"upvar": CmdFunc(func(interp Context, args Values) (Values, error) {
		return nil, errors.New("unimplemented")
	}),
	"uplevel": CmdExprFunc(func(tcl Context, args []Expr) (Values, error) {
		var depth uint = 1
		var exprs = args
		switch len(args) {
		case 1: // use defaults for depth, exprs
		case 2:
			vals, err := tcl.Do(args[0])
			if err != nil {
				return nil, err
			}
			val := strings.Join(Strings(vals), " ")
			depth64, err := strconv.ParseUint(val, 10, strconv.IntSize)
			if err != nil {
				return nil, fmt.Errorf("uplevel: cannot parse depth %q: %w", val, err)
			}
			depth = uint(depth64)
			exprs = args[1:]
		default:
			return nil, fmt.Errorf("uplevel takes 1..2 arguments, got %d", len(args))
		}

		scope, err := tcl.Parent(depth)
		if err != nil {
			return nil, fmt.Errorf("unable to get scope at depth %d: %w", depth, err)
		}
		return doInContext(tcl, scope, exprs)
	}),
	"do": CmdExprFunc(func(tcl Context, args []Expr) (Values, error) {
		return doInContext(tcl, tcl.Fork(), args)
	}),
	"proc": CmdExprFunc(func(tcl Context, args []Expr) (Values, error) {
		names, err := tcl.Do(args[0])
		if err != nil {
			return nil, fmt.Errorf("unable to parse proc name: %w", err)
		}

		if len(names) == 0 {
			return nil, fmt.Errorf("proc name at %v is empty", args[0].Token().Start)
		}
		name := names[0] // Name *can* be empty for some reason.

		paramNV, err := tcl.Do(args[1])
		if err != nil {
			return nil, fmt.Errorf("unable to parse proc %q parameter names: %w", name, err)
		}
		params := make([]string, 0, len(paramNV))
		for _, nv := range paramNV {
			params = append(params, strings.Fields(nv.String())...)
		}

		cmds, err := commandsFor(tcl, args[2])
		if err != nil {
			return nil, fmt.Errorf("unable to parse proc %q body: %w", name, err)
		}

		n := len(params)
		var vparam string
		variadic := n > 0 && strings.HasPrefix(params[n-1], "*")
		if variadic {
			n--
			vparam, params = params[n], params[:n]
		}

		proc := func(tcl Context, args Values) (last Values, err error) {
			argc := len(args)
			if variadic && argc < n {
				return nil, fmt.Errorf("%s: expected at least %d arguments, got %d", name, n, argc)
			} else if !variadic && argc != n {
				return nil, fmt.Errorf("%s: expected %d arguments, got %d", name, n, argc)
			}

			tcl = tcl.Fork()
			for i, param := range params {
				tcl.SetVar(param, Values{args[i]})
			}
			tcl.SetVar(vparam, args[n:])

			for _, cmd := range cmds {
				last, err = tcl.Do(cmd)
				if isReturn(err) {
					return last, nil
				} else if err != nil {
					return last, err
				}
			}
			return last, err
		}
		tcl.Bind(name.String(), CmdFunc(proc))

		return nil, nil
	}),
	"return": CmdFunc(func(tcl Context, args Values) (Values, error) {
		return slices.Clone(args), &Return{Code: ReturnOK}
	}),
}

func commandsFor(tcl Context, e Expr) ([]*Command, error) {
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

func NewInterp() *Interp {
	tr := &Interp{
		root: interpScope{
			cmds:   baseCmds,
			vars:   varTable{},
			interp: &Interp{},
		},
	}
	// Treat the outermost context as a global scope with the root scope
	// as the sort of prelude scope. Vars are shared between global and
	// root, but global-defined commands cannot overwrite root commands,
	// only hide them.
	tr.scope = tr.root.fork()
	tr.scope.vars = tr.root.vars
	return tr
}

type interpScope struct {
	cmds   map[string]Cmd
	vars   varTable
	parent *interpScope
	interp *Interp
}

func (scope *interpScope) Do(e Expr) (result Values, err error) {
	switch e := e.(type) {
	case *Access:
		val, err := scope.Var(e.Access)
		return val, err
	case *Block:
		var last Values
		var err error
		for _, cmd := range e.Block {
			last, err = scope.Do(cmd)
			if err != nil {
				break
			}
		}
		return last, err
	case *Command:
		nameEval, err := scope.Do(e.Command)
		if err != nil {
			return nil, err
		}
		if len(nameEval) == 0 {
			return nil, errors.New("command name does not evaluate to anything")
		}
		name := nameEval[0]
		cmd, err := scope.Cmd(name.String())
		if err != nil {
			return nil, fmt.Errorf("cannot find command %q: %w", name, err)
		}

		if ce, ok := cmd.(CmdExpr); ok {
			return ce.CallExpr(scope, slices.Clone(e.Params))
		}

		args := make(Values, len(nameEval)-1, len(e.Params)+len(nameEval)-1)
		copy(args, nameEval[1:])
		for i, param := range e.Params {
			vals, err := scope.Do(param)
			if err != nil {
				return nil, fmt.Errorf("error evaluating param %d to command %q: %w", i, name, err)
			}
			args = append(args, vals...)
		}
		return cmd.Call(scope, args)
	case *RawString:
		return Values{String(e.RawString)}, nil
	case *QuoteString:
		var out strings.Builder
		for _, chunk := range e.QuoteString.Word {
			vals, err := scope.Do(chunk)
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
	case *Word:
		if len(e.Word) == 0 {
			return nil, nil
		} else if len(e.Word) == 1 {
			if lit, ok := e.Word[0].(*Literal); ok {
				return Values{String(lit.Literal)}, nil
			}
			return scope.Do(e.Word[0])
		}

		var parts []Value
		for _, chunk := range e.Word {
			vals, err := scope.Do(chunk)
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
	case *Literal:
		return Values{String(e.Literal)}, nil

	default:
		return nil, fmt.Errorf("unsupported expression type %T", e)
	}
}

func (scope *interpScope) Call(name string, args ...Value) (Values, error) {
	cmd, err := scope.Cmd(name)
	if err != nil {
		return nil, err
	}
	return cmd.Call(scope, Values(args))
}

func (scope *interpScope) Fork() Context {
	return scope.fork()
}

func (scope *interpScope) fork() *interpScope {
	return &interpScope{
		cmds:   map[string]Cmd{},
		vars:   varTable{},
		parent: scope,
		interp: scope.interp,
	}
}

func (scope *interpScope) Cmd(name string) (Cmd, error) {
	for scope != nil {
		cmd, ok := scope.cmds[name]
		if ok {
			return cmd, nil
		}
		scope = scope.parent
	}
	return nil, ErrCmdNotFound
}

func (scope *interpScope) SetVar(name string, vals Values) {
	scope.vars.set(name, vals)
}

func (scope *interpScope) Var(name string) (Values, error) {
	vals, ok := scope.vars.val(name)
	if ok {
		return slices.Clone(vals), nil
	}
	return nil, fmt.Errorf("%w: %q", ErrVarNotFound, name)
}

func (scope *interpScope) lift(name string, src *interpScope) error {
	_, ok := scope.vars[name]
	if ok {
		return fmt.Errorf("variable %q already bound", name)
	}

	vp, ok := src.vars[name]
	if !ok {
		return fmt.Errorf("%w: %q", ErrVarNotFound, name)
	}

	scope.vars[name] = vp

	return nil

}

func (scope *interpScope) Global(name string) error {
	return scope.lift(name, &scope.interp.root)
}

func (scope *interpScope) Upvar(name string, depth uint) error {
	if depth == 0 {
		return nil
	}
	src := scope
	for ; src != nil && depth > 0; depth-- {
		src = src.parent
	}
	if src == nil {
		return fmt.Errorf("upvar: level underflow at depth=%d", depth)
	}
	return scope.lift(name, src)
}

func (scope *interpScope) Parent(nth uint) (Context, error) {
	depth := nth
	for ; scope != nil && nth > 0; nth-- {
		scope = scope.parent
	}
	if scope == nil {
		return nil, fmt.Errorf("no scope at depth %d", depth)
	}
	return scope, nil
}

func (scope *interpScope) Return() (Context, error) {
	if scope.parent == nil {
		return scope, errors.New("cannot return from root scope")
	}
	return scope.parent, nil
}

func (scope *interpScope) GlobalScope() Context {
	return scope.interp.GlobalScope()
}

func (scope *interpScope) Bind(name string, cmd Cmd) {
	scope.cmds[name] = cmd
}

func Strings[Slice ~[]E, E fmt.Stringer](vals Slice) []string {
	strs := make([]string, len(vals))
	for i, e := range vals {
		strs[i] = e.String()
	}
	return strs
}
