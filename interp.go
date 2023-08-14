package mtcl

import (
	"errors"
	"fmt"
	"strconv"
	"strings"

	"go.spiff.io/mtcl/lexer"
	"golang.org/x/exp/slices"
)

const UnknownCmd = `*unknown*`

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

type EvalErrorFunc func(Values, error) (values Values, err error, exit bool)

func (tcl *Interp) Eval(e Expr, lookup *Interp, handleErr EvalErrorFunc) (result Values, err error) {
	if lookup == nil {
		lookup = tcl
	}
	cmds, err := commandsFor(lookup, e)
	if err != nil {
		return nil, fmt.Errorf("eval: cannot parse expression as command(s): %w", err)
	}
	if handleErr == nil {
		handleErr = func(vals Values, err error) (_ Values, _ error, exit bool) {
			return vals, err, true
		}
	}
	var exit bool
	for _, cmd := range cmds {
		result, err = tcl.Do(cmd)
		if err != nil {
			result, err, exit = handleErr(result, err)
			if err != nil || exit {
				return result, err
			}
		}
	}
	return result, err
}

func (tcl *Interp) Do(e Expr) (result Values, err error) {
	switch e := e.(type) {
	case *Access:
		val, err := tcl.Var(e.Access)
		return val, err

	case *ListExpr:
		return tcl.expandList(e)

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
		switch e.Literal {
		case "true":
			return Values{True}, nil
		case "false":
			return Values{False}, nil
		case "":
			return Values{String("")}, nil
		}
		if f := e.Literal[0]; len(e.Literal) > 0 && (f == '-' || f == '+' || f == '.' || (f >= '0' && f <= '9')) {
			var n Int
			if f == '.' {
				// Fall through to float.
			} else if _, ok := n.Num.SetString(e.Literal, 0); ok {
				return Values{LitInt{
					Int:     &n,
					Literal: e,
				}}, nil
			}
			if f, err := strconv.ParseFloat(e.Literal, 64); err == nil {
				return Values{LitFloat{
					Float:   Float(f),
					Literal: e,
				}}, nil
			}
		}
		return Values{String(e.Literal)}, nil

	default:
		return nil, fmt.Errorf("unsupported expression type %T", e)
	}
}

func (tcl *Interp) expandList(e *ListExpr) (Values, error) {
	vals := make(Values, 0, len(e.List))
	for i, sub := range e.List {
		subvals, err := tcl.Do(sub)
		if err != nil {
			return nil, fmt.Errorf("cannot evaluate list expression %d: %w", i+1, err)
		}
		vals = append(vals, subvals...)
	}
	return Values{vals}, nil
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
