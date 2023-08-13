package mtcl

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"strconv"
	"strings"

	"golang.org/x/exp/maps"
	"golang.org/x/exp/slices"
)

var baseCmds = map[string]Cmd{
	"set":    CmdFunc(PreludeSet),
	"...":    CmdFunc(PreludeExpand),
	"&":      CmdFunc(PreludeBind),
	"puts":   CmdFunc(PreludePuts),
	"concat": CmdFunc(PreludeConcat),
	"dict":   CmdFunc(PreludeDict),
	"list":   CmdFunc(PreludeList),
	".":      CmdFunc(PreludeIndex),
	"empty?": CmdFunc(PreludeIsEmpty),
	"error?": CmdFunc(PreludeIsError),
	"len":    CmdFunc(PreludeLen),

	// Types
	"type": CmdFunc(PreludeType),
	"int":  CmdFunc(PreludeInt),
	"str":  CmdFunc(PreludeStr),
	"seq":  CmdFunc(PreludeSeq),

	// Control structures
	"catch":   CmdExprFunc(PreludeCatch),
	"foreach": CmdExprFunc(PreludeForeach),
	"if":      CmdExprFunc(PreludeIf),
	"and":     CmdExprFunc(PreludeAnd),
	"or":      CmdExprFunc(PreludeOr),
	"not":     CmdFunc(PreludeNot),
	"true?":   CmdFunc(PreludeIsTrue),
	"upvar":   CmdFunc(PreludeUpvar),
	"uplevel": CmdExprFunc(PreludeUplevel),

	// Control flow
	"break?":        isErrKindFunc(ReturnBreak),
	"break":         CmdFunc(PreludeBreak),
	"continue?":     isErrKindFunc(ReturnContinue),
	"continue":      CmdFunc(PreludeContinue),
	"return?":       isErrKindFunc(ReturnOK),
	"return-error?": isErrKindFunc(ReturnError),
	"return":        CmdFunc(PreludeReturn),

	// Function definition
	"fn": CmdExprFunc(PreludeFn),

	// Pass-through
	"true":  CmdFunc(PreludeTrue),
	"false": CmdFunc(PreludeFalse),
	"tap":   CmdFunc(PreludeTap),
}

func Prelude() map[string]Cmd {
	return maps.Clone(baseCmds)
}

func PreludeSet(tcl *Interp, args Values) (Values, error) {
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
}

func PreludeExpand(tcl *Interp, args Values) (Values, error) {
	vals := make(Values, 0, len(args))
	for _, arg := range args {
		vals = append(vals, arg.Expand()...)
	}
	return vals, nil
}

func PreludeBind(tcl *Interp, args Values) (Values, error) {
	var cmd Cmd
	if valCmd, ok := args[0].(Cmd); ok {
		cmd = valCmd
	} else {
		name := args[0].String()
		namedCmd, err := tcl.Cmd(name)
		if err != nil {
			return nil, err
		}
		cmd = namedCmd
	}

	if _, ok := cmd.(CmdExprFunc); ok {
		return nil, fmt.Errorf("&: cannot bind syntax")
	}

	args = args[1:]
	return Values{&Func{
		Fn:    cmd,
		Binds: slices.Clone(args),
	}}, nil
}

func PreludePuts(tr *Interp, args Values) (Values, error) {
	_, err := fmt.Println(strings.Join(Strings(args), "\t"))
	return nil, err
}

func PreludeConcat(tr *Interp, args Values) (Values, error) {
	strs := Strings(args)
	fs := flag.NewFlagSet("", flag.ContinueOnError)
	fs.SetOutput(io.Discard)
	sep := fs.String("sep", "", "Separator string")
	if err := fs.Parse(strs); err != nil {
		return nil, err
	}
	return Values{String(strings.Join(fs.Args(), *sep))}, nil
}

func PreludeDict(tcl *Interp, args Values) (Values, error) { // Doesn't really do anything.
	n := len(args)
	if n%2 != 0 {
		return nil, errors.New("dict: expected arguments of the form dict key pair")
	}
	dict := make(Map, n/2)
	for len(args) > 0 {
		k, v := String(args[0].String()), args[1]
		args = args[2:]
		dict[k] = v
	}
	return Values{dict}, nil
}

func PreludeList(tcl *Interp, args Values) (Values, error) {
	return Values{slices.Clone(args)}, nil
}

func PreludeIndex(tr *Interp, args Values) (Values, error) {
	top := args[0]
	path := func(i int) string {
		return `[` + strings.Join(Strings(args[1:1+i]), `][`) + `]`
	}
	for i, indexVal := range args[1:] {
		switch conc := top.(type) {
		case Values:
			index, err := strconv.Atoi(indexVal.String())
			if err != nil {
				return nil, fmt.Errorf(".: index %q could not be interpreted as an integer: %w", indexVal, err)
			}
			rawIndex := index
			if index > 0 {
				index = index - 1
			} else if index < 0 {
				index = len(conc) - index
			} else {
				return nil, fmt.Errorf(".: index %d must be a positive [1..) or negative integer (..-1]", rawIndex)
			}
			if index < 0 || index >= len(conc) {
				return nil, fmt.Errorf(".: index %d out of bounds for list", rawIndex)
			}
			top = conc[index]

		case Map:
			top = conc[String(indexVal.String())]

		default:
			if i == 0 {
				return nil, errors.New(".: first argument must be a list or map")
			}
			return nil, fmt.Errorf(".: value at path %v must be a list or map", path(i))
		}
	}
	if top == nil {
		return nil, nil
	}
	return Values{top}, nil
}

func PreludeIsEmpty(tcl *Interp, args Values) (Values, error) {
	if IsEmpty(args) {
		return Values{String("true")}, nil
	}
	out := make(Values, len(args))
	for i, arg := range args {
		if IsEmpty(arg) {
			out[i] = String("true")
		} else {
			out[i] = Empty()
		}
	}
	return out, nil
}

func PreludeIsError(tcl *Interp, args Values) (Values, error) {
	out := make(Values, len(args))
	for i, arg := range args {
		if _, ok := arg.(error); ok {
			out[i] = arg
		} else {
			out[i] = Empty()
		}
	}
	return out, nil
}

func PreludeLen(tcl *Interp, args Values) (Values, error) {
	lens := make(Values, len(args))
	for i, arg := range args {
		if arg == nil {
			lens[i] = NewInt(0)
		} else {
			lens[i] = NewInt(arg.Len())
		}
	}
	return lens, nil
}

func PreludeType(tcl *Interp, args Values) (Values, error) {
	types := make(Values, len(args))
	for i, arg := range args {
		types[i] = String(arg.Type())
	}
	return types, nil
}

func PreludeInt(tcl *Interp, args Values) (Values, error) {
	out := make(Values, len(args))
	for i, arg := range args {
		var n Int
		str := arg.String()
		if _, ok := n.SetString(str, 0); !ok {
			return nil, fmt.Errorf("cannot parse %q as integer", str)
		}
		out[i] = &n
	}
	return out, nil
}

func PreludeStr(tcl *Interp, args Values) (Values, error) {
	out := make(Values, len(args))
	for i, arg := range args {
		out[i] = String(arg.String())
	}
	return out, nil
}

func PreludeSeq(tcl *Interp, args Values) (Values, error) {
	args, err := PreludeInt(tcl, args)
	if err != nil {
		return nil, err
	}
	var seq *Seq
	switch len(args) {
	case 1: // seq start=0 end step=1|-1
		seq = &Seq{
			Start: new(Int),
			End:   args[0].(*Int),
			Step:  new(Int),
		}
		switch seq.End.Sign() {
		case -1:
			seq.Start.SetInt64(-1)
		case 1:
			seq.Start.SetInt64(1)
		}

	case 2: // seq start end step=1|-1
		seq = &Seq{
			Start: args[0].(*Int),
			End:   args[1].(*Int),
			Step:  new(Int),
		}
	case 3: // seq start end step
		seq = &Seq{
			Start: args[0].(*Int),
			End:   args[1].(*Int),
			Step:  args[2].(*Int),
		}
		return Values{seq}, nil
	default:
		return nil, fmt.Errorf("seq: expected 1..3 arguments, got %d", len(args))
	}
	if seq.End.Cmp(&seq.Start.Int) < 0 {
		seq.Step.SetInt64(-1)
	} else {
		seq.Step.SetInt64(1)
	}
	return Values{seq}, nil
}

func PreludeCatch(tcl *Interp, args []Expr) (last Values, err error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("catch: only accepts 1 argument, got %d", len(args))
	}
	cmds, err := commandsFor(tcl, args[0])
	if err != nil {
		return nil, fmt.Errorf("catch: unable to parse script: %w", err)
	}

	for _, cmd := range cmds {
		last, err = tcl.Do(cmd)
		if err != nil {
			break
		}
	}

	result := make(Values, len(last)+1)
	result[0] = Empty()
	if err != nil {
		result[0] = &Error{Err: err}
	}
	copy(result[1:], last)
	return result, nil
}

func PreludeForeach(tcl *Interp, args []Expr) (Values, error) {
	// Look for 'in' keyword
	inKW, err := tcl.Do(args[1])
	if err != nil || len(inKW) != 1 || inKW[0] != String("in") {
		if err != nil {
			return nil, fmt.Errorf("foreach: has invalid form, expected foreach {params...} in expr {body}: %w", err)
		}
		return nil, fmt.Errorf("foreach: has invalid form, expected foreach {params...} in expr {body}")
	}

	paramNV, err := tcl.Do(args[0])
	if err != nil {
		return nil, fmt.Errorf("unable to parse foreach parameter names: %w", err)
	}

	params := make([]string, 0, len(paramNV))
	for _, nv := range paramNV {
		params = append(params, strings.Fields(nv.String())...)
	}

	var ok bool
	var iterable Iterable
	iterables, err := tcl.Do(args[2])
	if err != nil {
		return nil, fmt.Errorf("foreach: error evaluating expression: %w", err)
	}
	if len(iterables) == 0 {
		return nil, nil
	} else if len(iterables) == 1 {
		iterable, ok = iterables[0].(Iterable)
		if !ok {
			iterable = iterables[0].Expand()
		}
	} else {
		iterable = iterables
	}

	cmds, err := commandsFor(tcl, args[3])
	if err != nil {
		return nil, fmt.Errorf("unable to parse foreach body: %w", err)
	}

	unwind := make(map[string]Values, len(params))
	for _, param := range params {
		vals, _ := tcl.Var(param)
		unwind[param] = vals
	}
	defer func() {
		for name, vals := range unwind {
			if vals == nil {
				tcl.Unset(name)
			} else {
				tcl.SetVar(name, vals)
			}
		}
	}()

	iter := iterable.Iterator()
	buf := make(Values, len(params))

	var last Values
foreach:
	for {
		for i := range buf {
			buf[i] = Empty()
		}
		ok, err = iter(buf)
		if err != nil {
			return last, fmt.Errorf("foreach: error iterating over values: %w", err)
		} else if !ok {
			return last, nil
		}

		for i, param := range params {
			tcl.SetVar(param, Values{buf[i]})
		}

		for _, cmd := range cmds {
			last, err = tcl.Do(cmd)
			if isBreak(err) {
				return last, nil
			} else if isContinue(err) {
				continue foreach
			} else if err != nil {
				return last, err
			}
		}
	}
}

func PreludeIf(tcl *Interp, args []Expr) (results Values, err error) {
	if len(args) < 2 {
		return nil, errors.New("if: must pass at least one condition and script pair")
	}

	// Scan structure for validity before evaluating.
	for args, condNum := args[2:], 1; len(args) > 0; condNum++ {
		if len(args) < 2 {
			return nil, errors.New("if: condition missing script")
		}
		cond := args[0]
		args = args[2:]
		if cond.String() == "else" {
			if len(args) != 0 {
				return nil, errors.New("if: else condition must be the last case")
			}
		} else if cond.String() == "elseif" {
			if len(args) == 0 {
				return nil, errors.New("if: condition missing script")
			}
			args = args[1:]
		} else {
			return nil, fmt.Errorf("if: unexpected word %q, need elseif or else", cond.String())
		}
	}

	cond := func(condNum int) (results Values, err error, next bool) {
		cond, body := args[0], args[1]
		args = args[2:]
		condEval, err := tcl.Eval(cond, tcl, nil)
		if rc, ok := returnCode(err); ok && rc != 0 {
			return nil, nil, true
		} else if err != nil {
			return nil, fmt.Errorf("if: error evaluating condition %d: %w", condNum, err), false
		} else if !Truthy(condEval) {
			return nil, nil, true
		}
		results, err = tcl.Eval(body, tcl, nil)
		return results, err, false
	}

	condElse := func(condNum int) (results Values, err error, next bool) {
		var cond, body Expr
		switch args[0].String() {
		case "elseif":
			cond, body, args = args[1], args[2], args[3:]
			condEval, err := tcl.Eval(cond, tcl, nil)
			if rc, ok := returnCode(err); ok && rc != 0 {
				return nil, nil, true
			} else if err != nil {
				return nil, fmt.Errorf("if: error evaluating condition %d: %w", condNum, err), false
			} else if !Truthy(condEval) {
				return nil, nil, true
			}

		case "else":
			body, args = args[1], args[2:]

		default:
			panic("unreachable")
		}

		results, err = tcl.Eval(body, tcl, nil)
		return results, err, false
	}

	for condNum := 1; len(args) > 0; condNum++ {
		var next bool
		results, err, next = cond(condNum)
		if next == false {
			return results, err
		}
		cond = condElse
	}
	return results, nil
}

func PreludeAnd(tcl *Interp, args []Expr) (last Values, err error) {
	for _, arg := range args {
		last, err = tcl.Do(arg)
		if rc, ok := returnCode(err); ok && rc != 0 {
			return nil, nil
		} else if err != nil || !Truthy(last) {
			return nil, err
		}
	}
	return last, err
}

func PreludeOr(tcl *Interp, args []Expr) (last Values, err error) {
	for _, arg := range args {
		last, err = tcl.Do(arg)
		if rc, ok := returnCode(err); ok && rc != 0 {
			last = nil
			continue
		} else if err != nil || Truthy(last) {
			return last, err
		} else if Truthy(last) {
			return last, nil
		}
	}
	return nil, nil
}

func PreludeNot(tcl *Interp, args Values) (Values, error) {
	out := make(Values, len(args))
	for i, arg := range args {
		out[i] = Bool(!Truthy(arg))
	}
	return out, nil
}

func PreludeIsTrue(tcl *Interp, args Values) (Values, error) {
	out := make(Values, len(args))
	for i, arg := range args {
		out[i] = Bool(Truthy(arg))
	}
	return out, nil
}

func PreludeUpvar(tcl *Interp, args Values) (Values, error) {
	if len(args) == 0 {
		return nil, errors.New("upvar requires at least 1 argument, got 0")
	}

	for _, name := range args {
		err := tcl.Upvar(name.String())
		if err != nil {
			return nil, err
		}
	}
	return nil, nil
}

func PreludeUplevel(tcl *Interp, args []Expr) (results Values, err error) {
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

	evalScope, err := tcl.Parent(depth)
	if err != nil {
		return nil, fmt.Errorf("unable to get scope at depth %d: %w", depth, err)
	}
	for _, expr := range exprs {
		results, err = evalScope.Eval(expr, evalScope, nil)
		if err != nil {
			break
		}
	}
	return results, err
}

func PreludeBreak(tcl *Interp, args Values) (Values, error) {
	return slices.Clone(args), ReturnBreak
}

func PreludeContinue(tcl *Interp, args Values) (Values, error) {
	return slices.Clone(args), ReturnContinue
}

func PreludeReturn(tcl *Interp, args Values) (Values, error) {
	return slices.Clone(args), ReturnOK
}

func PreludeFn(tcl *Interp, args []Expr) (Values, error) {
	fnScope := tcl.GlobalScope()

leader:
	for len(args) > 0 {
		val, err := tcl.Do(args[0])
		if err != nil {
			return nil, fmt.Errorf("unable to evaluate fn leader: %w", err)
		}

		switch val.String() {
		case "-local":
			fnScope = tcl
		default:
			break leader
		}

		args = args[1:]
	}

	named := len(args) == 3
	var name Value = String("<anonymous fn>")

	if named {
		names, err := tcl.Do(args[0])
		if err != nil {
			return nil, fmt.Errorf("unable to parse fn name: %w", err)
		}

		if len(names) == 0 {
			return nil, fmt.Errorf("fn name at %v is empty", args[0].Token().Start)
		}
		name = names[0] // Name *can* be empty for some reason.
		args = args[1:]
	}

	if len(args) != 2 {
		return nil, errors.New("fn: cannot define function without at least arguments and name")
	}

	paramNV, err := tcl.Do(args[0])
	if err != nil {
		return nil, fmt.Errorf("unable to parse fn %q parameter names: %w", name, err)
	}
	params := make([]string, 0, len(paramNV))
	for _, nv := range paramNV {
		params = append(params, strings.Fields(nv.String())...)
	}

	cmds, err := commandsFor(tcl, args[1])
	if err != nil {
		return nil, fmt.Errorf("unable to parse fn %q body: %w", name, err)
	}

	n := len(params)
	var vparam string
	variadic := n > 0 && strings.HasPrefix(params[n-1], "*")
	if variadic {
		n--
		vparam, params = params[n], params[:n]
	}

	bindingScope := tcl
	fn := func(tcl *Interp, args Values) (last Values, err error) {
		tcl = tcl.Fork()
		tcl.overlays = append(tcl.overlays, bindingScope)

		argc := len(args)
		if variadic && argc < n {
			return nil, fmt.Errorf("%s: expected at least %d arguments, got %d", name, n, argc)
		} else if !variadic && argc != n {
			return nil, fmt.Errorf("%s: expected %d arguments, got %d", name, n, argc)
		}

		for i, param := range params {
			tcl.SetVar(param, Values{args[i]})
		}
		if variadic {
			tcl.SetVar(vparam, args[n:])
		}

		for _, cmd := range cmds {
			last, err = tcl.Do(cmd)
			if err != nil {
				break
			}
		}
		if isReturn(err) {
			err = nil
		}
		return last, err
	}

	if named {
		fnScope.Bind(name.String(), CmdFunc(fn))
	}

	return Values{&Func{Fn: CmdFunc(fn)}}, nil
}

func PreludeTrue(tcl *Interp, args Values) (Values, error) {
	return Values{True}, nil
}

func PreludeFalse(tcl *Interp, args Values) (Values, error) {
	return Values{False}, nil
}

func PreludeTap(tcl *Interp, args Values) (Values, error) {
	return args, nil
}

func isErrKindFunc(target error) CmdFunc {
	return func(tcl *Interp, args Values) (Values, error) {
		out := make(Values, len(args))
		for i, arg := range args {
			if err, ok := arg.(error); ok && errors.Is(err, target) {
				out[i] = arg
			} else {
				out[i] = Empty()
			}
		}
		return out, nil
	}
}
