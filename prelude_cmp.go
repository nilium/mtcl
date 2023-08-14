package mtcl

func preludeTypedOp[T Value](tcl *Interp, args Values, ensure func(*Interp, Values) (Values, error), cmp func(T, T) bool) (results Values, err error) {
	if ensure != nil {
		args, err = ensure(tcl, args)
		if err != nil {
			return nil, err
		}
	}
	prev, ok := args[0].(T)
	if !ok {
		return Values{False}, nil
	}
	for _, arg := range args[1:] {
		sub, ok := arg.(T)
		if !ok || !cmp(prev, sub) {
			return Values{False}, nil
		}
		prev = sub
	}
	return Values{True}, nil
}

func PreludeLesser(tcl *Interp, args Values) (Values, error) {
	if len(args) < 2 {
		return Values{True}, nil
	}
	switch args[0].(type) {
	case String:
		return preludeTypedOp(tcl, args, nil, func(l, r Value) bool { return l.String() < r.String() })
	case *Int:
		return preludeTypedOp(tcl, args, PreludeInt, func(l, r *Int) bool { return l.Num.Cmp(&r.Num) < 0 })
	default:
		return Values{False}, nil
	}
}

func PreludeLesserEqual(tcl *Interp, args Values) (Values, error) {
	if len(args) < 2 {
		return Values{True}, nil
	}
	switch args[0].(type) {
	case String:
		return preludeTypedOp(tcl, args, nil, func(l, r Value) bool { return l.String() <= r.String() })
	case *Int:
		return preludeTypedOp(tcl, args, PreludeInt, func(l, r *Int) bool { return l.Num.Cmp(&r.Num) <= 0 })
	default:
		return Values{False}, nil
	}
}

func PreludeGreater(tcl *Interp, args Values) (Values, error) {
	if len(args) < 2 {
		return Values{True}, nil
	}
	switch args[0].(type) {
	case String:
		return preludeTypedOp(tcl, args, nil, func(l, r Value) bool { return l.String() > r.String() })
	case *Int:
		return preludeTypedOp(tcl, args, PreludeInt, func(l, r *Int) bool { return l.Num.Cmp(&r.Num) > 0 })
	default:
		return Values{False}, nil
	}
}

func PreludeGreaterEqual(tcl *Interp, args Values) (Values, error) {
	if len(args) < 2 {
		return Values{True}, nil
	}
	switch args[0].(type) {
	case String:
		return preludeTypedOp(tcl, args, nil, func(l, r Value) bool { return l.String() >= r.String() })
	case *Int:
		return preludeTypedOp(tcl, args, PreludeInt, func(l, r *Int) bool { return l.Num.Cmp(&r.Num) >= 0 })
	default:
		return Values{False}, nil
	}
}

func PreludeEqual(tcl *Interp, args Values) (Values, error) {
	if len(args) < 2 {
		return Values{True}, nil
	}
	switch args[0].(type) {
	case String:
		return preludeTypedOp(tcl, args, nil, func(l, r Value) bool { return l.String() == r.String() })
	case *Int:
		return preludeTypedOp(tcl, args, PreludeInt, func(l, r *Int) bool { return l.Num.Cmp(&r.Num) == 0 })
	default:
		return Values{False}, nil
	}
}

func PreludeNotEqual(tcl *Interp, args Values) (Values, error) {
	if len(args) < 2 {
		return Values{True}, nil
	}
	switch args[0].(type) {
	case String:
		return preludeTypedOp(tcl, args, nil, func(l, r Value) bool { return l.String() != r.String() })
	case *Int:
		return preludeTypedOp(tcl, args, PreludeInt, func(l, r *Int) bool { return l.Num.Cmp(&r.Num) != 0 })
	default:
		return Values{False}, nil
	}
}
