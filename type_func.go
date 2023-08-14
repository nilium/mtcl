package mtcl

type Func struct {
	Fn    Cmd
	Binds Values
}

func (*Func) value()         {}
func (*Func) String() string { return "func" }
func (*Func) Type() string   { return "func" }

func (fn *Func) Kind() ValueKind {
	return FuncKind
}

func (fn *Func) Convert(kind ValueKind) (Value, error) {
	if kind == FuncKind {
		return fn, nil
	}
	return nil, conversionError(fn, kind)
}

func (fn *Func) Expand() Values {
	return Values{fn}
}

func (*Func) Len() *Int {
	return NewInt(1)
}

func (fn *Func) Call(tcl *Interp, args Values) (Values, error) {
	if len(fn.Binds) > 0 {
		args = append(append(make(Values, 0, len(args)+len(fn.Binds)), fn.Binds...), args...)
	}
	return fn.Fn.Call(tcl, args)
}
