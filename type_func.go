package mtcl

type Func struct {
	Fn    Cmd
	Binds Values
}

func (*Func) value()         {}
func (*Func) String() string { return "func" }
func (*Func) Type() string   { return "func" }

func (fn *Func) Expand() Values {
	return Values{fn}
}

func (*Func) Len() int {
	return 1
}

func (fn *Func) Call(tcl *Interp, args Values) (Values, error) {
	if len(fn.Binds) > 0 {
		args = append(append(make(Values, 0, len(args)+len(fn.Binds)), fn.Binds...), args...)
	}
	return fn.Fn.Call(tcl, args)
}
