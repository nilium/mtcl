package mtcl

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

func (*Error) Len() int {
	return 1
}

func (e *Error) Type() string {
	return "error"
}
