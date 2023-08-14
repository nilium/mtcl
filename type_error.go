package mtcl

type Error struct {
	Err error
}

func (*Error) Kind() ValueKind {
	return DataKind
}

func (e *Error) Convert(kind ValueKind) (Value, error) {
	if kind == DataKind {
		return e, nil
	}

	val, err := String(e.String()).Convert(kind)
	if err == nil {
		return val, nil
	}
	return nil, conversionError(e, kind)
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

func (*Error) Len() *Int {
	return NewInt(1)
}

func (e *Error) Type() string {
	return "error"
}
