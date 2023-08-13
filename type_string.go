package mtcl

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

func (s String) Len() int {
	return len(s)
}
