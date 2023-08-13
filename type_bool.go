package mtcl

type Bool bool

const True Bool = true
const False Bool = false

func (Bool) value() {}

func (b Bool) String() string {
	if b {
		return "true"
	}
	return "false"
}

func (b Bool) Expand() Values {
	return Values{b}
}

func (Bool) Len() int {
	return 1
}

func (b Bool) Type() string {
	return "bool"
}
