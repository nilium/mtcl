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

func (Bool) Len() *Int {
	return NewInt(1)
}

func (b Bool) Type() string {
	return "bool"
}

func (b Bool) guaranteedFloat() {}

func (b Bool) FloatValue() Float {
	if b {
		return Float(1)
	}
	return Float(0)
}

func (b Bool) guaranteedInt() {}

func (b Bool) IntValue() *Int {
	var n Int
	if b {
		n.Num.SetInt64(1)
	}
	return &n
}

func (b Bool) Kind() ValueKind {
	return BoolKind
}

func (b Bool) Convert(kind ValueKind) (Value, error) {
	switch kind {
	case DataKind:
		return String(b.String()), nil
	case BoolKind:
		return b, nil
	case IntKind:
		return b.IntValue(), nil
	case FloatKind:
		return b.FloatValue(), nil
	}
	return nil, conversionError(b, kind)
}
