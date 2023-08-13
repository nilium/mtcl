package mtcl

import "math/big"

type Int struct {
	big.Int
}

func NewInt(x int) *Int {
	var n Int
	n.SetInt64(int64(x))
	return &n
}

var _ Value = (*Int)(nil)

func (*Int) value() {}

func (n *Int) Expand() Values {
	return Values{n}
}

func (*Int) Len() int {
	return 1
}

func (n *Int) Type() string {
	return "int"
}
