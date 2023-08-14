package mtcl

import (
	"errors"
	"math/big"
)

type IntValue interface {
	Value
	// Int may return nil for values that cannot be represented as an Int.
	IntValue() *Int
}

// GuaranteedIntValue means that the represented value will always return
// a valid integer if its Int method is called.
type GuaranteedIntValue interface {
	IntValue
	guaranteedInt()
}

type Int struct {
	Num big.Int
}

type LitInt struct {
	*Int
	*Literal
}

func (l LitInt) String() string {
	return l.Literal.Literal
}

func NewInt(x int) *Int {
	var n Int
	n.Num.SetInt64(int64(x))
	return &n
}

var _ GuaranteedIntValue = LitInt{}
var _ GuaranteedIntValue = (*Int)(nil)
var _ ArithValue = (*Int)(nil)

func (*Int) guaranteedInt() {}

func (*Int) value() {}

func (n *Int) String() string {
	return n.Num.String()
}

func (n *Int) Expand() Values {
	return Values{n}
}

func (n *Int) Type() string {
	return "int"
}

func (n *Int) FloatValue() Float {
	var bf big.Float
	bf.SetInt(&n.Num)
	of, _ := bf.Float64()
	return Float(of)
}

func (n *Int) IntValue() *Int {
	return n
}

func (n *Int) Kind() ValueKind {
	return IntKind
}

func (n *Int) Convert(kind ValueKind) (Value, error) {
	switch kind {
	case DataKind:
		return String(n.String()), nil
	case BoolKind:
		return Bool(n.Sign() != 0), nil
	case IntKind:
		return n, nil
	case FloatKind:
		return n.FloatValue(), nil
	}
	return nil, conversionError(n, kind)
}

func intOp(op func(*Int, Value) Value, fop func(Float, Value) Value, n *Int, other Value, combine func(out, l, r *big.Int) *big.Int) Value {
	// If the other side is a float, convert to float.
	if _, ok := other.(Float); ok {
		return fop(n.FloatValue(), other)
	}
	var out Int
	switch other := other.(type) {
	case GuaranteedIntValue:
		rv := other.IntValue()
		combine(&out.Num, &n.Num, &rv.Num)
		return &out
	case GuaranteedFloatValue:
		return fop(n.FloatValue(), other.FloatValue())
	case IntValue:
		rv := other.IntValue()
		if n != nil {
			combine(&out.Num, &n.Num, &rv.Num)
			return &out
		}
	case Values:
		sub := make(Values, len(other))
		for i, val := range other {
			sub[i] = op(n, val)
		}
		return sub
	}
	if fver, ok := other.(FloatValue); ok {
		fv := fver.FloatValue()
		return fop(n.FloatValue(), fv)
	}
	return String(n.String() + other.String())
}

// Sign implements ArithValue
func (n *Int) Sign() int {
	return n.Num.Sign()
}

// Neg implements ArithValue
func (n *Int) Neg() Value {
	var r Int
	r.Num.Neg(&n.Num)
	return &r
}

var errIntOp = errors.New("cannot extract int for op")

func opBinInt(n *Int, other Value, op func(out, l, r *big.Int) *big.Int) Value {
	switch other := other.(type) {
	case Mapper:
		return takeError(other.(Mapper).Map(func(rhs Value) (Value, error) {
			return orError(opBinInt(n, rhs, op))
		}))
	case *Int:
		var out Int
		op(&out.Num, &n.Num, &other.Num)
		return &out
	default:
		return &Error{Err: errIntOp}
	}
}

// Add implements ArithValue
func (n *Int) Add(other Value) Value {
	return opBinInt(n, other, (*big.Int).Add)
}

// Sub implements ArithValue
func (n *Int) Sub(other Value) Value {
	return opBinInt(n, other, (*big.Int).Sub)
}

// Mul implements ArithValue
func (n *Int) Mul(other Value) Value {
	return opBinInt(n, other, (*big.Int).Mul)
}

// Div implements ArithValue
func (n *Int) Div(other Value) Value {
	return opBinInt(n, other, (*big.Int).Div)
}

// Pow implements ArithValue
func (n *Int) Pow(other Value) Value {
	return takeError(other.(Mapper).Map(func(rhs Value) (Value, error) {
		var out Int
		out.Num.Exp(&n.Num, &rhs.(*Int).Num, nil)
		return &out, nil
	}))
}

func (n *Int) Mod(other Value) Value {
	return opBinInt(n, other, (*big.Int).Mod)
}
