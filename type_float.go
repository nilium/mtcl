package mtcl

import (
	"math"
	"math/big"
	"strconv"
)

type FloatValue interface {
	Value
	// Float may return NaN for values that cannot be represented
	// as floats.
	FloatValue() Float
}

// GuaranteedFloatValue means that the represented value will always return
// a valid float if its Float method is called. This does not mean that the
// float is not NaN but that if it is NaN it is not due to the result of
// converting the value at hand into a float.
type GuaranteedFloatValue interface {
	FloatValue
	guaranteedFloat()
}

// LitFloat is a float converted from a literal.
type LitFloat struct {
	Float
	Literal *Literal
}

func (l LitFloat) String() string {
	return l.Literal.Literal
}

var _ GuaranteedFloatValue = LitFloat{}

type Float float64

var _ GuaranteedFloatValue = Float(0)
var _ ArithValue = Float(0)

func (Float) value()           {}
func (Float) guaranteedFloat() {}

func (Float) Type() string { return "float" }

func (f Float) String() string {
	return strconv.FormatFloat(float64(f), 'f', -1, 64)
}

func (f Float) Kind() ValueKind {
	return FloatKind
}

func (f Float) Convert(kind ValueKind) (Value, error) {
	switch kind {
	case DataKind:
		return String(f.String()), nil
	case BoolKind:
		return Bool(f != 0), nil
	case IntKind:
		return f.Int(), nil
	case FloatKind:
		return f, nil
	}
	return nil, conversionError(f, kind)
}

func (f Float) Map(mapfn func(Value) (Value, error)) (Value, error) {
	return mapfn(f)
}

func (f Float) Expand() Values {
	return Values{f}
}

func (f Float) FloatValue() Float {
	return f
}

func (f Float) Int() *Int {
	var n Int
	bf := big.NewFloat(float64(f))
	bf.Int(&n.Num)
	return &n
}

func floatOp(op func(Float, Value) Value, f Float, other Value, combine func(float64, float64) float64) Value {
	if math.IsNaN(float64(f)) {
		return f
	}
	switch other := other.(type) {
	case FloatValue:
		of := other.FloatValue()
		if math.IsNaN(float64(of)) {
			return of
		}
		return Float(combine(float64(f), float64(of)))
	case Values:
		sub := make(Values, len(other))
		for i, val := range other {
			sub[i] = op(f, val)
		}
		return sub
	}
	return Float(math.NaN())
}

func (f Float) Sign() int {
	if f > 0 {
		return 1
	} else if f < 0 {
		return -1
	}
	return 0
}

func (f Float) Neg() Value {
	return -f
}

func (f Float) Add(other Value) Value {
	return floatOp(Float.Add, f, other, func(l, r float64) float64 { return l + r })
}

func (f Float) Sub(other Value) Value {
	return floatOp(Float.Sub, f, other, func(l, r float64) float64 { return l - r })
}

func (f Float) Mul(other Value) Value {
	return floatOp(Float.Mul, f, other, func(l, r float64) float64 { return l * r })
}

func (f Float) Div(other Value) Value {
	return floatOp(Float.Div, f, other, func(l, r float64) float64 { return l / r })
}

func (f Float) Pow(other Value) Value {
	return floatOp(Float.Pow, f, other, math.Pow)
}

func (f Float) Mod(other Value) Value {
	return floatOp(Float.Mod, f, other, math.Mod)
}
