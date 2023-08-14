package mtcl

import (
	"fmt"
	"math"
	"strconv"
	"strings"

	"golang.org/x/exp/slices"
)

func takeError[T Value](value T, err error) Value {
	if err != nil {
		return &Error{Err: err}
	}
	return value
}

func orError(val Value) (Value, error) {
	if err, ok := val.(error); ok {
		return nil, err
	}
	return val, nil
}

type Value interface {
	value()

	Kind() ValueKind
	Convert(kind ValueKind) (Value, error)

	String() string
	Type() string
	Expand() Values
}

type Mapper interface {
	Map(tform func(Value) (Value, error)) (Value, error)
}

type ConversionError struct {
	Value  Value
	Target ValueKind
}

func conversionError(val Value, target ValueKind) *ConversionError {
	return &ConversionError{Value: val, Target: target}
}

func (c *ConversionError) Error() string {
	return fmt.Sprintf("unable to convert %v (%T) to %v", c.Value.Type(), c.Value, c.Target)
}

// ValueKind denotes the ordinal kind of an arithmetic-supporting value. The
// higher of two value kinds is the type that both values will be converted
// to for an operation. Func kind values are not convertible to other
// kinds. Data, like a string, may not be convertible to an upper type. Bool
// kinds can always be converted upward. For most operations, bool values are
// saturating. IntKind may be convertible upwards but accuracy and validity
// are not guaranteed (for example, the resulting float may become Inf or
// NaN). Floats are the maximal type at this time.
//
// If two values are found to be data or less, some operators may fail.
//
// Conversion rules are such that FuncKind and DataKind may return opaque
// types. Most of the time, DataKind is a string, but may be something
// that doesn't support most operations (such as a map). If either of these
// types are converted to and their resulting values implement ArithValue,
// they *must* perform type checks. If converting to BoolKind, IntKind, or
// FloatKind, the result must be a Bool, *Int, or Float respectively. Bool,
// Int, and Float are not required to check the type of their LHS values in
// arithmetic operations if conversion succeeds.
type ValueKind int

const (
	FuncKind ValueKind = iota
	DataKind
	BoolKind
	IntKind
	FloatKind
)

func (v ValueKind) String() string {
	switch v {
	case DataKind:
		return "data"
	case BoolKind:
		return "bool"
	case IntKind:
		return "int"
	case FloatKind:
		return "float"
	}
	return "invalid"
}

type LengthValue interface {
	Value

	Len() *Int
}

type Values []Value

func Empty() Values {
	return nil
}

func (Values) value() {}

func (vs Values) String() string {
	switch len(vs) {
	case 0:
		return ""
	case 1:
		return vs[0].String()
	default:
		var out strings.Builder
		for i, v := range vs {
			str := v.String()
			if str == "" {
				continue
			}
			out.Grow(len(str) + 1)
			if i > 0 && out.Len() > 0 {
				out.WriteByte(' ')
			}
			out.WriteString(str)
		}
		return out.String()
	}
}

func (vs Values) Map(mapfn func(Value) (Value, error)) (result Value, err error) {
	vs = slices.Clone(vs)
	for i, v := range vs {
		vs[i], err = mapfn(v)
		if err != nil {
			return nil, fmt.Errorf("error mapping value %d: %w", i+1, err)
		}
	}
	return vs, nil
}

func (vs Values) Kind() ValueKind {
	if len(vs) == 0 {
		return DataKind
	}
	var maxKind ValueKind
	for _, v := range vs {
		maxKind = max(maxKind, v.Kind())
	}
	return maxKind
}

func (vs Values) Convert(kind ValueKind) (result Value, err error) {
	return vs.Map(func(v Value) (Value, error) {
		return v.Convert(kind)
	})
}

func (vs Values) Expand() Values {
	return vs
}

func (vs Values) Len() *Int {
	return NewInt(len(vs))
}

func (vs Values) Type() string {
	return "vec"
}

func (vs Values) Iterator() Iterator {
	return func(vals Values) (bool, error) {
		if len(vals) == 0 {
			return len(vs) > 0, nil
		}
		if len(vs) < len(vals) {
			return false, nil
		}
		copy(vals, vs)
		vs = vs[len(vals):]
		return true, nil
	}
}

type Iterable interface {
	Value
	Iterator() Iterator
}

type Iterator func(vals Values) (bool, error)

func EmptyIterator() Iterator {
	return func(Values) (bool, error) {
		return false, nil
	}
}

type OnceIterable struct {
	Value
}

func (o OnceIterable) Iterator() Iterator {
	if IsEmpty(o.Value) {
		return EmptyIterator()
	}
	return OnceIterator(o.Value)
}

func OnceIterator(val Value) Iterator {
	var iter Iterator
	iter = func(vals Values) (bool, error) {
		if len(vals) == 0 {
			return true, nil
		}
		iter = func(vals Values) (bool, error) { return false, nil }
		if len(vals) != 1 {
			return false, nil
		}
		vals[0] = val
		return true, nil
	}
	return func(vals Values) (bool, error) {
		return iter(vals)
	}
}

func Truthy(val Value) bool {
	if IsEmpty(val) {
		return false
	}
	switch val := val.(type) {
	case *Int:
		return val.Num.Sign() != 0
	case Float:
		return float64(val) != 0 && !math.IsNaN(float64(val))
	case Bool:
		return bool(val)
	case String:
		b, err := strconv.ParseBool(string(val))
		return b || err != nil
	case Values:
		for _, sub := range val {
			if Truthy(sub) {
				return true
			}
		}
	}
	return false
}

func IsEmpty(val Value) bool {
	switch val := val.(type) {
	case String:
		return len(val) == 0
	case Values:
		return len(val) == 0
	case Map:
		return len(val) == 0
	case Bool:
		return !bool(val)
	case nil:
		return true
	}
	return false
}
