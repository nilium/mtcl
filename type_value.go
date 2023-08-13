package mtcl

import (
	"strconv"
	"strings"
)

type Value interface {
	value()

	Len() int
	String() string
	Type() string
	Expand() Values
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

func (vs Values) Expand() Values {
	return vs
}

func (vs Values) Len() int {
	return len(vs)
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
		return val.Sign() != 0
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
	case *Int:
		return false
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
