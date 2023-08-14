package mtcl

import (
	"math"
	"strconv"
	"unicode/utf8"
)

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

func (s String) Len() *Int {
	return NewInt(utf8.RuneCountInString(string(s)))
}

func (s String) FloatValue() Float {
	sf, err := strconv.ParseFloat(string(s), 64)
	if err != nil {
		return Float(math.NaN())
	}
	return Float(sf)
}

func (s String) IntValue() *Int {
	var n Int
	if _, ok := n.Num.SetString(string(s), 0); !ok {
		return nil
	}
	return &n
}

func (s String) Kind() ValueKind {
	return DataKind
}

func (s String) Convert(kind ValueKind) (Value, error) {
	switch kind {
	case DataKind:
		return s, nil
	case BoolKind:
		return Bool(s != ""), nil
	case IntKind:
		if n := s.IntValue(); n != nil {
			return n, nil
		}
	case FloatKind:
		f, err := strconv.ParseFloat(string(s), 64)
		if err == nil {
			return Float(f), nil
		}
		if n := s.IntValue(); n != nil {
			return n.FloatValue(), nil
		}
	}
	return nil, conversionError(s, kind)
}
