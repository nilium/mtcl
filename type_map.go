package mtcl

import (
	"errors"
	"fmt"
	"strings"

	"golang.org/x/exp/maps"
	"golang.org/x/exp/slices"
)

type Map map[String]Value

func (m Map) value() {}

func (m Map) Type() string {
	return "dict"
}

func (m Map) Kind() ValueKind {
	if len(m) == 0 {
		return DataKind
	}
	var maxKind ValueKind
	for _, v := range m {
		maxKind = max(maxKind, v.Kind())
	}
	return maxKind
}

func (m Map) Map(mapfn func(Value) (Value, error)) (Value, error) {
	m = maps.Clone(m)
	for k, v := range m {
		v, err := mapfn(v)
		if err != nil {
			return nil, fmt.Errorf("error to mapping value of key %q: %w", k, err)
		}
		m[k] = v
	}
	return m, nil
}

func (m Map) Convert(kind ValueKind) (Value, error) {
	return m.Map(func(v Value) (Value, error) {
		return v.Convert(kind)
	})
}

func (m Map) Expand() Values {
	keys := maps.Keys(m)
	slices.Sort(keys)
	values := make(Values, len(keys))
	for i, key := range keys {
		values[i] = Values{key, m[key]}
	}
	return values
}

func (m Map) Len() *Int {
	return NewInt(len(m))
}

func (m Map) String() string {
	items := make([]string, 0, len(m)*2)
	for k, v := range m {
		items = append(items, string(k), v.String())
	}
	return strings.Join(items, " ")
}

func (m Map) Iterator() Iterator {
	keys := maps.Keys(m)
	slices.Sort(keys)
	return func(vals Values) (bool, error) {
		switch len(vals) {
		case 0:
			return len(keys) > 0, nil
		case 1, 2:
		default:
			return false, errors.New("map iterator only accepts one or two iterator parameters")
		}
		for i, key := range keys {
			val, ok := m[key]
			if !ok {
				continue
			}
			keys = keys[i+1:]
			switch len(vals) {
			case 2:
				vals[0], vals[1] = String(key), val
			case 1:
				vals[0] = String(key)
			}
			return true, nil
		}
		keys = nil
		return false, nil
	}
}
