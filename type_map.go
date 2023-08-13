package mtcl

import (
	"errors"
	"strings"

	"golang.org/x/exp/maps"
	"golang.org/x/exp/slices"
)

type Map map[String]Value

func (m Map) value() {}

func (m Map) Type() string {
	return "dict"
}

func (m Map) Expand() Values {
	keys := maps.Keys(m)
	slices.Sort(keys)
	values := make(Values, len(keys))
	for i, key := range keys {
		ki := i * 2
		vi := ki + 1
		values[ki], values[vi] = String(key), m[key]
	}
	return values
}

func (m Map) Len() int {
	return len(m)
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
