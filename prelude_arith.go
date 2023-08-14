package mtcl

import (
	"fmt"
)

type ArithValue interface {
	Value

	Signer
	NegOp
	AddOp
	SubOp
	MulOp
	DivOp
	PowOp
	ModOp
}

// In all cases, x is the receiver and y is the argument.

type (
	Signer interface {
		Value
		Sign() int // x => -1, 0, 1
	}

	NegOp interface {
		Value
		Neg() Value // unary - x
	}

	AddOp interface {
		Value
		Add(Value) Value // + x y
	}

	SubOp interface {
		Value
		Sub(Value) Value // - x y
	}

	MulOp interface {
		Value
		Mul(Value) Value // * x y
	}

	DivOp interface {
		Value
		Div(Value) Value // / x y
	}

	PowOp interface {
		Value
		Pow(Value) Value // pow x y
	}

	ModOp interface {
		Value
		Mod(Value) Value // mod x y
	}
)

func preludeBinArith[T Value](name string, args Values, op func(T, Value) Value) (Values, error) {
	if len(args) <= 1 {
		return Values{}, nil
	}

	var maxKind ValueKind
	for _, arg := range args {
		maxKind = max(maxKind, arg.Kind())
	}

	rhs := args[0]
	rhs, err := rhs.Convert(maxKind)
	if err != nil {
		return nil, fmt.Errorf("could not convert rhs to kind %v: %w", maxKind, err)
	}

	rhsArith, ok := rhs.(T)
	if !ok {
		return nil, fmt.Errorf("rhs %v (%T) does not implement %v", rhs.Type(), rhs, name)
	}

	result, err := args[1:].Map(func(lhs Value) (Value, error) {
		lhs, err := lhs.Convert(maxKind)
		if err != nil {
			return nil, err
		}
		return orError(op(rhsArith, lhs))
	})
	if err != nil {
		return nil, err
	}
	return result.(Values), nil
}

func PreludeAdd(tcl *Interp, args Values) (results Values, err error) {
	return preludeBinArith("add", args, AddOp.Add)
}

func PreludeSub(tcl *Interp, args Values) (results Values, err error) {
	if len(args) == 1 {
		arg := args[0]
		switch av := arg.(type) {
		case NegOp:
			return Values{av.Neg()}, nil
		case Mapper:
			mr, err := av.Map(func(v Value) (Value, error) {
				if s, ok := v.(NegOp); ok {
					return s.Neg(), nil
				}
				return nil, fmt.Errorf("%T does not implement Signer", v)
			})
			if err != nil {
				return nil, err
			}
			return Values{mr}, nil
		default:
			return nil, fmt.Errorf("cannot negate value of type %v", av.Type())
		}
	}
	return preludeBinArith("sub", args, SubOp.Sub)
}

func PreludeMul(tcl *Interp, args Values) (results Values, err error) {
	return preludeBinArith("mul", args, MulOp.Mul)
}

func PreludeDiv(tcl *Interp, args Values) (results Values, err error) {
	if len(args) == 1 {
		args = Values{Float(1.0), args[0]}
	}
	return preludeBinArith("div", args, DivOp.Div)
}

func PreludePow(tcl *Interp, args Values) (results Values, err error) {
	return preludeBinArith("pow", args, PowOp.Pow)
}

func PreludeMod(tcl *Interp, args Values) (results Values, err error) {
	return preludeBinArith("mod", args, ModOp.Mod)
}

func vecOp[T Value](name string, lhs Values, other Value, op func(T, Value) Value) Value {
	var args [2]Value
	switch rhs := other.(type) {
	case Values:
		if ll, rl := len(lhs), len(rhs); ll != rl {
			return &Error{fmt.Errorf("rhs and lhs lengths mismatched: %d != %d", ll, rl)}
		}

		iter := rhs.Iterator()
		return takeError(lhs.Map(func(lcol Value) (Value, error) {
			args := args[:]
			args[0] = lcol
			_, _ = iter(args[1:])
			vals, err := preludeBinArith(name, args, op)
			if err != nil {
				return nil, err
			} else if len(vals) == 0 {
				return nil, fmt.Errorf("no value produced from %v op", name)
			}
			return orError(vals[0])
		}))
	default:
		var args [2]Value
		args[1] = rhs
		return takeError(lhs.Map(func(lcol Value) (Value, error) {
			args := args[:]
			args[0] = lcol
			vals, err := preludeBinArith(name, args, op)
			if err != nil {
				return nil, err
			} else if len(vals) == 0 {
				return nil, fmt.Errorf("no value produced from %v op", name)
			}
			return orError(vals[0])
		}))
	}
}

func (vs Values) Neg() Value {
	return takeError(vs.Map(func(v Value) (Value, error) {
		if n, ok := v.(NegOp); ok {
			return n.Neg(), nil
		}
		return nil, fmt.Errorf("%v does not implement NegOp", v.Type())
	}))
}

func (vs Values) Add(other Value) Value {
	return vecOp("add", vs, other, AddOp.Add)
}

func (vs Values) Sub(other Value) Value {
	return vecOp("sub", vs, other, SubOp.Sub)
}

func (vs Values) Mul(other Value) Value {
	return vecOp("mul", vs, other, MulOp.Mul)
}

func (vs Values) Div(other Value) Value {
	return vecOp("div", vs, other, DivOp.Div)
}

func (vs Values) Pow(other Value) Value {
	return vecOp("pow", vs, other, PowOp.Pow)
}

func (vs Values) Mod(other Value) Value {
	return vecOp("mod", vs, other, ModOp.Mod)
}
