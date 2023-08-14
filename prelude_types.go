package mtcl

import (
	"errors"
	"fmt"
	"math"
	"strconv"

	"golang.org/x/exp/slices"
)

func PreludeIsError(tcl *Interp, args Values) (Values, error) {
	out := make(Values, len(args))
	for i, arg := range args {
		if _, ok := arg.(error); ok {
			out[i] = arg
		} else {
			out[i] = Empty()
		}
	}
	return out, nil
}

func PreludeType(tcl *Interp, args Values) (Values, error) {
	types := make(Values, len(args))
	for i, arg := range args {
		types[i] = String(arg.Type())
	}
	return types, nil
}

func PreludeInt(tcl *Interp, args Values) (Values, error) {
	out := make(Values, len(args))
	for i, arg := range args {
		switch arg := arg.(type) {
		case GuaranteedIntValue:
			out[i] = arg.IntValue()
		case IntValue:
			n := arg.IntValue()
			if n == nil {
				return nil, fmt.Errorf("cannot convert %v %q to integer", arg.Type(), arg)
			}
			out[i] = n
		default:
			var n Int
			str := arg.String()
			if _, ok := n.Num.SetString(str, 0); !ok {
				return nil, fmt.Errorf("cannot convert %v %q as string as integer", arg.Type(), str)
			}
			out[i] = &n
		}
	}
	return out, nil
}

func PreludeFloat(tcl *Interp, args Values) (Values, error) {
	out := make(Values, len(args))
	for i, arg := range args {
		switch arg := arg.(type) {
		case GuaranteedFloatValue:
			out[i] = arg.FloatValue() // Allows NaN.
		case FloatValue:
			f := arg.FloatValue()
			if math.IsNaN(float64(f)) {
				return nil, fmt.Errorf("cannot convert %v %q to float", arg.Type(), arg)
			}
			out[i] = f
		default:
			str := arg.String()
			f, err := strconv.ParseFloat(str, 64)
			if err != nil {
				return nil, fmt.Errorf("cannot convert %v %q as string to float: %w", arg.Type(), str, err)
			}
			out[i] = Float(f)
		}
	}
	return out, nil
}

func PreludeStr(tcl *Interp, args Values) (Values, error) {
	out := make(Values, len(args))
	for i, arg := range args {
		out[i] = String(arg.String())
	}
	return out, nil
}

func PreludeSeq(tcl *Interp, args Values) (Values, error) {
	args, err := PreludeInt(tcl, args)
	if err != nil {
		return nil, err
	}
	var seq *Seq
	switch len(args) {
	case 1: // seq start=0 end step=1|-1
		seq = &Seq{
			Start: new(Int),
			End:   args[0].(*Int),
			Step:  new(Int),
		}
		switch seq.End.Num.Sign() {
		case -1:
			seq.Start.Num.SetInt64(-1)
		case 1:
			seq.Start.Num.SetInt64(1)
		}

	case 2: // seq start end step=1|-1
		seq = &Seq{
			Start: args[0].(*Int),
			End:   args[1].(*Int),
			Step:  new(Int),
		}
	case 3: // seq start end step
		seq = &Seq{
			Start: args[0].(*Int),
			End:   args[1].(*Int),
			Step:  args[2].(*Int),
		}
		return Values{seq}, nil
	default:
		return nil, fmt.Errorf("seq: expected 1..3 arguments, got %d", len(args))
	}
	if seq.End.Num.Cmp(&seq.Start.Num) < 0 {
		seq.Step.Num.SetInt64(-1)
	} else {
		seq.Step.Num.SetInt64(1)
	}
	return Values{seq}, nil
}

func PreludeList(tcl *Interp, args Values) (Values, error) {
	return Values{slices.Clone(args)}, nil
}

func PreludeDict(tcl *Interp, args Values) (Values, error) { // Doesn't really do anything.
	n := len(args)
	if n%2 != 0 {
		return nil, errors.New("dict: expected arguments of the form dict key pair")
	}
	dict := make(Map, n/2)
	for len(args) > 0 {
		k, v := String(args[0].String()), args[1]
		args = args[2:]
		dict[k] = v
	}
	return Values{dict}, nil
}
