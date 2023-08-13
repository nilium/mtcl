package mtcl

import (
	"fmt"
	"math"
	"math/big"
)

type Seq struct {
	Start *Int
	End   *Int
	Step  *Int
}

func (s *Seq) String() string {
	return fmt.Sprintf("%v:%v:%v", s.Start, s.End, s.Step)
}

func (*Seq) value() {}

func (*Seq) Type() string { return "seq" }

func (s *Seq) Expand() (values Values) {
	iter := s.Iterator()
	values = make(Values, 0, s.Len())
	var temp [1]Value
	for {
		ok, err := iter(Values(temp[:]))
		if err != nil {
			return nil
		} else if !ok {
			break
		}
		values = append(values, temp[0])
	}
	return values
}

func (s *Seq) Len() int {
	pos := new(big.Int).Set(&s.Start.Int)
	end := &s.End.Int
	dir := end.Cmp(pos)
	if dir == 0 {
		return 1
	}

	step := &s.Step.Int
	if step.Sign() != dir {
		return 0 // Cannot compute.
	}

	delta := new(big.Int).Sub(end, pos)
	delta, mod := delta.DivMod(delta, step, new(big.Int))
	if mod.Sign() == 0 {
		delta = delta.Add(delta, big.NewInt(1))
	}
	if !delta.IsInt64() {
		return math.MaxInt64
	}
	estimate := delta.Int64()
	if estimate < 0 {
		estimate = -estimate
	}
	if estimate > math.MaxInt32 {
		return math.MaxInt32
	}
	return int(estimate)
}

func (s *Seq) Iterator() Iterator {
	pos := new(big.Int).Set(&s.Start.Int)
	end := &s.End.Int
	dir := end.Cmp(pos)
	if dir == 0 {
		return EmptyIterator()
	}

	step := &s.Step.Int
	if step.Sign() != dir {
		return EmptyIterator()
	}

	next := func() *Int {
		if end.Cmp(pos) == -dir {
			return nil
		}

		n := &Int{}
		n.Set(pos)
		pos = pos.Add(pos, step)
		return n
	}

	return func(out Values) (ok bool, err error) {
		for i := range out {
			n := next()
			if n == nil {
				return false, nil
			}
			out[i] = n
		}
		return true, nil
	}
}
