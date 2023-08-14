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

func (s *Seq) Kind() ValueKind {
	return DataKind
}

func (s *Seq) Convert(kind ValueKind) (Value, error) {
	if kind == DataKind {
		return s, nil
	}
	return nil, conversionError(s, kind)
}

func (s *Seq) String() string {
	return fmt.Sprintf("%v:%v:%v", s.Start, s.End, s.Step)
}

func (*Seq) value() {}

func (*Seq) Type() string { return "seq" }

func (s *Seq) tooLarge() {
	panic(fmt.Errorf("cannot represent seq %v as a list", s))
}

func (s *Seq) Expand() (values Values) {
	iter := s.Iterator()
	numVals := s.Len()
	if !numVals.Num.IsInt64() {
		s.tooLarge()
		return nil
	}
	numVals64 := numVals.Num.Int64()
	if numVals64 > math.MaxInt {
		s.tooLarge()
		return nil
	}

	values = make(Values, 0, int(numVals64))
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

func (s *Seq) Len() *Int {
	pos := new(big.Int).Set(&s.Start.Num)
	end := &s.End.Num
	dir := end.Cmp(pos)
	if dir == 0 {
		var seqlen Int
		seqlen.Num.SetInt64(1)
		return &seqlen
	}

	step := &s.Step.Num
	if step.Sign() != dir {
		return new(Int) // Cannot compute.
	}

	delta := new(big.Int).Sub(end, pos)
	delta, mod := delta.DivMod(delta, step, new(big.Int))
	if mod.Sign() == 0 {
		delta = delta.Add(delta, big.NewInt(1))
	}
	var seqlen Int
	seqlen.Num.Set(delta)
	return &seqlen
}

func (s *Seq) Iterator() Iterator {
	pos := new(big.Int).Set(&s.Start.Num)
	end := &s.End.Num
	dir := end.Cmp(pos)
	if dir == 0 {
		return EmptyIterator()
	}

	step := &s.Step.Num
	if step.Sign() != dir {
		return EmptyIterator()
	}

	next := func() *Int {
		if end.Cmp(pos) == -dir {
			return nil
		}

		n := &Int{}
		n.Num.Set(pos)
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
