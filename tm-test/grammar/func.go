package grammar

import (
	"errors"
	"strconv"
)

func operAdd(a interface{}, b interface{}) interface{} {
	return a.(int) + b.(int)
}

func atoi(s string) (n int) {
	n, _ = strconv.Atoi(s)
	return
}

var (
	EoiInCommit = errors.New("EOI in commit")
)

func unquote(s string) string {
	a, e := strconv.Unquote(`"` + s + `"`)
	if e != nil {
		panic(e)
	}
	return a
}
func strUnquote(s string) string {
	a, e := strconv.Unquote(s)
	if e != nil {
		panic(e)
	}
	return a
}

type BaseType uint8

const (
	BtNil BaseType = iota
	BtNum
	BtStr
	BtBool
)

type BaseValue struct {
	T BaseType
	V interface{}
}
