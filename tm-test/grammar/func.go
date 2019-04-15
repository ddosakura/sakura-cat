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
