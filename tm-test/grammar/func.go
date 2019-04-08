package grammar

import "strconv"

func operAdd(a interface{}, b interface{}) interface{} {
	return a.(int) + b.(int)
}

func atoi(s string) (n int) {
	n, _ = strconv.Atoi(s)
	return
}
