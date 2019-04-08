package main

import "github.com/davecgh/go-spew/spew"

func main() {
	t := []uint8{'H', 'e', 'l', 'l', 'o', '\n'}
	for i := 0; i < 5; i++ {
		t = append(t, t...)
	}
	spew.Dump(t)
}
