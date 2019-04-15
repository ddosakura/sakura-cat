package main

import (
	"io/ioutil"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/types"
)

func main() {
	m := ir.NewModule()

	puts := m.NewFunc("_skr_puts", types.Void,
		ir.NewParam("", types.I8Ptr),
		ir.NewParam("", types.I32))

	p1 := ir.NewParam("s", types.I8Ptr)
	p2 := ir.NewParam("l", types.I32)
	f := m.NewFunc("print", types.Void, p1, p2)
	b := f.NewBlock("")
	b.NewCall(puts, p1, p2)
	b.NewRet(nil)

	ioutil.WriteFile("io.ll", []byte(m.String()), 0644)
	// pretty.Println(m)
}
