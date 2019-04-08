package main

import (
	"io/ioutil"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/types"
)

func main() {
	zero := constant.NewInt(types.I32, 0)

	m := ir.NewModule()
	text := []byte{
		'H', 'e', 'l', 'l', 'o', '\n', 0,
	}
	str := constant.NewCharArray(text)
	strPtr := constant.NewGetElementPtr(m.NewGlobalDef(".str", str), zero, zero)
	// strPtr.InBounds = true
	strLen := constant.NewInt(types.I32, int64(len(text)))

	pf := m.NewFunc("print", types.I32,
		ir.NewParam("", types.I8Ptr),
		ir.NewParam("", types.I32))

	f := m.NewFunc("main", types.I32)
	b := f.NewBlock("")
	b.NewCall(pf, strPtr, strLen)
	b.NewRet(zero)

	ioutil.WriteFile("test.ll", []byte(m.String()), 0644)
	// pretty.Println(m)
}
