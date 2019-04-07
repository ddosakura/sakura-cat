package main

import (
	"fmt"
	"io/ioutil"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/types"
)

func tmp() {
	VOID := types.Void
	INT := types.I64

	TMP := constant.NewInt(INT, 2)

	m := ir.NewModule()
	m.SourceFilename = "main.src"
	str := m.NewGlobalDef(".str", constant.NewCharArray([]byte{
		'%', 'l', 'l', 'd', '\n', 0,
	}))
	printf := m.NewFunc("printf", types.I32, ir.NewParam("", types.I8Ptr))

	ip := m.NewFunc("ip", INT)
	ipb := ip.NewBlock("input")
	ipb.NewRet(TMP)

	op := m.NewFunc("op", VOID, ir.NewParam("x", INT))
	opb := op.NewBlock("output")
	opb.NewCall(printf, str, TMP)
	// see: llvm/ir/terminator.go
	opb.NewRet(nil)

	addx := ir.NewParam("x", INT)
	addy := ir.NewParam("y", INT)
	add := m.NewFunc("add", INT, addx, addy)
	addb := add.NewBlock("add")
	tmp := addb.NewAdd(addx, addy)
	addb.NewRet(tmp)

	main := m.NewFunc("main", INT)
	mainb := main.NewBlock("main")
	a := mainb.NewCall(ip)
	b := mainb.NewCall(ip)
	c := mainb.NewCall(add, a, b)
	mainb.NewCall(op, c)
	mainb.NewRet(constant.NewInt(INT, 0))

	fmt.Println(m)

	ioutil.WriteFile("main.ll", []byte(m.String()), 0644)
}
