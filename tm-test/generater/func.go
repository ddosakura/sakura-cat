package generater

import (
	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/value"
)

type Func struct {
	p       *Pkg
	f       *ir.Func
	b       *ir.Block
	argsMap map[string]*ir.Param

	// move to block later
	ValMap map[string]value.Value
}

func (f *Func) init() {
	f.b = f.f.NewBlock("")

	f.ValMap = make(map[string]value.Value)
}

func (f *Func) CallFunc(name string, values ...value.Value) *ir.InstCall {
	if ret, ok := f.checkBuildIn(name, values...); ok {
		return ret
	}
	fn := f.p.funcsMap[name]
	return f.b.NewCall(fn.f, values...)
}

func (f *Func) Block() *ir.Block {
	return f.b
}
