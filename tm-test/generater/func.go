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
}

func (f *Func) init() {
	f.b = f.f.NewBlock("")
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
