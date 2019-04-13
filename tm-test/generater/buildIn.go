package generater

import (
	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

var (
	zero = constant.NewInt(types.I32, 0)
)

func (f *Func) checkBuildIn(name string, values ...value.Value) (*ir.InstCall, bool) {
	hasEnter := false
	var fn *Func
	switch name {
	case "println":
		hasEnter = true
		fallthrough
	case "print":
		initBuildInFunc(f.p, "puts")
		fn = f.p.funcsMap["puts"]
		var ret *ir.InstCall
		for i, v := range values {
			if i > 0 {
				ptr := constant.NewGetElementPtr(f.p.GlobalStr(" "), zero, zero)
				l := constant.NewInt(types.I32, 2)
				// puts(" ", 2)
				ret = f.b.NewCall(fn.f, ptr, l)
			}
			// puts(ptr, len)
			vv := v.(*ir.Global)
			ptr := constant.NewGetElementPtr(vv, zero, zero)
			l := constant.NewInt(types.I32,
				int64((vv.ContentType.(*types.ArrayType)).Len))
			ret = f.b.NewCall(fn.f, ptr, l)
		}
		if hasEnter {
			ptr := constant.NewGetElementPtr(f.p.GlobalStr("\n"), zero, zero)
			l := constant.NewInt(types.I32, 2)
			// puts(" ", 2)
			ret = f.b.NewCall(fn.f, ptr, l)
		}
		return ret, true
	default:
		return nil, false
	}
	return f.b.NewCall(fn.f, values...), true
}

func initBuildInFunc(p *Pkg, name string) {
	if p.buildIn[name] {
		return
	}
	p.buildIn[name] = true
	p.ExternFunc("puts", types.I32, types.I8Ptr, types.I32)
}
