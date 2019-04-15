package generater

import (
	"strconv"

	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/types"
)

type Pkg struct {
	m            *ir.Module
	funcsMap     map[string]*Func
	globalStrMap map[string]*ir.Global

	buildIn map[string]bool
}

func NewPkg() *Pkg {
	return &Pkg{
		m:            ir.NewModule(),
		funcsMap:     make(map[string]*Func),
		globalStrMap: make(map[string]*ir.Global),
		buildIn:      make(map[string]bool),
	}
}

func (p *Pkg) Name(n string) *Pkg {
	p.m.SourceFilename = n
	return p
}

func (p *Pkg) CheckName(n string) bool {
	return p.m.SourceFilename == n
}

func (p *Pkg) Module() *ir.Module {
	return p.m
}

func (p *Pkg) ExternFunc(name string, retType types.Type, pTypes ...types.Type) *ir.Func {
	params := make([]*ir.Param, 0, len(pTypes))
	for _, t := range pTypes {
		params = append(params, ir.NewParam("", t))
	}

	f := &Func{
		p: p,
		f: p.m.NewFunc(name, retType, params...),
	}
	p.funcsMap[name] = f
	return f.f
}

func (p *Pkg) DefineFunc(name string, retType types.Type, params ...*ir.Param) *Func {
	f := &Func{
		p: p,
		f: p.m.NewFunc(name, retType, params...),
	}
	p.funcsMap[name] = f

	f.argsMap = make(map[string]*ir.Param)
	for _, param := range params {
		f.argsMap[param.Name()] = param
	}

	f.init()
	return f
}

func (p *Pkg) GlobalStr(name string) *ir.Global {
	gd := p.globalStrMap[name]
	if gd == nil {
		v := []byte(name)
		v = append(v, 0)
		gd = p.m.NewGlobalDef(
			".str-"+strconv.Itoa(len(p.globalStrMap)),
			constant.NewCharArray(v))
		p.globalStrMap[name] = gd
	}
	return gd
}
