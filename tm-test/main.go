package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/ddosakura/sakura-cat/tm-test/generater"
	"github.com/ddosakura/sakura-cat/tm-test/grammar"
	"github.com/ddosakura/sakura-cat/tm-test/grammar/ast"
	"github.com/ddosakura/sakura-cat/tm-test/grammar/selector"
	"github.com/kr/pretty"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

func main1() {
	l := &grammar.Lexer{}
	// l.Init("34\n100+2")
	l.Init("1+1\n6\n4-2")

	p := &grammar.Parser{}
	p.Init(func(err grammar.SyntaxError) bool {
		pretty.Println("lexer", err)
		return true
	}, func(t grammar.NodeType, offset, endoffset int) {
		println(t, offset, endoffset)
	})

	if e := p.Parse(l); e != nil {
		se := e.(grammar.SyntaxError)
		fmt.Printf("stdin:%v:%v: syntax error", se.Line, se.Offset+1)
	}
}

func main() {
	// SkrBin := os.Getenv("SKRBIN")
	// SkrCache := os.Getenv("SKRCACHE")
	SkrPath := os.Getenv("SKRPATH")
	// SkrMod := os.Getenv("SKRMOD")

	mod := SkrPath + "/test1"
	bufs := make([]byte, 0)
	files := make([]string, 0)

	filepath.Walk(mod, func(p string, info os.FileInfo, err error) error {
		if info.IsDir() {
			return nil
		}
		fmt.Println(p, path.Ext(p))
		if path.Ext(p) != ".skr" {
			return nil
		}
		buf, e := ioutil.ReadFile(p)
		if e != nil {
			panic(e)
		}
		bufs = append(bufs, buf...)
		files = append(files, info.Name())
		return nil
	})

	t, e := ast.Parse(mod, string(bufs), func(err grammar.SyntaxError) bool {
		pretty.Println("parser", err)
		return true
	})
	if e != nil {
		pretty.Println(e)
		return
	}
	// pretty.Println(t)
	root := t.Root()
	tmp(0, root)

	pkgs := root.Children(selector.Package)
	pkg := generater.NewPkg()
	fs := make(map[string]*generater.Func)
	for i, pkgRoot := range pkgs {
		firstLen, _ := pkgRoot.LineColumn()
		firstLen--
		p := mod + "/" + files[i]

		tmp := pkgRoot.Child(selector.PackageName)
		pkgName := tmp.Text()
		if i == 0 {
			pkg.Name(pkgName)
		} else {
			ok := pkg.CheckName(pkgName)
			if !ok {
				_, c := tmp.LineColumn()
				fmt.Printf("%v:1:%v: package name should be same\n", p, c)
				return
			}
		}

		defines := pkgRoot.Children(selector.Define)
		for _, astD := range defines {
			astF := astD.Child(selector.Func)
			tmp := astF.Child(selector.FuncName)
			fName := tmp.Text()
			f := pkg.DefineFunc(fName, types.Void)
			if fs[fName] != nil {
				l, c := tmp.LineColumn()
				fmt.Printf("%v:%v:%v: func has be defined\n", p, l-firstLen, c)
			}
			fs[fName] = f
		}
		for _, astD := range defines {
			astF := astD.Child(selector.Func)
			fName := astF.Child(selector.FuncName).Text()
			f := fs[fName]
			stats := astF.Children(selector.Stat)
			for _, astS := range stats {
				stat := astS.Child(selector.Any)
				switch stat.Type() {
				case grammar.CallStat:
					fN := stat.Child(selector.FuncName).Text()
					// fmt.Println(fN)
					argList := stat.Children(selector.Expr)
					args := make([]value.Value, 0)
					for _, expr := range argList {
						args = append(args, expr2Value(pkg, expr))
					}
					f.CallFunc(fN, args...)
				case grammar.AssignStat:
					vN := stat.Child(selector.VarName).Text()
					expr := stat.Child(selector.Expr)
					v, t := parserExpr(expr)
					val := f.Block().NewAlloca(t)
					f.Block().NewStore(v, val)
					f.ValMap[vN] = val
				default:
					l, c := stat.LineColumn()
					fmt.Printf("%v:%v:%v: unknow stat\n", p, l-firstLen, c)
					return
				}
			}
			f.Block().NewRet(nil)
		}
	}

	m := pkg.Module()
	// println(m.String())
	ioutil.WriteFile("test.ll", []byte(m.String()), 0644)
}

func unquote(s string) string {
	a, e := strconv.Unquote(`"` + s + `"`)
	if e != nil {
		panic(e)
	}
	return a
}

func expr2Value(p *generater.Pkg, e *ast.Node) value.Value {
	s := e.Text()
	s = unquote(s[1 : len(s)-1])
	return p.GlobalStr(s)
}

func parserExpr(e *ast.Node) (value.Value, types.Type) {
	v, _ := strconv.Atoi(e.Text())
	t := types.I32
	return constant.NewInt(t, int64(v)), t
}

func tmp(dep int, n *ast.Node) {
	space(dep)
	fmt.Println(n.LineColumn())
	space(dep)
	fmt.Println(n.Type(), n.IsValid(),
		strings.ReplaceAll(n.Text(), "\n", "\\n"))
	for _, c := range n.Children(selector.Any) {
		tmp(dep+1, c)
	}
}

func space(dep int) {
	for dep > 0 {
		dep--
		fmt.Print("  ")
	}
}
