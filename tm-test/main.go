package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"github.com/ddosakura/sakura-cat/tm-test/generater"
	"github.com/ddosakura/sakura-cat/tm-test/grammar"
	"github.com/ddosakura/sakura-cat/tm-test/grammar/ast"
	"github.com/ddosakura/sakura-cat/tm-test/grammar/selector"
	"github.com/kr/pretty"
	"github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

func main1() {
	l := &grammar.Lexer{}
	// l.Init("34\n100+2")
	l.Init("1+1\n6\n4-2")

	p := &grammar.Parser{}
	p.Init(func(t grammar.NodeType, offset, endoffset int) {
		println(t, offset, endoffset)
	})

	if e := p.Parse(l); e != nil {
		se := e.(grammar.SyntaxError)
		fmt.Printf("stdin:%v:%v: syntax error", se.Line, se.Offset+1)
	}
}

func main() {
	filepath.Walk("./test", func(p string, info os.FileInfo, err error) error {
		if info.IsDir() {
			return nil
		}
		fmt.Println(p)
		buf, e := ioutil.ReadFile(p)
		if e != nil {
			panic(e)
		}
		t, e := ast.Parse(info.Name(), string(buf))
		if e != nil {
			pretty.Println(e)
			return nil
		}
		// pretty.Println(t)
		root := t.Root()
		tmp(0, root)

		pkgName := root.Child(selector.PackageName).Text()
		pkg := generater.NewPkg().Name(pkgName)

		funcs := root.Children(selector.Func)
		fs := make(map[string]*generater.Func)
		for _, astF := range funcs {
			tmp := astF.Child(selector.FuncName)
			fName := tmp.Text()
			f := pkg.DefineFunc(fName, types.Void)
			if fs[fName] != nil {
				l, c := tmp.LineColumn()
				fmt.Printf("%v:%v:%v: func has be defined\n", p, l, c)
				return nil
			}
			fs[fName] = f
		}
		for _, astF := range funcs {
			fName := astF.Child(selector.FuncName).Text()
			f := fs[fName]
			stats := astF.Children(selector.Stat)
			for _, astS := range stats {
				stat := astS.Child(selector.Any)
				switch stat.Type() {
				case grammar.CallStat:
					fN := stat.Child(selector.FuncName).Text()
					// fmt.Println(fN)
					argList := stat.Child(selector.ArgList)
					args := make([]value.Value, 0)
					for argList.IsValid() {
						exprs := argList.Child(selector.Expr)
						args = append(args, expr2Value(pkg, exprs))
						argList = argList.Child(selector.ArgList)
					}
					f.CallFunc(fN, args...)
				default:
					l, c := stat.LineColumn()
					fmt.Printf("%v:%v:%v: unknow stat\n", p, l, c)
					return nil
				}
			}
			f.Block().NewRet(nil)
		}

		m := pkg.Module()
		ioutil.WriteFile("test.ll", []byte(m.String()), 0644)
		return nil
	})
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
