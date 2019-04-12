package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"

	"github.com/ddosakura/sakura-cat/tm-test/grammar"
	"github.com/ddosakura/sakura-cat/tm-test/grammar/ast"
	"github.com/ddosakura/sakura-cat/tm-test/grammar/selector"
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
			panic(e)
		}
		// pretty.Println(t)
		tmp(0, t.Root())
		return nil
	})
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
