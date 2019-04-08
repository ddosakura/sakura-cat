package main

import (
	"fmt"

	"github.com/ddosakura/sakura-cat/tm-test/grammar"
)

func main() {
	l := &grammar.Lexer{}
	l.Init("34\n100+2")

	p := &grammar.Parser{}
	p.Init(func(t grammar.NodeType, offset, endoffset int) {
		println(t, offset, endoffset)
	})

	if e := p.Parse(l); e != nil {
		se := e.(grammar.SyntaxError)
		fmt.Printf("stdin:%v:%v: syntax error", se.Line, se.Offset+1)
	}
}
