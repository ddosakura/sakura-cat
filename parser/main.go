package main

import (
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"strconv"

	"github.com/llir/llvm/ir"
)

var (
	mm      *ir.Module
	pkgName string
)

func parserFile(m *ir.Module, filename string, info os.FileInfo, workpath string) error {
	f, e := os.Open(filename)
	if e != nil {
		return e
	}
	mm = m
	pkgName = ""
	var lex *Lexer
	lex = NewLexer(f)
	for callParse(filename, lex) {
	}
	return nil
}

func parserDir(root string, workpath string) error {
	m := ir.NewModule()
	m.SourceFilename = root
	e := filepath.Walk(root, func(p string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return parserDir(p, path.Join(workpath, info.Name()))
		}
		return parserFile(m, p, info, workpath)
	})
	if e != nil {
		return e
	}
	return ioutil.WriteFile(workpath+".ll", []byte(m.String()), 0644)
}

// Parser source files
func Parser(root string, workpath string) error {
	info, e := os.Stat(root)
	if e != nil {
		return e
	}
	if info.IsDir() {
		return parserDir(root, workpath)
	}
	m := ir.NewModule()
	m.SourceFilename = root
	e = parserFile(m, root, info, path.Join(workpath, info.Name()))
	if e != nil {
		return e
	}
	return ioutil.WriteFile(workpath+".ll", []byte(m.String()), 0644)
}

func main() {
	var path string
	path = os.Args[1]
	// os.MkdirAll("./pkg", 755)
	Parser(path, "./pkg")
}

func callParse(filename string, lex *Lexer) (b bool) {
	defer func() {
		if e := recover(); e != nil {
			err := errors.New(fmt.Sprint(e))

			fmt.Printf("%s:%d:%d: %s\n", filename, lex.Line()+1, lex.Column()+1, err.Error())

			if lex.Line() > 0 || lex.Column() > 0 {
				b = true
			}
		}
	}()
	if lex != nil {
		yyParse(lex)
	}
	return false
}

func unquote(s string) string {
	a, e := strconv.Unquote(`"` + s + `"`)
	if e != nil {
		panic(e)
	}
	return a
}
