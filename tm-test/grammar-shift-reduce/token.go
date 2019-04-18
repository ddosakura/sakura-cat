// generated by Textmapper; DO NOT EDIT

package grammar

import (
	"fmt"
)

// Token is an enum of all terminal symbols of the skr language.
type Token int

// Token values.
const (
	UNAVAILABLE Token = iota-1
	EOI
	INVALID_TOKEN
	ERROR
	WHITESPACE
	MULTILINECOMMENT
	SINGLELINECOMMENT
	IDENTIFIER
	PACKAGE // package
	SKR // skr
	GO // go
	ASM // asm
	EXTERN // extern
	STRUCT // struct
	AWAIT // await
	BREAK // break
	CASE // case
	CATCH // catch
	CLASS // class
	CONST // const
	CONTINUE // continue
	DEBUGGER // debugger
	DEFAULT // default
	DELETE // delete
	DO // do
	ELSE // else
	EXPORT // export
	EXTENDS // extends
	FINALLY // finally
	FOR // for
	FUNC // func
	IF // if
	IMPORT // import
	IN // in
	INSTANCEOF // instanceof
	NEW // new
	RETURN // return
	SUPER // super
	SWITCH // switch
	THIS // this
	THROW // throw
	TRY // try
	TYPEOF // typeof
	VAR // var
	WHILE // while
	WITH // with
	YIELD // yield
	ENUM // enum
	AS // as
	ASYNC // async
	FROM // from
	GET // get
	LET // let
	OF // of
	SET // set
	STATIC // static
	TARGET // target
	IMPLEMENTS // implements
	INTERFACE // interface
	PRIVATE // private
	PROTECTED // protected
	PUBLIC // public
	ABSTRACT // abstract
	CONSTRUCTOR // constructor
	DECLARE // declare
	IS // is
	MODULE // module
	NAMESPACE // namespace
	REQUIRE // require
	TYPE // type
	READONLY // readonly
	KEYOF // keyof
	VOID // void
	SYMBOL // symbol
	STRING // string
	BOOL // bool
	BYTE // byte
	UINTPTR // uintptr
	INT // int
	INT8 // int8
	INT16 // int16
	INT32 // int32
	INT64 // int64
	UINT // uint
	UINT8 // uint8
	UINT16 // uint16
	UINT32 // uint32
	UINT64 // uint64
	FLOAT32 // float32
	FLOAT64 // float64
	COMPLEX // complex
	COMPLEX64 // complex64
	COMPLEX128 // complex128
	COLONASSIGN // :=
	LBRACE // {
	RBRACE // }
	LPAREN // (
	RPAREN // )
	LBRACK // [
	RBRACK // ]
	DOT // .
	DOTDOT // ..
	DOTDOTDOT // ...
	SEMICOLON // ;
	COMMA // ,
	LT // <
	GT // >
	LTASSIGN // <=
	GTASSIGN // >=
	ASSIGNASSIGN // ==
	EXCLASSIGN // !=
	ASSIGNASSIGNASSIGN // ===
	EXCLASSIGNASSIGN // !==
	ATSIGN // @
	PLUS // +
	MINUS // -
	MULT // *
	DIV // /
	REM // %
	PLUSPLUS // ++
	MINUSMINUS // --
	LTLT // <<
	GTGT // >>
	GTGTGT // >>>
	AND // &
	OR // |
	XOR // ^
	EXCL // !
	TILDE // ~
	ANDAND // &&
	OROR // ||
	QUEST // ?
	COLON // :
	ASSIGN // =
	PLUSASSIGN // +=
	MINUSASSIGN // -=
	MULTASSIGN // *=
	DIVASSIGN // /=
	REMASSIGN // %=
	LTLTASSIGN // <<=
	GTGTASSIGN // >>=
	GTGTGTASSIGN // >>>=
	ANDASSIGN // &=
	ORASSIGN // |=
	XORASSIGN // ^=
	ASSIGNGT // =>
	MULTMULT // **
	MULTMULTASSIGN // **=
	NULL
	NULL1 // null
	NIL // nil
	TRUE // true
	FALSE // false
	NUMERICLITERAL
	STRINGLITERAL
	RESOLVESHIFT

	NumTokens
)

var tokenStr = [...]string{
	"EOI",
	"INVALID_TOKEN",
	"ERROR",
	"WHITESPACE",
	"MULTILINECOMMENT",
	"SINGLELINECOMMENT",
	"IDENTIFIER",
	"package",
	"skr",
	"go",
	"asm",
	"extern",
	"struct",
	"await",
	"break",
	"case",
	"catch",
	"class",
	"const",
	"continue",
	"debugger",
	"default",
	"delete",
	"do",
	"else",
	"export",
	"extends",
	"finally",
	"for",
	"func",
	"if",
	"import",
	"in",
	"instanceof",
	"new",
	"return",
	"super",
	"switch",
	"this",
	"throw",
	"try",
	"typeof",
	"var",
	"while",
	"with",
	"yield",
	"enum",
	"as",
	"async",
	"from",
	"get",
	"let",
	"of",
	"set",
	"static",
	"target",
	"implements",
	"interface",
	"private",
	"protected",
	"public",
	"abstract",
	"constructor",
	"declare",
	"is",
	"module",
	"namespace",
	"require",
	"type",
	"readonly",
	"keyof",
	"void",
	"symbol",
	"string",
	"bool",
	"byte",
	"uintptr",
	"int",
	"int8",
	"int16",
	"int32",
	"int64",
	"uint",
	"uint8",
	"uint16",
	"uint32",
	"uint64",
	"float32",
	"float64",
	"complex",
	"complex64",
	"complex128",
	":=",
	"{",
	"}",
	"(",
	")",
	"[",
	"]",
	".",
	"..",
	"...",
	";",
	",",
	"<",
	">",
	"<=",
	">=",
	"==",
	"!=",
	"===",
	"!==",
	"@",
	"+",
	"-",
	"*",
	"/",
	"%",
	"++",
	"--",
	"<<",
	">>",
	">>>",
	"&",
	"|",
	"^",
	"!",
	"~",
	"&&",
	"||",
	"?",
	":",
	"=",
	"+=",
	"-=",
	"*=",
	"/=",
	"%=",
	"<<=",
	">>=",
	">>>=",
	"&=",
	"|=",
	"^=",
	"=>",
	"**",
	"**=",
	"NULL",
	"null",
	"nil",
	"true",
	"false",
	"NUMERICLITERAL",
	"STRINGLITERAL",
	"RESOLVESHIFT",
}

func (tok Token) String() string {
	if tok >= 0 && int(tok) < len(tokenStr) {
		return tokenStr[tok]
	}
	return fmt.Sprintf("token(%d)", tok)
}
