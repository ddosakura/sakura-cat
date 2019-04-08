language llvm(go);

lang = "llvm"
package = "github.com/ddosakura/sakura-cat/tm-test/grammar"
eventBased = true
eventFields = true
eventAST = true

# lexer

:: lexer

_ascii_letter_upper = /[A-Z]/

_ascii_letter_lower = /[a-z]/

_ascii_letter = /{_ascii_letter_upper}|{_ascii_letter_lower}/

_letter = /{_ascii_letter}|[-$\._]/

_escape_letter = /{_letter}|[\\]/

_decimal_digit = /[0-9]/

_hex_digit = /{_decimal_digit}|[A-Fa-f]/

comment : /[;][^\r\n]*/               (space)
whitespace : /[\x00 \t\r\n]+/         (space)

num: /{_decimal_digit}+/ {
    $$ = atoi(l.Text())
}
oper: /[+\-*\/]/ {
    $$ = int(l.Text()[0])
}

# parser

:: parser
%input Stat;

Stat -> Stat
    : Expr*
;

Expr {int} -> Expr
    : num {
        fmt.Printf("%v\n", $0)
    }
    | num oper num {
        $$ = operAdd($0, $2)
        fmt.Printf("%v\n", $$)
    }
;
