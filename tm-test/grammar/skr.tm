language llvm(go);

lang = "llvm"
package = "github.com/ddosakura/sakura-cat/tm-test/grammar"
eventBased = true
reportTokens = [comment, invalid_token]
eventFields = true
eventAST = true

# ### [ Lexical part ]
:: lexer

_bin_digit = /[0-1]/
_oct_digit = /[0-7]/
_dec_digit = /[0-9]/
_hex_digit = /{_decimal_digit}|[A-Fa-f]/

comment : /[\/\/][^\r\n]*/               (space)
whitespace : /[\x00 \t\r\n]+/         (space)

'package': /package/ 1 {
    // println(l.Text())
}
'func': /func/ 1 {
    // println(l.Text())
}

# === [ Identifiers ]
ident: /[a-zA-Z_][a-zA-z_0-9]*/ {
    // println("ident", l.Text())
}
# --- [ Global identifiers ]
#global_ident: /[A-Z]([a-zA-Z_])*/ {
#    println("gi", l.Text())
#}
# --- [ Local identifiers ]
#local_ident: /[a-z_]([a-zA-Z_])*/ {
#    println("li", l.Text())
#}
# --- [ Labels ]
# --- [ Attribute group identifiers ]
# --- [ Comdat identifiers ]
# --- [ Metadata identifiers ]

# === [ Integer literals ]
# === [ Floating-point literals ]
# === [ String literals ]
# === [ Types ]

# === Opers etc.

'+': /[+]/
'-': /[-]/
'*': /[*]/
#'/': /[\/]/
':=': /:=/

',' : /[,]/
'!' : /[!]/
'...' : /\.\.\./
'(' : /[(]/
')' : /[)]/
'[' : /[\[]/
']' : /[\]]/
'{' : /[{]/
'}' : /[}]/
#'*' : /[*]/
'<' : /[<]/
'=' : /[=]/
'>' : /[>]/
'|' : /[|]/



num: /{_dec_digit}+/ {
    // println("num", l.Text())
}
#num: /{_dec_digit}+/ {
#    $$ = atoi(l.Text())
#}
#oper: /[+\-*\/]/ {
#    $$ = int(l.Text()[0])
#}

str: /("([^"\\]|\\.)*")|('([^'\\]|\\.)*')/



# ### [ Syntax part ]
:: parser
%input Package;

Package -> Package
    : 'package' PackageName Func*
;
PackageName -> PackageName: ident;

Func -> Func
    : 'func' FuncName '(' ')' '{' Stat* '}'
;
FuncName -> FuncName: ident;

Stat -> Stat
    : AssignStat | CallStat
;
VarName -> VarName: ident;
AssignStat -> AssignStat
    : VarName '=' Expr
    | VarName ':=' Expr
;
CallStat -> CallStat
    : FuncName '(' ArgListopt ')'
;
ArgList -> ArgList
    : Expr
    | Expr ',' ArgList
;

Expr -> Expr
    : Atom
    | Expr '+' Atom
    | Expr '-' Atom
;

Atom -> Atom: str | num | ident;
