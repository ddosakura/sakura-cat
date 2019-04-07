/package/   { return TPACKAGE }
/import/    { return TIMPORT }

/type/      { return TTYPE }
/interface/ { return TINTERFACE }
/struct/    { return TSTRUCT }
/var/       { return TVAR }
/const/     { return TCONST }
/map/       { return TMAP }
			
/if/            { return TIF }
/else/          { return TELSE }
/switch/        { return TSWITCH }
/select/        { return TSELECT }
/case/          { return TCASE }
/fallthrough/   { return TFALLTHROUGH }
/default/       { return TDEFAULT }

/for/       { return TFOR }
/range/     { return TRANGE }
/break/     { return TBREAK }
/continue/  { return TCONTINUE }

/func/      { return TFUNC }
/defer/     { return TDEFER }
/return/    { return TRETURN }

/go/    { return TGO }
/chan/  { return TCHAN }

/("([^"\\]|\\.)*")|('([^'\\]|\\.)*')/ {
    lval.s = unquote(yylex.Text()[1:len(yylex.Text())-1])
    return STR
}
/[a-zA-Z_][a-zA-Z0-9_]*/ {
    lval.s = yylex.Text()
    return VAL
}

/\/\/[^\n]*/ {
    lval.s = yylex.Text()[2:]
    return COMMENT
}
/[ \t\r]/ {}
/\n/ {
    return ENTER
}
/./ {
    return int(yylex.Text()[0])
}
//

package main
/* import("strconv") */
