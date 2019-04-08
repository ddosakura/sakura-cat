%{
package main

import ("fmt";"os")
%}

%union {
    s string
}

%token TPACKAGE
%token TIMPORT

%token TTYPE
%token TINTERFACE
%token TSTRUCT
%token TVAR
%token TCONST
%token TMAP

%token TIF
%token TELSE
%token TSWITCH
%token TSELECT
%token TCASE
%token TFALLTHROUGH
%token TDEFAULT

%token TFOR
%token TRANGE
%token TBREAK
%token TCONTINUE

%token TFUNC
%token TDEFER
%token TRETURN

%token TGO
%token TCHAN


%token STR
%token VAL

%token COMMENT
%token ENTER

%%
prog: DefinePackage pass ImportPackages pass Defines pass;

pass: passable | pass passable | ;
passable: COMMENT | ENTER ;

DefinePackage: TPACKAGE VAL ENTER {
	println($2.s)
};

ImportPackages: ImportPackage {

} | ImportPackages pass ImportPackage {

} | ;
ImportPackage: TIMPORT STR ENTER {

} | TIMPORT '(' STRs ')' {

};
STRs: STR {

} | STRs STR {

};

Defines: Definable | Defines pass Definable | ;
Definable: DefineType | DefineFunc;

DefineType: TTYPE VAL ValType;

DefineFunc: TFUNC obj VAL '(' args ')' rets block {
	println($3.s)
};
obj: '(' VAL ValType ')' {

} | ;
args: arg {

} | args ',' arg {

} | ;
arg: VAL ValType {

};
rets: ValType | '(' retlist ')' | ;
retlist: ValType ',' ValType | retlist ',' ValType;
block: '{' ENTER stats ENTER '}';
stats: ';' | stat | stats pass stat;
stat: VAL '(' exprs ')' {

};

ValType: VAL | TSTRUCT '{' '}' | TINTERFACE '{' '}';

exprs: expr | exprs ',' expr | ;
expr: STR | VAL

%%

func emit(format string, a ...interface{}) {
	fmt.Fprintf(os.Stdout,format, a...);
	fmt.Fprintln(os.Stdout,"")
}
func die(format string, a ...interface{}) {
    panic(fmt.Sprintf(format, a...))
}
