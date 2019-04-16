# docs: http://textmapper.org/documentation.html
language skr(go);

lang = "skr"
package = "github.com/ddosakura/sakura-cat/tm-test/grammar"
eventBased = true
eventFields = true
eventAST = true
#fileNode = "Module"
#cancellable = true
#recursiveLookaheads = true
reportTokens = [MultiLineComment, SingleLineComment, invalid_token]
#reportTokens = [MultiLineComment, SingleLineComment, invalid_token, NoSubstitutionTemplate, TemplateHead, TemplateMiddle, TemplateTail]
#extraTypes = ["InsertedSemicolon"]



# ### [ Quick query ] unicode categories
# Lu             Letter, uppercase
# Ll             Letter, lowercase
# Lt             Letter, titlecase
# Lm             Letter, modifier
# Lo             Letter, other
# Mn             Mark, nonspacing
# Mc             Mark, spacing combining
# Me             Mark, enclosing
# Nd             Number, decimal digit
# Nl             Number, letter
# No             Number, other
# Pc             Punctuation, connector
# Pd             Punctuation, dash
# Ps             Punctuation, open
# Pe             Punctuation, close
# Pi             Punctuation, initial quote (Ps or Pe depending on usage)
# Pf             Punctuation, final quote (Ps or Pe depending on usage)
# Po             Punctuation, other
# Sm             Symbol, math
# Sc             Symbol, currency
# Sk             Symbol, modifier
# So             Symbol, other
# Zs             Separator, space
# Zl             Separator, line
# Zp             Separator, paragraph
# Cc             Other, control
# Cf             Other, format
# Cs             Other, surrogate
# Co             Other, private use
# Cn             Other, not assigned (including non-characters)

# ### [ Lexical part ]
:: lexer

#%s initial, div, template, templateDiv, templateExpr, templateExprDiv;
%s initial, div;
<*> eoi: /{eoi}/

invalid_token:
error:

#whitespace : /[\x00 \t\r\n]+/ (space)
#<initial, div, template, templateDiv, templateExpr, templateExprDiv> {
#    WhiteSpace: /[\t\x0b\x0c\x20\xa0\ufeff\p{Zs}]/ (space)
#}
<initial, div> {
    WhiteSpace: /[\t\x0b\x0c\x20\xa0\ufeff\p{Zs}]/ (space)
}
WhiteSpace: /[\n\r\u2028\u2029]|\r\n/ (space)

#comment : /[\/\/][^\r\n]*/               (space)
#%x inComment;
#commentStart: /\/\*/ 1 (space)    { l.State = StateInComment }
#<inComment> {
#  commentText: /\*|[^*]+/  (space)
#  commentEnd: /\*\// (space)   { l.State = StateInitial }
#  eoi: /{eoi}/     { panic(EoiInCommit); l.State = StateInitial }
#}
commentChars = /([^*]|\*+[^*\/])*\**/
MultiLineComment:  /\/\*{commentChars}\*\//     (space)
# Note: the following rule disables backtracking for incomplete multiline comments, which
# would otherwise be reported as '/', '*', etc.
invalid_token: /\/\*{commentChars}/
SingleLineComment: /\/\/[^\n\r\u2028\u2029]*/   (space)
# Shebang.
SingleLineComment: /#![^\n\r\u2028\u2029]*/   (space)

#bin = /[0-1]/
#oct = /[0-7]/
#dec = /[0-9]/
hex = /[0-9A-Fa-f]/

#ident: /[a-zA-Z_][a-zA-z_0-9]*/ {
#    // println("ident", l.Text())
#}
# Note: see http://unicode.org/reports/tr31/
IDStart = /\p{Lu}|\p{Ll}|\p{Lt}|\p{Lm}|\p{Lo}|\p{Nl}/
IDContinue = /{IDStart}|\p{Mn}|\p{Mc}|\p{Nd}|\p{Pc}/
JoinControl = /\u200c|\u200d/
unicodeEscapeSequence = /u(\{{hex}+\}|{hex}{4})/
brokenEscapeSequence = /\\(u({hex}{0,3}|\{{hex}*))?/
#identifierStart = /{IDStart}|$|_|\\{unicodeEscapeSequence}/
identifierStart = /{IDStart}|_|\\{unicodeEscapeSequence}/
identifierPart =  /{identifierStart}|{IDContinue}|{JoinControl}/
Identifier: /{identifierStart}{identifierPart}*/    (class)
# Note: the following rule disables backtracking for incomplete identifiers.
invalid_token: /({identifierStart}{identifierPart}*)?{brokenEscapeSequence}/

# Keywords.
'package':    /package/
'skr':    /skr/
'go':     /go/
'asm':    /asm/
'extern': /extern/

'await':      /await/
'break':      /break/
'case':       /case/
'catch':      /catch/
'class':      /class/
'const':      /const/
'continue':   /continue/
'debugger':   /debugger/
'default':    /default/
'delete':     /delete/
'do':         /do/
'else':       /else/
'export':     /export/
'extends':    /extends/
'finally':    /finally/
'for':        /for/
'func':       /func/
'if':         /if/
'import':     /import/
'in':         /in/
'instanceof': /instanceof/
'new':        /new/
'return':     /return/
'super':      /super/
'switch':     /switch/
'this':       /this/
'throw':      /throw/
'try':        /try/
'typeof':     /typeof/
'var':        /var/
'void':       /void/
'while':      /while/
'with':       /with/
'yield':      /yield/
# Future-reserved.
'enum':  /enum/
# Literals.
NULL:
'null': /null/ { token = NULL }
'nil': /nil/ { token = NULL }
'true':  /true/
'false': /false/
# Soft (contextual) keywords.
'as':     /as/
'async':  /async/
'from':   /from/
'get':    /get/
'let':    /let/
'of':     /of/
'set':    /set/
'static': /static/
'target': /target/
# etc. I
'implements':   /implements/
'interface':    /interface/
'private':      /private/
'protected':    /protected/
'public':       /public/
# etc. II
'any':     /any/
'boolean': /boolean/
'number':  /number/
# etc. III
'abstract':    /abstract/
'constructor': /constructor/
'declare':     /declare/
'is':          /is/
'module':      /module/
'namespace':   /namespace/
'require':     /require/
'type':        /type/
# etc. IV
'readonly': /readonly/
'keyof': /keyof/

# types
'void': /void/
'symbol':  /symbol/
'string': /string/
'bool': /bool/
'byte': /byte/
'uintptr': /uintptr/
'int': /int/
'int8': /int8/
'int16': /int16/
'int32': /int32/
'int64': /int64/
'uint': /uint/
'uint8': /uint8/
'uint16': /uint16/
'uint32': /uint32/
'uint64': /uint64/
'float32': /float32/
'float64': /float64/
'complex': /complex/
'complex64': /complex64/
'complex128': /complex128/

#BOOL {bool}:
#'true' {bool}: /true/ 1 {
#    token = BOOL
#    $$ = true
#    println("bool T")
#}
#'false' {bool}: /false/ 1 {
#    token = BOOL
#    $$ = false
#    println("bool F")
#}

# Punctuation
':=': /:=/
'{': /\{/
'}': /\}/
'(': /\(/
')': /\)/
'[': /\[/
']': /\]/
'.': /\./
#invalid_token: /\.\./
'..': /\.\./
'...': /\.\.\./
';': /;/
',': /,/
'<': /</
'>': />/
'<=': /<=/
'>=': />=/
'==': /==/
'!=': /!=/
'===': /===/
'!==': /!==/
'@': /@/
'+': /\+/
'-': /-/
'*': /\*/
'/': /\//
'%': /%/
'++': /\+\+/
'--': /--/
'<<': /<</
'>>': />>/
'>>>': />>>/
'&': /&/
'|': /\|/
'^': /^/
'!': /!/
'~': /~/
'&&': /&&/
'||': /\|\|/
'?': /\?/
':': /:/
'=': /=/
'+=': /\+=/
'-=': /-=/
'*=': /\*=/
'/=': /\/=/
'%=': /%=/
'<<=': /<<=/
'>>=': />>=/
'>>>=': />>>=/
'&=': /&=/
'|=': /\|=/
'^=': /^=/
'=>': /=>/
'**': /\*\*/
'**=': /\*\*=/

# Num
#bin = /[0-1]/
#oct = /[0-7]/
#dec = /[0-9]/
#hex = /[0-9A-Fa-f]/
#num: /{dec}+/ {
#    // println("num", l.Text())
#}
int = /(0+([0-7]*[89][0-9]*)?|[1-9][0-9]*)/ # 
frac = /\.[0-9]*/
exp = /[eE][+-]?[0-9]+/
bad_exp = /[eE][+-]?/
NumericLiteral: /{int}{frac}?{exp}?/# dec
NumericLiteral: /\.[0-9]+{exp}?/    # dec
NumericLiteral: /0[xX]{hex}+/# hex
NumericLiteral: /0[oO][0-7]+/# oct
NumericLiteral: /0+[0-7]+/ 1 # oct(Takes priority over the float rule above)
NumericLiteral: /0[bB][01]+/ # bin
invalid_token: /0[xXbBoO]/
invalid_token: /{int}{frac}?{bad_exp}/
invalid_token: /\.[0-9]+{bad_exp}/

# Str
#str: /("([^"\\]|\\.)*")|('([^'\\]|\\.)*')/ {
#	s := l.Text()
#	s = unquote(s[1 : len(s)-1])
#    // println("str", s)
#}
escape = /\\([^1-9xu\n\r\u2028\u2029]|x{hex}{2}|{unicodeEscapeSequence})/
lineCont = /\\([\n\r\u2028\u2029]|\r\n)/
dsChar = /[^\n\r"\\\u2028\u2029]|{escape}|{lineCont}/
ssChar = /[^\n\r'\\\u2028\u2029]|{escape}|{lineCont}/
# TODO check \0 is valid if [lookahead != DecimalDigit]
StringLiteral: /"{dsChar}*"/
StringLiteral: /'{ssChar}*'/

#tplChars = /([^\$`\\]|\$*{escape}|\$*{lineCont}|\$+[^\$\{`\\])*\$*/
#<initial, div, templateExpr, templateExprDiv> '}': /\}/
#<initial, div, template, templateDiv, templateExpr, templateExprDiv> {
#    NoSubstitutionTemplate: /`{tplChars}`/
#    TemplateHead: /`{tplChars}\$\{/
#}
#<template, templateDiv> {
#    TemplateMiddle: /\}{tplChars}\$\{/
#    TemplateTail: /\}{tplChars}`/
#}
#<div, templateDiv, templateExprDiv> {
#    '/': /\//
#    '/=': /\/=/
#}

# 按Go标准，反引号内无法转义反引号
MultiLineChars = /([^`]|{escape}|{lineCont})*/
StringLiteral: /`{MultiLineChars}`/

# For precedence resolution.
resolveShift:
#unaryMinus:



# Tokens
#   keyword(s)
#   Punctuation(s)
#
#   invalid_token
#   error
#   Identifier
#
#   NumericLiteral
#   StringLiteral
#
#   resolveShift
#   unaryMinus

# ### [ Syntax part ]
:: parser
%input Package;

%assert empty set(follow error & ~('}' | ')' | ',' | ';' | ']'));

%generate afterErr = set(follow error);

%flag In;

%flag WithoutNew = false;
%flag WithoutPredefinedTypes = false;
%flag WithoutImplements = false;

%lookahead flag NoLet = false;
%lookahead flag NoLetSq = false;
%lookahead flag NoObjLiteral = false;
%lookahead flag NoFuncClass = false;
%lookahead flag NoAs = false;
%lookahead flag StartWithLet = false;

SyntaxError -> SyntaxProblem
    : error
;

# === [ Skr Types ]

SkrType -> SkrType
    : 'void'
    | 'symbol'
    | 'string'
    | 'bool'
    | 'byte'
    | 'uintptr'

    | 'int'
    | 'int8'
    | 'int16'
    | 'int32'
    | 'int64'

    | 'uint'
    | 'uint8'
    | 'uint16'
    | 'uint32'
    | 'uint64'

    | 'float32'
    | 'float64'
    | 'complex'
    | 'complex64'
    | 'complex128'

    | CustomType
;
CustomType:
    Identifier
;

# === [ Identifier ]

# 标识符名
IdentifierName<WithoutNew>
    : Identifier
    # Keywords
    | [!WithoutNew] 'new'
    | 'await'
    | 'break'      | 'do'         | 'in'         | 'typeof'
    | 'case'       | 'else'       | 'instanceof' | 'var'
    | 'catch'      | 'export'     | 'void'
    | 'class'      | 'extends'    | 'return'     | 'while'
    | 'const'      | 'finally'    | 'super'      | 'with'
    | 'continue'   | 'for'        | 'switch'     | 'yield'
    | 'debugger'   | 'func'       | 'this'
    | 'default'    | 'if'         | 'throw'
    | 'delete'     | 'import'     | 'try'
    # Future-reserved.
    | 'enum'
    # NullLiteral | BooleanLiteral
    | NULL | 'true' | 'false'
    # Soft keywords
    | 'as'     | 'from' | 'get' | 'let' | 'of' | 'set' | 'static'
    | 'target' | 'async'
    # Typescript.
    | 'implements' | 'interface'   | 'private' | 'protected' | 'public'
    | 'any'        | 'boolean'     | 'number'  | 'string'    | 'symbol'
    | 'abstract'   | 'constructor' | 'declare' | 'is'        | 'module'
    | 'namespace'  | 'require'     | 'type'
    | 'readonly'   | 'keyof'
;

IdentifierNameDecl<WithoutNew>
    : IdentifierName -> BindingIdentifier
;

IdentifierNameRef
    : IdentifierName -> IdentifierReference
;

IdentifierReference<WithoutPredefinedTypes> -> IdentifierReference
    : Identifier
    | 'yield'
    | 'await'
    | [!NoLet] 'let'
    | 'async'
    # Soft keywords
    | 'as' | 'from' | 'get' | 'of' | 'set' | 'static' | 'target'
    # Typescript.
    | 'implements' | 'interface' | 'private' | 'protected' | 'public'
    | [!WithoutPredefinedTypes] ('any' | 'boolean' | 'number' | 'string' | 'symbol')
    | 'abstract' | 'constructor' | 'declare' | 'is' | 'module' | 'namespace' | 'require' | 'type'
    | 'readonly' | [!WithoutPredefinedTypes] 'keyof'
;

BindingIdentifier<WithoutImplements> -> BindingIdentifier
    : Identifier
    # These are allowed or not, depending on the context.
    | 'yield' | 'await'
    # Soft keywords
    | 'as' | 'from' | 'get' | 'let' | 'of' | 'set' | 'static'
    | 'target' |'async'
    # Typescript.
    | [!WithoutImplements] 'implements'
    | 'interface' | 'private' | 'protected' | 'public'
    | 'any' | 'boolean' | 'number' | 'string' | 'symbol'
    | 'abstract' | 'constructor' | 'declare' | 'is' | 'module'
    | 'namespace' | 'require' | 'type'
    | 'readonly' | 'keyof'
;

LabelIdentifier -> LabelIdentifier
    : Identifier
    # These are allowed or not, depending on the context.
    | 'yield' | 'await'
    # Soft keywords
    | 'as' | 'from' | 'get' | 'let' | 'of' | 'set' | 'static'
    | 'target' | 'async'
    # Typescript.
    | 'implements' | 'interface' | 'private' | 'protected' | 'public'
    | 'any' | 'boolean' | 'number' | 'string' | 'symbol'
    | 'abstract' | 'constructor' | 'declare' | 'is' | 'module'
    | 'namespace' | 'require' | 'type'
    | 'readonly' | 'keyof'
;

# === [ Expression ]
%interface Expression;

Expression<In> -> Expression /* interface */
    # 赋值表达式
    : AssignmentExpression
    # 逗号表达式
    | CommaExpression
;

PrimaryExpression -> Expression /* interface */
    : 'this' -> This
    | IdentifierReference
    | Literal
    | ArrayLiteral
    | [!NoObjLiteral] ObjectLiteral
    | [!NoFuncClass] FunctionExpression
    | [!NoFuncClass] ClassExpression
    | Parenthesized
;

Parenthesized -> Parenthesized
    : '(' Expression<+In> ')'
    | '(' SyntaxError ')'
;

# 字面量
Literal -> Literal
    : NULL -> NullLiteral
    | 'true' -> BoolLiteral
    | 'false' -> BoolLiteral
    | NumericLiteral -> NumLiteral
    | StringLiteral -> StrLiteral
;

# 数组字面量
ArrayLiteral -> ArrayLiteral
    : '[' Elisionopt ']'
    | '[' list=ElementList ']'
    | '[' list=ElementList ',' Elisionopt ']'
;

ElementList
    : Elisionopt AssignmentExpression<+In>
    | Elisionopt SpreadElement
    | ElementList ',' Elisionopt AssignmentExpression<+In>
    | ElementList ',' Elisionopt SpreadElement
;

Elision
    : ','
    | Elision ','
;

SpreadElement -> Expression /* interface */
    : '...' AssignmentExpression<+In> -> SpreadElement
;

# 对象字面量
ObjectLiteral -> ObjectLiteral
    : '{' '}'
    | '{' .recoveryScope PropertyDefinitionList '}'
    | '{' .recoveryScope PropertyDefinitionList ',' '}'
;

PropertyDefinitionList
    : PropertyDefinition
    | PropertyDefinitionList ',' PropertyDefinition
;

%interface PropertyName, PropertyDefinition;

PropertyDefinition -> PropertyDefinition /* interface */
    : IdentifierReference -> ShorthandProperty
    | Modifiers? PropertyName ':' value=AssignmentExpression<+In> -> Property
    | Modifiers? MethodDefinition -> ObjectMethod
    | CoverInitializedName -> SyntaxProblem
    | SyntaxError
    | '...' AssignmentExpression<+In> -> SpreadProperty
;

PropertyName<WithoutNew> -> PropertyName /* interface */
    : LiteralPropertyName
    | ComputedPropertyName
;

LiteralPropertyName<WithoutNew> -> LiteralPropertyName
    : IdentifierNameDecl
    | StringLiteral
    | NumericLiteral
;

ComputedPropertyName -> ComputedPropertyName
    : '[' AssignmentExpression<+In> ']'
;

CoverInitializedName
    : IdentifierReference Initializer<+In>
;

Initializer<In> -> Initializer
    : '=' AssignmentExpression
;

MemberExpression<flag NoLetOnly = false> -> Expression /* interface */
    : [!NoLetOnly && !StartWithLet] PrimaryExpression
    | [NoLetOnly && !StartWithLet] PrimaryExpression<+NoLet>
    | [StartWithLet && !NoLetOnly] 'let' -> IdentifierReference
    | [StartWithLet] expr=MemberExpression<+NoLetOnly> '[' index=Expression<+In> ']' -> IndexAccess
    | [!StartWithLet] expr=MemberExpression<NoLetOnly: NoLetSq> '[' index=Expression<+In> ']' -> IndexAccess
    | expr=MemberExpression '.' selector=IdentifierNameRef -> PropertyAccess
    | expr=MemberExpression .noLineBreak '!' -> TsNonNull
    | [!StartWithLet] SuperProperty
    | [!StartWithLet] MetaProperty
    | [!StartWithLet] 'new' expr=MemberExpression Arguments -> NewExpression
;

SuperExpression -> Expression /* interface */
    : 'super' -> SuperExpression
;

SuperProperty -> Expression /* interface */
    : expr=SuperExpression '[' index=Expression<+In> ']' -> IndexAccess
    | expr=SuperExpression '.' selector=IdentifierNameRef -> PropertyAccess
;

MetaProperty
    : NewTarget
;

NewTarget -> NewTarget
    : 'new' '.' 'target'
;

NewExpression -> Expression /* interface */
    : MemberExpression  (?= !StartOfParametrizedCall)
    | [!StartWithLet] 'new' expr=NewExpression -> NewExpression
;

CallExpression -> Expression /* interface */
    : expr=MemberExpression Arguments -> CallExpression
    | [!StartWithLet] SuperCall -> CallExpression
    | expr=CallExpression Arguments -> CallExpression
    | expr=CallExpression '[' index=Expression<+In> ']' -> IndexAccess
    | expr=CallExpression '.' selector=IdentifierNameRef -> PropertyAccess
    | expr=CallExpression .noLineBreak '!' -> TsNonNull
;

SuperCall
    : expr=SuperExpression Arguments
;

Arguments -> Arguments
    : (?= StartOfParametrizedCall) TypeArguments '(' (list=ArgumentList ','?)? ')'
    | '(' (list=ArgumentList ','?)? ')'
;

StartOfParametrizedCall
    : TypeArguments '('
;

ArgumentList
    : AssignmentExpression<+In>
    | SpreadElement
    | ArgumentList ',' AssignmentExpression<+In>
    | ArgumentList ',' SpreadElement
;

LeftHandSideExpression -> Expression /* interface */
    : NewExpression
    | CallExpression (?= !StartOfParametrizedCall)
;

UpdateExpression -> Expression /* interface */
    : LeftHandSideExpression
    | LeftHandSideExpression .noLineBreak '++' -> PostInc
    | LeftHandSideExpression .noLineBreak '--' -> PostDec
    | [!StartWithLet] '++' UnaryExpression -> PreInc
    | [!StartWithLet] '--' UnaryExpression -> PreDec
;

# 一元表达式
UnaryExpression -> Expression /* interface */
    : UpdateExpression
    | [!StartWithLet] 'delete' UnaryExpression -> UnaryExpression
    | [!StartWithLet] 'void' UnaryExpression -> UnaryExpression
    | [!StartWithLet] 'typeof' UnaryExpression -> UnaryExpression
    | [!StartWithLet] '+' UnaryExpression -> UnaryExpression
    | [!StartWithLet] '-' UnaryExpression -> UnaryExpression
    | [!StartWithLet] '~' UnaryExpression -> UnaryExpression
    | [!StartWithLet] '!' UnaryExpression -> UnaryExpression
    | [!StartWithLet] '<' Type '>' UnaryExpression -> TsCastExpression
;

%left resolveShift;

%left '||';
%left '&&';
%left '|';
%left '^';
%left '&';
%left '==' '!=' '===' '!==';
%left '<' '>' '<=' '>=' 'instanceof' 'in' 'as';
%left '<<' '>>' '>>>';
%left '-' '+';
%left '*' '/' '%';
%right '**';

# 算术表达式
ArithmeticExpression -> Expression /* interface */
    : UnaryExpression
    | left=ArithmeticExpression '+' right=ArithmeticExpression -> AdditiveExpression
    | left=ArithmeticExpression '-' right=ArithmeticExpression -> AdditiveExpression
    | left=ArithmeticExpression '<<' right=ArithmeticExpression -> ShiftExpression
    | left=ArithmeticExpression '>>' right=ArithmeticExpression -> ShiftExpression
    | left=ArithmeticExpression '>>>' right=ArithmeticExpression -> ShiftExpression
    | left=ArithmeticExpression '*' right=ArithmeticExpression -> MultiplicativeExpression
    | left=ArithmeticExpression '/' right=ArithmeticExpression -> MultiplicativeExpression
    | left=ArithmeticExpression '%' right=ArithmeticExpression -> MultiplicativeExpression
    | left=UpdateExpression '**' right=ArithmeticExpression -> ExponentiationExpression
;

# 二进制表达式
BinaryExpression<In> -> Expression /* interface */
    : ArithmeticExpression
    | left=BinaryExpression '<' right=BinaryExpression -> RelationalExpression
    | left=BinaryExpression '>' right=BinaryExpression -> RelationalExpression
    | left=BinaryExpression '<=' right=BinaryExpression -> RelationalExpression
    | left=BinaryExpression '>=' right=BinaryExpression -> RelationalExpression
    | left=BinaryExpression 'instanceof' right=BinaryExpression -> RelationalExpression
    | [In] left=BinaryExpression 'in' right=BinaryExpression -> RelationalExpression
    | [!NoAs] left=BinaryExpression .noLineBreak 'as' Type -> TsAsExpression
    | left=BinaryExpression '==' right=BinaryExpression -> EqualityExpression
    | left=BinaryExpression '!=' right=BinaryExpression -> EqualityExpression
    | left=BinaryExpression '===' right=BinaryExpression -> EqualityExpression
    | left=BinaryExpression '!==' right=BinaryExpression -> EqualityExpression
    | left=BinaryExpression '&' right=BinaryExpression -> BitwiseANDExpression
    | left=BinaryExpression '^' right=BinaryExpression -> BitwiseXORExpression
    | left=BinaryExpression '|' right=BinaryExpression -> BitwiseORExpression
    | left=BinaryExpression '&&' right=BinaryExpression -> LogicalANDExpression
    | left=BinaryExpression '||' right=BinaryExpression -> LogicalORExpression
;

# 条件表达式
ConditionalExpression<In> -> Expression /* interface */
    : BinaryExpression
    | cond=BinaryExpression '?' then=AssignmentExpression<+In> ':' else=AssignmentExpression -> ConditionalExpression
;

# 赋值表达式
AssignmentExpression<In> -> Expression /* interface */
    : ConditionalExpression
    | left=LeftHandSideExpression '=' right=AssignmentExpression -> AssignmentExpression
    | left=LeftHandSideExpression AssignmentOperator right=AssignmentExpression -> AssignmentExpression
;

AssignmentOperator -> AssignmentOperator
    : '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '>>>='
    | '&=' | '^=' | '|=' | '**='
;

# 逗号表达式
CommaExpression<In> -> CommaExpression
    : left=Expression ',' right=AssignmentExpression
;

# === [ Statement ]
%interface Statement, Declaration;

Statement -> Statement /* interface */:
    BlockStatement
  | VariableStatement
  | EmptyStatement
  | ExpressionStatement
  | IfStatement
  | BreakableStatement
  | ContinueStatement
  | BreakStatement
  | ReturnStatement
  | WithStatement
  | LabelledStatement
  | ThrowStatement
  | TryStatement
  | DebuggerStatement
;

Declaration -> Declaration /* interface */:
    HoistableDeclaration
  | ClassDeclaration
  | LexicalDeclaration<+In>
  | TypeAliasDeclaration
  | InterfaceDeclaration
  | EnumDeclaration
;

HoistableDeclaration -> Declaration /* interface */:
    FunctionDeclaration
;

BreakableStatement -> Statement /* interface */:
    IterationStatement
  | SwitchStatement
;

BlockStatement :
    Block ;

Block -> Block :
    '{' .recoveryScope StatementList? '}' ;

StatementList :
    StatementListItem
  | StatementList StatementListItem
;

%interface StatementListItem, BindingPattern, PropertyPattern, ElementPattern, CaseClause;

StatementListItem -> StatementListItem /* interface */:
    Statement
  | Declaration
  | error ';'                                         -> SyntaxProblem
;

LexicalDeclaration<In> -> LexicalDeclaration :
    LetOrConst BindingList ';' ;

LetOrConst :
    'let'
  | 'const'
;

BindingList<In> :
    LexicalBinding
  | BindingList ',' LexicalBinding
;

LexicalBinding<In> -> LexicalBinding
    #: BindingIdentifier TypeAnnotationopt Initializeropt
    #| BindingPattern TypeAnnotationopt Initializer
    : BindingIdentifier Initializeropt
    | BindingPattern Initializer
;

VariableStatement -> VariableStatement :
    'var' VariableDeclarationList<+In> ';'
;

VariableDeclarationList<In> :
    VariableDeclaration
  | VariableDeclarationList ',' VariableDeclaration
;

VariableDeclaration<In> -> VariableDeclaration
    #: BindingIdentifier TypeAnnotationopt Initializeropt
    #| BindingPattern TypeAnnotationopt Initializer
    : BindingIdentifier Initializeropt
    | BindingPattern Initializer
;

BindingPattern -> BindingPattern /* interface */
    : ArrayBindingPattern
    | ObjectBindingPattern
;

ObjectBindingPattern -> ObjectPattern :
    '{' .recoveryScope BindingRestElementopt '}'
  | '{' .recoveryScope (PropertyPattern separator ',')+ (',' BindingRestElementopt)? '}'
;

ArrayBindingPattern -> ArrayPattern :
    '[' Elisionopt BindingRestElementopt ']'
  | '[' ElementPatternList ']'
  | '[' ElementPatternList ',' Elisionopt BindingRestElementopt ']'
;

ElementPatternList :
    BindingElisionElement
  | ElementPatternList ',' BindingElisionElement
;

BindingElisionElement :
    Elision? ElementPattern
;

PropertyPattern -> PropertyPattern /* interface */:
    SingleNameBinding
  | PropertyName ':' ElementPattern                   -> PropertyBinding
  | SyntaxError
;

ElementPattern -> ElementPattern /* interface */:
    SingleNameBinding
  | BindingPattern Initializeropt<+In>                -> ElementBinding
  | SyntaxError
;

SingleNameBinding -> SingleNameBinding :
    BindingIdentifier Initializeropt<+In>
;

BindingRestElement -> BindingRestElement :
    '...' BindingIdentifier
;

EmptyStatement -> EmptyStatement :
    ';' .emptyStatement ;

ExpressionStatement -> ExpressionStatement :
    Expression<+In, +NoFuncClass, +NoAs, +NoObjLiteral, +NoLetSq> ';' ;

%right 'else';

IfStatement -> IfStatement :
    'if' '(' Expression<+In> ')' then=Statement 'else' else=Statement
  | 'if' '(' Expression<+In> ')' then=Statement %prec 'else'
;

IterationStatement -> Statement /* interface */:
    'do' Statement 'while' '(' Expression<+In> ')' ';' .doWhile       -> DoWhileStatement
  | 'while' '(' Expression<+In> ')' Statement                         -> WhileStatement
  | 'for' '(' var=Expressionopt<~In,+NoLet> ';' .forSC ForCondition
          ';' .forSC ForFinalExpression ')' Statement                 -> ForStatement
  | 'for' '(' var=Expression<~In,+StartWithLet, +NoAs> ';' .forSC ForCondition
          ';' .forSC ForFinalExpression ')' Statement                 -> ForStatement
  | 'for' '(' 'var' VariableDeclarationList<~In> ';' .forSC ForCondition
          ';' .forSC ForFinalExpression ')' Statement                 -> ForStatementWithVar
  | 'for' '(' LetOrConst BindingList<~In> ';' .forSC ForCondition
          ';' .forSC ForFinalExpression ')' Statement                 -> ForStatementWithVar
  | 'for' '(' var=LeftHandSideExpression<+NoLet>
          'in' object=Expression<+In> ')' Statement                   -> ForInStatement
  | 'for' '(' var=LeftHandSideExpression<+StartWithLet>
          'in' object=Expression<+In> ')' Statement                   -> ForInStatement
  | 'for' '(' 'var' ForBinding
          'in' object=Expression<+In> ')' Statement                   -> ForInStatementWithVar
  | 'for' '(' ForDeclaration
          'in' object=Expression<+In> ')' Statement                   -> ForInStatementWithVar
  | 'for' '(' var=LeftHandSideExpression<+NoLet>
          'of' iterable=AssignmentExpression<+In> ')' Statement       -> ForOfStatement
  | 'for' '(' 'var' ForBinding
          'of' iterable=AssignmentExpression<+In> ')' Statement       -> ForOfStatementWithVar
  | 'for' '(' ForDeclaration
          'of' iterable=AssignmentExpression<+In> ')' Statement       -> ForOfStatementWithVar
;

ForDeclaration :
    LetOrConst ForBinding
;

ForBinding -> ForBinding :
    BindingIdentifier
  | BindingPattern
;

ForCondition -> ForCondition :
    Expressionopt<+In> ;

ForFinalExpression -> ForFinalExpression :
    Expressionopt<+In> ;

ContinueStatement -> ContinueStatement :
    'continue' ';'
  | 'continue' .noLineBreak LabelIdentifier ';'
;

BreakStatement -> BreakStatement :
    'break' ';'
  | 'break' .noLineBreak LabelIdentifier ';'
;

ReturnStatement -> ReturnStatement :
    'return' ';'
  | 'return' .noLineBreak Expression<+In> ';'
;

WithStatement -> WithStatement :
    'with' '(' Expression<+In> ')' Statement
;

SwitchStatement -> SwitchStatement :
    'switch' '(' Expression<+In> ')' CaseBlock
;

CaseBlock -> Block :
    '{' .recoveryScope CaseClausesopt '}'
;

CaseClauses :
    CaseClause
  | CaseClauses CaseClause
;

CaseClause -> CaseClause /* interface */:
    'case' Expression<+In> ':' StatementList?         -> Case
  | 'default' ':' StatementList?                      -> Default
;

LabelledStatement -> LabelledStatement :
    LabelIdentifier ':' LabelledItem ;

LabelledItem :
    Statement
  | FunctionDeclaration
;

ThrowStatement -> ThrowStatement :
    'throw' .noLineBreak Expression<+In> ';'
;

TryStatement -> TryStatement :
    'try' Block Catch
  | 'try' Block Catch? Finally
;

Catch -> Catch :
    'catch' ('(' CatchParameter ')')? Block
;

Finally -> Finally :
    'finally' Block
;

CatchParameter :
    BindingIdentifier
  | BindingPattern
;

DebuggerStatement -> DebuggerStatement :
    'debugger' ';'
;

# === [ Functions and Classes ]

%interface ClassElement, MethodDefinition;

FunctionDeclaration -> Function :
    'func' BindingIdentifier? FormalParameters FunctionBody ;

FunctionExpression -> FunctionExpression :
    'func' BindingIdentifier? FormalParameters FunctionBody ;

UniqueFormalParameters :
    FormalParameters ;

FunctionBody -> Body :
    '{' .recoveryScope StatementList? '}'
  | ';'
;

MethodDefinition -> MethodDefinition /* interface */
    : PropertyName '?'? UniqueFormalParameters FunctionBody -> Method
    #| 'get' PropertyName '(' ')' TypeAnnotationopt FunctionBody             -> Getter
    | 'get' PropertyName '(' ')' FunctionBody             -> Getter
    | 'set' PropertyName '(' PropertySetParameterList ')'  FunctionBody     -> Setter
;

PropertySetParameterList :
    Parameter ;

ClassDeclaration -> Declaration /* interface */:
    Modifiers? 'class' BindingIdentifier<+WithoutImplements>? TypeParametersopt ClassTail   -> Class
;

ClassExpression -> ClassExpr :
    Modifiers? 'class' BindingIdentifier<+WithoutImplements>? TypeParameters? ClassTail
;

ClassTail :
    ClassHeritage ClassBody ;

ClassHeritage:
    ClassExtendsClause? ImplementsClause? ;

StartOfExtendsTypeRef:
    TypeReference ('implements' | '{') ;

ClassExtendsClause -> Extends:
    'extends' (?= StartOfExtendsTypeRef) TypeReference
  | 'extends' (?= !StartOfExtendsTypeRef) LeftHandSideExpression
;

ImplementsClause -> TsImplementsClause:
    'implements' (TypeReference separator ',')+ ;

ClassBody -> ClassBody :
    '{' .recoveryScope ClassElementList? '}' ;

ClassElementList :
    ClassElement
  | ClassElementList ClassElement
;

%interface Modifier;

Modifier -> Modifier:
    AccessibilityModifier
  | Decorator
  | 'static'                -> Static
  | 'abstract'              -> Abstract
  | 'readonly'              -> Readonly
;

Modifiers:
    Modifier
  | Modifiers Modifier
;

ClassElement -> ClassElement /* interface */
    : Modifiers? MethodDefinition                 -> MemberMethod
    #| Modifiers? PropertyName ('?'|'!')? TypeAnnotationopt Initializeropt<+In> ';' -> MemberVar
    | Modifiers? PropertyName ('?'|'!')? Initializeropt<+In> ';' -> MemberVar
    #| IndexSignature ';' -> TsIndexMemberDeclaration
    | ';'                                         -> EmptyDecl
;

# === [ Types ]
%interface TsType, TypeMember;

Type -> TsType /* interface */:
    UnionOrIntersectionOrPrimaryType %prec resolveShift
  | FunctionType
  | ConstructorType
  | paramref=IdentifierNameRef 'is' Type -> TypePredicate
;

TypeParameters -> TypeParameters :
    '<' (TypeParameter separator ',')+ '>' ;

TypeParameter -> TypeParameter :
    BindingIdentifier Constraint? ('=' Type)?;

Constraint -> TypeConstraint :
    'extends' Type ;

TypeArguments -> TypeArguments :
    '<' (Type separator ',')+ '>' ;

UnionOrIntersectionOrPrimaryType -> TsType /* interface */:
    inner+=UnionOrIntersectionOrPrimaryType? '|' inner+=IntersectionOrPrimaryType -> UnionType
  | IntersectionOrPrimaryType %prec resolveShift
;

IntersectionOrPrimaryType -> TsType /* interface */:
    inner+=IntersectionOrPrimaryType? '&' inner+=KeyOfOrPrimaryType -> IntersectionType
  | KeyOfOrPrimaryType
;

KeyOfOrPrimaryType -> TsType /* interface */:
    KeyOfType
  | PrimaryType
;

PrimaryType -> TsType /* interface */:
    ParenthesizedType
  | PredefinedType
  | TypeReference
  | ObjectType
  #| MappedType
  | ArrayType
  | IndexedAccessType
  | LiteralType
  | TupleType
  | TypeQuery
  | 'this'                           -> ThisType
;

ParenthesizedType -> ParenthesizedType :
    '(' (?= !StartOfFunctionType) Type ')' ;

# 2.0
LiteralType -> LiteralType :
    StringLiteral
  | '-'? NumericLiteral
  | NULL
  | 'true'
  | 'false'
;

PredefinedType -> PredefinedType :
    'any'
  | 'number'
  | 'boolean'
  | 'string'
  | 'symbol'
  | 'void'
;

TypeReference -> TypeReference :
    TypeName .noLineBreak TypeArguments? %prec resolveShift ;

TypeName -> TypeName :
    ref+=IdentifierReference<+WithoutPredefinedTypes>
;

ObjectType -> ObjectType :
    '{' .recoveryScope (?= !StartOfMappedType) TypeBody? '}' ;

TypeBody :
    TypeMemberList
  | TypeMemberList ','
  | TypeMemberList ';'
;

TypeMemberList :
    TypeMember
  | TypeMemberList ';' TypeMember
  | TypeMemberList ',' TypeMember
;

TypeMember -> TypeMember /* interface */:
    PropertySignature
  | MethodSignature
  | CallSignature
  | ConstructSignature
  #| IndexSignature
;

ArrayType -> ArrayType :
    PrimaryType .noLineBreak '[' ']' ;

# 2.1
IndexedAccessType -> IndexedAccessType :
    left=PrimaryType .noLineBreak '[' index=Type ']' ;

# 2.1
StartOfMappedType :
    'readonly'? '[' IdentifierName 'in' ;

# 2.1
#MappedType -> MappedType
#    : '{' .recoveryScope (?= StartOfMappedType) 'readonly'? '[' Identifier 'in' Type ']' '?'? TypeAnnotation ';'? '}'
#;

TupleType -> TupleType :
    '[' (Type separator ',')+ ']' ;

# This lookahead rule disambiguates FunctionType vs ParenthesizedType
# productions by enumerating all prefixes of FunctionType that would
# lead to parse failure if interpreted as Type.
# (partially inspired by isUnambiguouslyStartOfFunctionType() in
# src/compiler/parser.ts)
StartOfFunctionType :
    Modifiers? BindingIdentifier (':' | ',' | '?' | '=' | ')' '=>')
  | Modifiers? BindingPattern (':' | ',' | '?' | '=' | ')' '=>')
  | '...'
  | 'this' ':'
  | ')'
;

FunctionType -> FunctionType :
    TypeParameters? FunctionTypeParameterList '=>' Type ;

FunctionTypeParameterList -> Parameters :
    '(' (?= StartOfFunctionType) (Parameter separator ',')+? ','? ')' ;

ConstructorType -> ConstructorType :
    'new' TypeParameters? ParameterList '=>' Type ;

%left 'keyof' 'typeof';
%nonassoc 'is';

# 2.1
KeyOfType -> KeyOfType :
    'keyof' KeyOfOrPrimaryType ;

TypeQuery -> TypeQuery :
    'typeof' TypeQueryExpression ;

TypeQueryExpression :
    IdentifierReference
  | TypeQueryExpression '.' IdentifierName
;

PropertySignature -> PropertySignature
    #: Modifiers? PropertyName<+WithoutNew> '?'? TypeAnnotation?
    : Modifiers? PropertyName<+WithoutNew> '?'?
;

FormalParameters
    #: TypeParameters? ParameterList TypeAnnotation?
    : TypeParameters? ParameterList
;

CallSignature -> CallSignature
    #: TypeParameters? ParameterList TypeAnnotation?
    : TypeParameters? ParameterList
;

ParameterList -> Parameters :
    '(' (Parameter separator ',')+? ','? ')' ;

%interface Parameter;

Parameter -> Parameter
    #: Modifiers? BindingIdentifier '?'? TypeAnnotation? -> DefaultParameter
    #| Modifiers? BindingPattern '?'? TypeAnnotation? -> DefaultParameter
    #| Modifiers? BindingIdentifier TypeAnnotation? Initializer<+In> -> DefaultParameter
    #| Modifiers? BindingPattern TypeAnnotation? Initializer<+In> -> DefaultParameter
    #| '...' BindingIdentifier TypeAnnotation? -> RestParameter
    : Modifiers? BindingIdentifier '?'? -> DefaultParameter
    | Modifiers? BindingPattern '?'? -> DefaultParameter
    | Modifiers? BindingIdentifier Initializer<+In> -> DefaultParameter
    | Modifiers? BindingPattern Initializer<+In> -> DefaultParameter
    | '...' BindingIdentifier -> RestParameter

    #| 'this' TypeAnnotation -> TsThisParameter
    | SyntaxError
;

AccessibilityModifier -> AccessibilityModifier :
    'public'
  | 'private'
  | 'protected'
;

BindingIdentifierOrPattern :
    BindingIdentifier
  | BindingPattern
;

ConstructSignature -> ConstructSignature
    #: Modifiers? 'new' TypeParameters? ParameterList TypeAnnotation?
    : Modifiers? 'new' TypeParameters? ParameterList
;

# Note: using IdentifierName instead of BindingIdentifier to avoid r/r
# conflicts with ComputedPropertyName.
#IndexSignature -> IndexSignature :
#    Modifiers? '[' IdentifierName ':' 'string' ']' TypeAnnotation
#  | Modifiers? '[' IdentifierName ':' 'number' ']' TypeAnnotation
#;

MethodSignature -> MethodSignature :
    Modifiers? PropertyName<+WithoutNew> '?'? FormalParameters ;

TypeAliasDeclaration -> TypeAliasDeclaration :
    'type' BindingIdentifier TypeParameters? '=' Type ';' ;

# === [ Interfaces ]
InterfaceDeclaration -> TsInterface:
    'interface' BindingIdentifier TypeParametersopt InterfaceExtendsClause? ObjectType ;

InterfaceExtendsClause -> TsInterfaceExtends:
    'extends' (TypeReference separator ',')+ ;

# === [ Enums ]
EnumDeclaration -> TsEnum:
    'const'? 'enum' BindingIdentifier EnumBody ;

EnumBody -> TsEnumBody:
    '{' .recoveryScope ((EnumMember separator ',')+ ','?)? '}' ;

EnumMember -> TsEnumMember:
    PropertyName
  | PropertyName '=' AssignmentExpression<+In>
;









# === [ Decorator ]
%interface Decorator;

Decorator -> Decorator /* interface */
    : '@' DecoratorMemberExpression -> DecoratorExpr
    | '@' DecoratorCallExpression -> DecoratorCall
;

DecoratorMemberExpression
    : IdentifierReference
    | DecoratorMemberExpression '.' IdentifierName
;

DecoratorCallExpression
    : DecoratorMemberExpression Arguments
;

# === [ Import ]
ImportList
    : ImportDeclaration*
;

ImportDeclaration -> ImportDeclaration
    : 'import' ModuleSpecifier
    | 'import' '(' ModuleSpecifier+ ')'
;

ModuleSpecifier -> ModuleSpecifier
    : BindingIdentifier? StringLiteral
;

# === [ Modules ]

Package -> Package
    #: SingleLineComment* 'package' PackageName Define*
    : 'package' PackageName PackageBody
;
PackageName -> PackageName: Identifier;
PackageBody
    : ImportList ModuleItemList
;

%interface ModuleItem;
ModuleItemList
    : ModuleItem
    | ModuleItemList ModuleItem
;
ModuleItem -> ModuleItem /* interface */
    :StatementListItem
;




# ### [ Has Removed ]
# jsx
# ~~赋值表达式(AssignmentExpression) - 禁止连续赋值
# namespace, ts-module, export
# GeneratorFunc&yield
# async/await
# ArrowFunc & ConciseBody
# regexp
# templateString

# ### [ Temp Remove | Need Change ]
# [X] import
# [ ] TsType => SkrType (e.g. PredefinedType); remove TypeAnnotation
# [ ] for/while/...
# [ ] if/else
# [ ] var/const/let -> var/const
##### `a:=1`
##### `var a int`
##### `var (
#####    a int
#####    b = 1
##### )`
# [ ] ts-declare(Ambient) -> extern asm
