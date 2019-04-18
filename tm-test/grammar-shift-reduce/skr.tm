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

%s initial, div;
<*> eoi: /{eoi}/

invalid_token:
error:

# Whitespace
<initial, div> {
    WhiteSpace: /[\t\x0b\x0c\x20\xa0\ufeff\p{Zs}]/ (space)
}
WhiteSpace: /[\n\r\u2028\u2029]|\r\n/ (space)

# Comment
commentChars = /([^*]|\*+[^*\/])*\**/
MultiLineComment:  /\/\*{commentChars}\*\//     (space)
# Note: the following rule disables backtracking for incomplete multiline comments, which
# would otherwise be reported as '/', '*', etc.
invalid_token: /\/\*{commentChars}/
SingleLineComment: /\/\/[^\n\r\u2028\u2029]*/   (space)
# Shebang.
SingleLineComment: /#![^\n\r\u2028\u2029]*/   (space)

# Identifier
hex = /[0-9A-Fa-f]/
IDStart = /\p{Lu}|\p{Ll}|\p{Lt}|\p{Lm}|\p{Lo}|\p{Nl}/
IDContinue = /{IDStart}|\p{Mn}|\p{Mc}|\p{Nd}|\p{Pc}/
JoinControl = /\u200c|\u200d/
unicodeEscapeSequence = /u(\{{hex}+\}|{hex}{4})/
brokenEscapeSequence = /\\(u({hex}{0,3}|\{{hex}*))?/
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
'struct': /struct/

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
'while':      /while/
'with':       /with/
'yield':      /yield/
# Future-reserved.
'enum':  /enum/
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

# Punctuation
':=': /:=/
'{': /\{/
'}': /\}/
'(': /\(/
')': /\)/
'[': /\[/
']': /\]/
'.': /\./
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

# Literals.
NULL:
'null': /null/ { token = NULL }
'nil': /nil/ { token = NULL }
'true':  /true/
'false': /false/

# Num
#bin = /[0-1]/
#oct = /[0-7]/
#dec = /[0-9]/
#hex = /[0-9A-Fa-f]/
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
# s = unquote(s[1 : len(s)-1])
escape = /\\([^1-9xu\n\r\u2028\u2029]|x{hex}{2}|{unicodeEscapeSequence})/
lineCont = /\\([\n\r\u2028\u2029]|\r\n)/
dsChar = /[^\n\r"\\\u2028\u2029]|{escape}|{lineCont}/
ssChar = /[^\n\r'\\\u2028\u2029]|{escape}|{lineCont}/
# TODO check \0 is valid if [lookahead != DecimalDigit]
StringLiteral: /"{dsChar}*"/
StringLiteral: /'{ssChar}*'/
# 按Go标准，反引号内无法转义反引号
MultiLineChars = /([^`]|{escape}|{lineCont})*/
StringLiteral: /`{MultiLineChars}`/

# For precedence resolution.
resolveShift:



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

# ### [ Syntax part ]
:: parser
%input Type;

%assert empty set(follow error & ~('}' | ')' | ',' | ';' | ']'));

%generate afterErr = set(follow error);

%flag In;

%lookahead flag NoLetSq = false;
%lookahead flag NoObjLiteral = false;
%lookahead flag NoFuncClass = false;
%lookahead flag NoAs = false;
%lookahead flag StartWithLet = false;

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
%right 'else';
%left 'keyof' 'typeof';
%nonassoc 'is';

SyntaxError -> SyntaxProblem
    : error
;

# === [ Identifier ]

IdentifierName
    : Identifier
;

IdentifierNameDecl
    : IdentifierName -> BindingIdentifier
;

IdentifierNameRef
    : IdentifierName -> IdentifierReference
;

IdentifierReference -> IdentifierReference
    : Identifier
;

BindingIdentifier -> BindingIdentifier
    : Identifier
;

# 标签标识符
LabelIdentifier -> LabelIdentifier
    : Identifier
;

# === [ Types ]
%interface SkrType, TypeMember;

PredefinedType -> PredefinedType
    : 'void'
    | 'symbol'
    | 'string'
    | 'bool'
    | 'int'
;

LiteralType -> LiteralType
    #: StringLiteral
    : BindingIdentifier
;

Type -> SkrType /* interface */
    : PrimaryType
;
PrimaryType -> SkrType /* interface */
    : PredefinedType
    #| ParenthesizedType
    | LiteralType

    | StructType
    | InterfaceType
    | FunctionType
;
ParenthesizedType -> ParenthesizedType
    : '(' Type ')'
;

# --- [ Struct ]
StructType -> StructType
    : 'struct' '{' ParameterList* '}'
;

# --- [ Interface ]
InterfaceType -> InterfaceType
    : 'interface' '{' InterfaceItem* '}'
;
InterfaceItem -> InterfaceItem
    : BindingIdentifier FunctionInfo
;

# --- [ Function ]
FunctionType -> FunctionType
    : 'func' FunctionInfo
;
FunctionInfo
    : '(' (ParameterList separator ',')* ')' RetList?
;
ParameterList -> ParameterList
    : (BindingIdentifier separator ',')+ Type
;
RetList -> RetList
    : Type
    | '(' (Type separator ',')+ ')'
;
