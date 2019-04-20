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
'package':      /package/
'import':       /import/

'skr':          /skr/
'go':           /go/
'asm':          /asm/
'extern':       /extern/
'declare':      /declare/

'var':          /var/
'const':        /const/
'type':         /type/
'interface':    /interface/
'struct':       /struct/
'map':          /map/
'enum':         /enum/
'chan':         /chan/

'func':         /func/
'defer':        /defer/
'return':       /return/

'select':       /select/
'switch':       /switch/
'case':         /case/
'default':      /default/
'fallthrough':  /fallthrough/

'if':           /if/
'else':         /else/

'for':          /for/
'range':        /range/
'break':        /break/
'continue':     /continue/
'goto':         /goto/

'throw':        /throw/
'try':          /try/
'catch':        /catch/
'finally':      /finally/

# types
#'void': /void/
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
'error': /error/

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
'_': /_/

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
%input Package;

%assert empty set(follow error & ~('}' | ')' | ',' | ';' | ']'));

%generate afterErr = set(follow error);

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
%left '<' '>' '<=' '>=';# 'instanceof' 'in' 'as';
%left '<<' '>>' '>>>';
%left '-' '+';
%left '*' '/' '%';
%right '**';
%right 'else';
#%left 'keyof' 'typeof';
#%nonassoc 'is';

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
    : 'error'
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

    #| 'void'
;

LiteralType -> LiteralType
    #: StringLiteral
    : BindingIdentifier
;
TypeAliasDeclaration -> TypeAliasDeclaration
    : 'type' BindingIdentifier Type
;

Type -> SkrType /* interface */
    : PrimaryType
;
PrimaryType -> SkrType /* interface */
    : PredefinedType
    #| ParenthesizedType
    | LiteralType

    | ArrayType
    | MapType
    | RefType

    | EnumType
    | StructType
    | InterfaceType
    | FunctionType
;

ParenthesizedType -> ParenthesizedType
    : '(' Type ')'
;

ArrayType -> ArrayType
    : '[' ']' Type
;

MapType -> ArrayType
    : 'map' '[' KeyType ']' Type
;
KeyType -> KeyType: Type;

RefType -> RefType
    : '*' Type
;

# --- [ Enum ]
EnumType -> EnumType
    : 'enum' '{' ('+'|'<<') (BindingIdentifier separator ',')+ '}'
;

# --- [ Struct ]
StructType -> StructType
    : 'struct' '{' StructItem* '}'
;
StructItem -> StructItem
    : ParameterList
;

# --- [ Interface ]
InterfaceType -> InterfaceType
    : 'interface' '{' InterfaceItem* '}'
;
InterfaceItem -> InterfaceItem
    #: BindingIdentifier FunctionInfo
    : BindingIdentifier FunctionInfo ';'
;

# --- [ Function ]
FunctionType -> FunctionType
    #:'func' FunctionInfo
    :  '(' 'func' FunctionInfo ')'
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

# === [ Package & Module ]

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
    : StatementListItem
;

# --- [ Import Declaration ]

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

# === [ Literal ]
%interface Literal;

# --- [ 一般字面量 ]
Literal -> Literal
    : NULL -> NullLiteral
    | 'true' -> BoolLiteral
    | 'false' -> BoolLiteral
    | NumericLiteral -> NumLiteral
    | StringLiteral -> StrLiteral
    | '_' -> UseLessLiteral
;

# --- [ 数组字面量 ]
ArrayLiteral -> ArrayLiteral
    : '[' Elisionopt ']'
    | '[' list=ElementList ']'
    | '[' list=ElementList ',' Elisionopt ']'
;

ElementList
    : Elisionopt AssignmentExpression
    | Elisionopt SpreadElement
    | ElementList ',' Elisionopt AssignmentExpression
    | ElementList ',' Elisionopt SpreadElement
;

Elision
    : ','
    | Elision ','
;

SpreadElement -> Expression /* interface */
    : '...' AssignmentExpression -> SpreadElement
;

# --- [ 对象字面量 ]
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
    | Modifiers? PropertyName ':' value=AssignmentExpression -> Property
    #| Modifiers? MethodDefinition -> ObjectMethod
    | CoverInitializedName -> SyntaxProblem
    | SyntaxError
    | '...' AssignmentExpression -> SpreadProperty
;

PropertyName -> PropertyName /* interface */
    : LiteralPropertyName
    | ComputedPropertyName
;

LiteralPropertyName -> LiteralPropertyName
    : IdentifierNameDecl
    | StringLiteral
    | NumericLiteral
;

ComputedPropertyName -> ComputedPropertyName
    : '[' AssignmentExpression ']'
;

CoverInitializedName
    : IdentifierReference Initializer
;

Initializer -> Initializer
    : '=' AssignmentExpression
;

# === [ Expression ]
%interface Expression;

Expression -> Expression /* interface */
    # 赋值表达式
    : AssignmentExpression
    # 逗号表达式
    | CommaExpression
;

PrimaryExpression -> Expression /* interface */
    : IdentifierReference
    | Literal
    | ArrayLiteral
    | [!NoObjLiteral] ObjectLiteral
    #| [!NoFuncClass] FunctionExpression
    #| [!NoFuncClass] ClassExpression
    | Parenthesized
;

Parenthesized -> Parenthesized
    : '(' Expression ')'
    | '(' SyntaxError ')'
;

MemberExpression<flag NoLetOnly = false> -> Expression /* interface */
    : [!NoLetOnly && !StartWithLet] PrimaryExpression
    | [NoLetOnly && !StartWithLet] PrimaryExpression
    #########| [StartWithLet && !NoLetOnly] 'let' -> IdentifierReference
    | [StartWithLet] expr=MemberExpression<+NoLetOnly> '[' index=Expression ']' -> IndexAccess
    | [!StartWithLet] expr=MemberExpression<NoLetOnly: NoLetSq> '[' index=Expression ']' -> IndexAccess
    | expr=MemberExpression '.' selector=IdentifierNameRef -> PropertyAccess
    | expr=MemberExpression .noLineBreak '!' -> TsNonNull
;

NewExpression -> Expression /* interface */
    : MemberExpression  #(?= !StartOfParametrizedCall)
;

CallExpression -> Expression /* interface */
    : expr=MemberExpression Arguments -> CallExpression
    | expr=CallExpression Arguments -> CallExpression
    | expr=CallExpression '[' index=Expression ']' -> IndexAccess
    | expr=CallExpression '.' selector=IdentifierNameRef -> PropertyAccess
    | expr=CallExpression .noLineBreak '!' -> TsNonNull
;

Arguments -> Arguments
    : '(' (list=ArgumentList ','?)? ')'
    #| (?= StartOfParametrizedCall) TypeArguments '(' (list=ArgumentList ','?)? ')'
;

#StartOfParametrizedCall
#    : TypeArguments '('
#;

ArgumentList
    : AssignmentExpression
    | SpreadElement
    | ArgumentList ',' AssignmentExpression
    | ArgumentList ',' SpreadElement
;

LeftHandSideExpression -> Expression /* interface */
    : NewExpression
    | CallExpression #(?= !StartOfParametrizedCall)
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
    | [!StartWithLet] '+' UnaryExpression -> UnaryExpression
    | [!StartWithLet] '-' UnaryExpression -> UnaryExpression
    | [!StartWithLet] '~' UnaryExpression -> UnaryExpression
    | [!StartWithLet] '!' UnaryExpression -> UnaryExpression
    | [!StartWithLet] '<' Type '>' UnaryExpression -> TsCastExpression
;

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
BinaryExpression -> Expression /* interface */
    : ArithmeticExpression
    | left=BinaryExpression '<' right=BinaryExpression -> RelationalExpression
    | left=BinaryExpression '>' right=BinaryExpression -> RelationalExpression
    | left=BinaryExpression '<=' right=BinaryExpression -> RelationalExpression
    | left=BinaryExpression '>=' right=BinaryExpression -> RelationalExpression
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
ConditionalExpression -> Expression /* interface */
    : BinaryExpression
    | cond=BinaryExpression '?' then=AssignmentExpression ':' else=AssignmentExpression -> ConditionalExpression
;

# 赋值表达式
AssignmentExpression -> Expression /* interface */
    : ConditionalExpression
    | left=LeftHandSideExpression '=' right=AssignmentExpression -> AssignmentExpression
    | left=LeftHandSideExpression AssignmentOperator right=AssignmentExpression -> AssignmentExpression
;

AssignmentOperator -> AssignmentOperator
    : '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '>>>='
    | '&=' | '^=' | '|=' | '**='
;

# 逗号表达式
CommaExpression -> CommaExpression
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
  | LabelledStatement
  | ThrowStatement
  | TryStatement
;

Declaration -> Declaration /* interface */:
    HoistableDeclaration
  #| ClassDeclaration
  | LexicalDeclaration
  | TypeAliasDeclaration
;

HoistableDeclaration -> Declaration /* interface */:
    TopFunctionDeclaration
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

StatementListItem -> StatementListItem /* interface */
    : Declaration
    #| Statement
    | error ';' -> SyntaxProblem
;

LexicalDeclaration -> LexicalDeclaration :
    LetOrConst BindingList ';' ;

LetOrConst
    #########: 'let'
    : 'var'
    | 'const'
;

BindingList :
    LexicalBinding
  | BindingList ',' LexicalBinding
;

LexicalBinding -> LexicalBinding
    #: BindingIdentifier TypeAnnotationopt Initializeropt
    #| BindingPattern TypeAnnotationopt Initializer
    : BindingIdentifier Initializeropt
    | BindingPattern Initializer
;

VariableStatement -> VariableStatement :
    'var' VariableDeclarationList ';'
;

VariableDeclarationList :
    VariableDeclaration
  | VariableDeclarationList ',' VariableDeclaration
;

VariableDeclaration -> VariableDeclaration
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
  | PropertyName ':' ElementPattern -> PropertyBinding
  | SyntaxError
;

ElementPattern -> ElementPattern /* interface */:
    SingleNameBinding
  | BindingPattern Initializeropt                -> ElementBinding
  | SyntaxError
;

SingleNameBinding -> SingleNameBinding :
    BindingIdentifier Initializeropt
;

BindingRestElement -> BindingRestElement :
    '...' BindingIdentifier
;

EmptyStatement -> EmptyStatement :
    ';' .emptyStatement ;

ExpressionStatement -> ExpressionStatement :
    Expression<+NoFuncClass, +NoAs, +NoObjLiteral, +NoLetSq> ';' ;

%right 'else';

IfStatement -> IfStatement :
    'if' '(' Expression ')' then=Statement 'else' else=Statement
  | 'if' '(' Expression ')' then=Statement %prec 'else'
;

IterationStatement -> Statement /* interface */:
  | 'for' '(' Expression ')' Statement                         -> WhileStatement
  | 'for' '(' var=Expressionopt ';' .forSC ForCondition
          ';' .forSC ForFinalExpression ')' Statement                 -> ForStatement
  | 'for' '(' var=Expression<+StartWithLet, +NoAs> ';' .forSC ForCondition
          ';' .forSC ForFinalExpression ')' Statement                 -> ForStatement
  | 'for' '(' 'var' VariableDeclarationList ';' .forSC ForCondition
          ';' .forSC ForFinalExpression ')' Statement                 -> ForStatementWithVar
  | 'for' '(' LetOrConst BindingList ';' .forSC ForCondition
          ';' .forSC ForFinalExpression ')' Statement                 -> ForStatementWithVar
  ###| 'for' '(' var=LeftHandSideExpression 'in' object=Expression ')' Statement                   -> ForInStatement
  ###| 'for' '(' var=LeftHandSideExpression<+StartWithLet> 'in' object=Expression ')' Statement                   -> ForInStatement
  ###| 'for' '(' 'var' ForBinding 'in' object=Expression ')' Statement                   -> ForInStatementWithVar
  ###| 'for' '(' ForDeclaration 'in' object=Expression ')' Statement                   -> ForInStatementWithVar
  ###| 'for' '(' var=LeftHandSideExpression 'of' iterable=AssignmentExpression ')' Statement       -> ForOfStatement
  ###| 'for' '(' 'var' ForBinding 'of' iterable=AssignmentExpression ')' Statement       -> ForOfStatementWithVar
  ###| 'for' '(' ForDeclaration 'of' iterable=AssignmentExpression ')' Statement       -> ForOfStatementWithVar
;

ForDeclaration :
    LetOrConst ForBinding
;

ForBinding -> ForBinding :
    BindingIdentifier
  | BindingPattern
;

ForCondition -> ForCondition :
    Expressionopt ;

ForFinalExpression -> ForFinalExpression :
    Expressionopt ;

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
  | 'return' .noLineBreak Expression ';'
;

SwitchStatement -> SwitchStatement :
    'switch' '(' Expression ')' CaseBlock
;

CaseBlock -> Block :
    '{' .recoveryScope CaseClausesopt '}'
;

CaseClauses :
    CaseClause
  | CaseClauses CaseClause
;

CaseClause -> CaseClause /* interface */:
    'case' Expression ':' StatementList?         -> Case
  | 'default' ':' StatementList?                      -> Default
;

LabelledStatement -> LabelledStatement :
    LabelIdentifier ':' LabelledItem ;

LabelledItem :
    Statement
  | FunctionDeclaration
;

ThrowStatement -> ThrowStatement :
    'throw' .noLineBreak Expression ';'
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

# === [ Functions ]

TopFunctionDeclaration -> FunctionDeclaration
    : Modifiers? 'func' FunctionNamespace? FunctionName FunctionInfo Block
;
FunctionDeclaration -> FunctionDeclaration
    : Modifiers? 'func' FunctionInfo Block
;

FunctionName -> FunctionName
    : BindingIdentifier
;
FunctionNamespace -> FunctionNamespace
    : '(' BindingIdentifier '*'? IdentifierReference ')'
;

%interface Modifier;
Modifier -> Modifier
    : Decorator
;
Modifiers: Modifier+;








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

# [ ] for/while/...
# [ ] if/else
# [ ] var/const/let -> var/const
# [ ] ts-declare(Ambient) -> extern asm

# [ ] TsType => SkrType (e.g. PredefinedType); remove TypeAnnotation
# [ ] interface
# [ ] class -> struct
# [ ] enum

# `Ts`:
# TsNonNull TsCastExpression TsAsExpression TsImplementsClause
# TsInterface TsInterfaceExtends TsEnum TsEnumBody TsEnumMember

# [ ] TypeQuery
