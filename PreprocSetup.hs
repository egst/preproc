{-# LANGUAGE TypeFamilies #-}

module PreprocSetup where

import Prelude hiding (LT, LE, GT, GE)

import Tools
import Automata
import JSONType

-- Lexer: ----------------------------------------------------------------------

data Literal =
    NullLiteral              | -- null
    BooleanLiteral Bool      | -- true, false
    NumberLiteral {            -- 12.34e56
        nlitIntegral :: Int,
        nlitDecimal  :: Int,
        nlitExponent :: Int
    }                        |
    StringLiteral String       -- "..."
    deriving (Eq, Show)

data TokenDomain =
    ContentToken String  | -- Plain content
    NameToken String     | -- Variable name
    LiteralToken Literal | -- Literal
    -- Symbols:
    PlusToken      | -- +
    ConcatToken    | -- ++
    MinusToken     | -- -
    MultToken      | -- *
    PowToken       | -- **
    DivToken       | -- /
    IDivToken      | -- //
    ModToken       | -- %
    NotToken       | -- !
    NEToken        | -- !=
    AssToken       | -- =
    EqToken        | -- ==
    LTToken        | -- <
    LEToken        | -- <=
    GTToken        | -- >
    GEToken        | -- >=
    AndToken       | -- &&
    OrToken        | -- ||
    CommaToken     | -- ,
    PairToken      | -- :
    SemicolonToken | -- ;
    OBrackToken    | -- [
    CBrackToken    | -- ]
    OBracToken     | -- {
    CBracToken     | -- }
    OParToken      | -- (
    CParToken      | -- )
    -- Statement keywords:
    EndToken       | -- end
    IfToken        | -- if
    ElifToken      | -- elif
    ElseToken      | -- else
    ForToken       | -- for
    InToken        | -- in
    LetToken       | -- let
    -- Invalid:
    InvalidToken String |
    UnknownToken
    deriving (Eq, Show)

data LexerStateDomain =
    CodeState    | -- Reading code
    ContentState | -- Reading content
    NameState    | -- Reading name
    EndState''   | -- Reading EOF inside a sub-routine
    EscapeState' | -- Escaping \n with \
    EscapeState''| -- Escaping \n with ;
    -- Double-symbol operators:
    PlusState    | -- + -> ++
    AsterState   | -- * -> **
    SlashState   | -- / -> //
    ExclState    | -- ! -> !=
    EqualState   | -- = -> ==
    OAnglState   | -- < -> <=
    CAnglState   | -- > -> >=
    OBrackState  | -- [ -> [:
    AmperState   | -- & -> &&
    PipeState    | -- | -> ||
    PipeState'   | -- |
    -- Keywords:
    L' | LE' | LET  |                 -- let
    E' | EN' | END  |                 -- end
         EL' | ELS' | ELSE  |         -- else
               ELI' | ELIF  |         -- elif
    I' | IF  |                        -- if
         IN  |                        -- in
    F' | FO' | FOR  |                 -- for
         FA' | FAL' | FALS' | FALSE | -- false
    T' | TR' | TRU' | TRUE  |         -- true
    N' | NU' | NUL' | NULL  |         -- nul
    -- String literal:
    QuoteState  | -- Opening quote
    StringState | -- String content
    EscapeState | -- Escaping character
    -- Number literal:
    IntegralState | -- Reading digits before decimal point
    PointState    | -- Decimal point
    DecimalState  | -- Reading digits after decimal point
    EState        | -- Exponent symbol
    ExponentState   -- Reading exponent digits
    deriving (Eq, Show)

-- First operator symbols and their semantics as single-symbol operators:
firstOps :: (Automaton a, StateDomain a ~ LexerStateDomain) => [(Char, State a, TokenDomain)]
firstOps =
    map3 (id, State, id) [
        ( '+', PlusState,  PlusToken        ),
        ( '-', CodeState,  MinusToken       ),
        ( '*', AsterState, MultToken        ),
        ( '/', SlashState, DivToken         ),
        ( '%', CodeState,  ModToken         ),
        ( '!', ExclState,  NotToken         ),
        ( '=', EqualState, AssToken         ),
        ( '<', OAnglState, LTToken          ),
        ( '>', CAnglState, GTToken          ),
        ( '&', AmperState, InvalidToken "&" ),
        ( '|', PipeState', InvalidToken "|" ),
        ( ',', CodeState,  CommaToken       ),
        ( ':', CodeState,  PairToken        ),
        ( ';', CodeState,  SemicolonToken   ),
        ( '[', CodeState,  OBrackToken      ),
        ( ']', CodeState,  CBrackToken      ),
        ( '{', CodeState,  OBracToken       ),
        ( '}', CodeState,  CBracToken       ),
        ( '(', CodeState,  OParToken        ),
        ( ')', CodeState,  CParToken        )
    ]

-- Second operator symbols and their semantics as double-symbol operators:
secondOps :: (Automaton a, StateDomain a ~ LexerStateDomain) => [(State a, Char, TokenDomain)]
secondOps =
    map3 (State, id, id) [
        ( PlusState,  '+', ConcatToken       ),
        ( AsterState, '*', PowToken          ),
        ( SlashState, '/', IDivToken         ),
        ( ExclState,  '=', NEToken           ),
        ( EqualState, '=', EqToken           ),
        ( OAnglState, '=', LEToken           ),
        ( CAnglState, '=', GEToken           ),
        ( AmperState, '&', AndToken          )
        --( PipeState,  '|', OrToken           )
    ]

-- Symbol categories:
digitSymbols      = "0123456789"
initNameSymbols   = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWZ_"
nameSymbols       = initNameSymbols ++ digitSymbols
whitespaceSymbols = " \f\r\t\v"
pointSymbol       = '.'
exponentSymbols   = "eE"
quoteSymbol       = '"'

-- Keyword state transitions:
keywordTransitions :: (Automaton a, StateDomain a ~ LexerStateDomain) => [(State a, Char, State a)]
keywordTransitions =
    map3 (id, id, State) [
        ( InitState,   'l', L'    ),
        ( InitState,   'e', E'    ),
        ( InitState,   'i', I'    ),
        ( InitState,   'f', F'    ),
        ( InitState,   't', T'    ),
        ( State L',    'e', LE'   ),
        ( State LE',   't', LET   ),
        ( State E',    'n', EN'   ),
        ( State EN',   'd', END   ),
        ( State E',    'l', EL'   ),
        ( State EL',   's', ELS'  ),
        ( State ELS',  'e', ELSE  ),
        ( State EL',   'i', ELI'  ),
        ( State ELI',  'f', ELIF  ),
        ( State I',    'f', IF    ),
        ( State I',    'n', IN    ),
        ( State F',    'o', FO'   ),
        ( State FO',   'r', FOR   ),
        ( State F',    'a', FA'   ),
        ( State FA',   'l', FAL'  ),
        ( State FAL',  's', FALS' ),
        ( State FALS', 'e', FALSE ),
        ( State T',    'r', TR'   ),
        ( State TR',   'u', TRU'  ),
        ( State TRU',  'e', TRUE  )
    ]
-- String values associated with parts of keywords
keywordStrings :: (Automaton a, StateDomain a ~ LexerStateDomain) => [(State a, String)]
keywordStrings =
    map2 (State, id) [
        ( L',    "l"     ),
        ( LE',   "le"    ),
        ( LET,   "let"   ),
        ( E',    "e"     ),
        ( EN',   "en"    ),
        ( END,   "end"   ),
        ( EL',   "el"    ),
        ( ELS',  "els"   ),
        ( ELSE,  "else"  ),
        ( ELI',  "eli"   ),
        ( ELIF,  "elif"  ),
        ( I',    "i"     ),
        ( IF,    "if"    ),
        ( IN,    "in"    ),
        ( F',    "f"     ),
        ( FO',   "fo"    ),
        ( FOR,   "for"   ),
        ( FA',   "fa"    ),
        ( FAL',  "fal"   ),
        ( FALS', "fals"  ),
        ( FALSE, "false" ),
        ( T',    "t"     ),
        ( TR',   "tr"    ),
        ( TRU',  "tru"   ),
        ( TRUE,  "true"  ),
        ( N',    "n"     ),
        ( NU',   "nu"    ),
        ( NUL',  "nul"   ),
        ( NULL,  "null"  )
    ]
-- Semantics of keywords:
keywordTokens :: (Automaton a, StateDomain a ~ LexerStateDomain) => [(State a, TokenDomain)]
keywordTokens =
    map2 (State, id) [
        ( LET,   LetToken                            ),
        ( END,   EndToken                            ),
        ( IF,    IfToken                             ),
        ( ELIF,  ElifToken                           ),
        ( ELSE,  ElseToken                           ),
        ( FOR,   ForToken                            ),
        ( IN,    InToken                             ),
        ( FALSE, LiteralToken $ BooleanLiteral False ),
        ( TRUE,  LiteralToken $ BooleanLiteral True  ),
        ( NULL,  LiteralToken   NullLiteral          )
    ]

-- Parser: ---------------------------------------------------------------------

type Data = JSON

type UnaryOperation  = Data -> Data
type BinaryOperation = Data -> Data -> Data

data Expression =
    Computed Data |
    Unary UnaryOperation Expression | Binary BinaryOperation Expression Expression |
    Name String | Literal Literal |
    List [Expression] | Dictionary [(Expression, Expression)] |
    ErrorExpr
    --deriving (Eq, Show)

instance Eq Expression where
    Computed   x == Computed   y = x == y
    Name       x == Name       y = x == y
    Literal    x == Literal    y = x == y
    List       x == List       y = x == y
    Dictionary x == Dictionary y = x == y
    ErrorExpr    == ErrorExpr    = True
    _            == _            = False

instance Show Expression where
    show (Computed   x) = "Computed"--("   ++ show x ++ ")"
    show (Name       x) = "Name"--("       ++ x      ++ ")"
    show (Literal    x) = "Literal"--("    ++ show x ++ ")"
    show (List       x) = "List"--("       ++ show x ++ ")"
    show (Dictionary x) = "Dictionary"--(" ++ show x ++ ")"
    show  ErrorExpr     = "ErrorExpr"--()"
    show Unary  {} = "Unary"
    show Binary {} = "Unary"

data Body = ContentBody String | StatementBody Statement deriving (Eq, Show)

data Condition =
    Condition {
        condCheck  :: Expression,
        condBody   :: [Body]
    }
    deriving (Eq, Show)

data Statement =
    RootStatement {
        globalData  :: Data,
        rootBody    :: [Body]
    } |
    ConditionStatement [Condition] |
    LoopStatement {
        loopKey     :: Maybe String,
        loopVal     :: String,
        loopDomain  :: Expression,
        loopBody    :: [Body]
    } |
    DeclarationStatement {
        declVars :: [(String, Expression)],
        declBody :: [Body]
    } |
    InjectionStatement Expression
    deriving (Eq, Show)

type OutputDomain  = [OutputDomain']
data OutputDomain' =
    Name'          String                             |
    NamePair'      String String                      |
    Declaration    String Expression                  |
    Declarations [(String, Expression)]               |
    ForHead       (Maybe String) String Expression    |
    Condition'     Condition                          |
    Conditions    [Condition]                         |
    Injection      Expression                         |
    Statement'     Statement                          |
    Content'       String                             |
    Body          [Body]                              |
    -- Expressions:
    Single   Expression                | -- Single evald data
    Double  (Expression, Expression)   | -- Pair of evald data
    Singles [Expression]               | -- List of evald data
    Doubles [(Expression, Expression)] | -- List of pairs of evald data
    Error
    deriving (Eq, Show)

{-
instance Eq OutputDomain' where
    Error == Error = True
    _     == _     = False

instance Show OutputDomain' where
    show  Error      = "Err"
    show (Single  _) = "Single"
    show (Double  _) = "Double"
    show (Singles _) = "Singles"
    show (Doubles _) = "Doubles"
-}

data ParserStateDomain =
    BodyState       |
    ExprState       |
    EndState'       |
    ForState        |
    KeyValState     |
    InState         |
    DomState        |
    IfState         |
    ElifState       |
    ElifState'      |
    ElseState       |
    ElseState'      |
    LetState        |
    LetState'       |
    VarState        |
    InjectionState  |
    CloseState      |
    CloseLetState   |
    CloseCondsState |
    ElseCloseState  |
    ElifCloseState  |
    -- Expressions:
    ExState        | -- Expecting expression - literal / prefix operators / unary circumfix operators
    OpState        | -- Expecting operator   - infix operators
    OpenState      | -- Opened circumfix operator with no content
    CParState      | -- Closing parentheses
    CBracState     | -- Closing braces
    CBrackState      -- Closing brackets
    deriving (Eq, Show)

data StackDomain =
    Statement |
    Content   |
    For       |
    If        |
    Elif      |
    Else      |
    Cond      |
    IfCond    |
    Let       |
    Ass       |
    Inj       |
    -- Expressions:
    Not    | -- Negation
    UPlus  | -- Unary plus
    UMinus | -- Unary minus
    Paren  | -- Parentheses
    Brac   | -- Braces - Object literal
    Brack  | -- Brackets - Array literal
    Subscr | -- Subscript
    Mult   | -- Multiplication
    Div    | -- Division
    IDiv   | -- Integer division
    Mod    | -- Modulo
    Pow    | -- Exponentiation
    Plus   | -- Addition
    Minus  | -- Subtraction
    Concat | -- Concatenation
    LT     | -- Less than
    LE     | -- Less than or equal to
    GT     | -- Greater than
    GE     | -- Greater than or equal to
    Eq     | -- Equality
    NE     | -- Inequality
    And    | -- Conjuntion
    Or     | -- Disjunction
    Pair   | -- Key-value pair
    Comma    -- Array/object element separator
    deriving (Eq, Show)

statementTokens :: [TokenDomain]
statementTokens =
    [
        ForToken,
        LetToken,
        AssToken,
        IfToken,
        ElifToken,
        ElseToken,
        EndToken
    ]
statementStates :: (Automaton a, StateDomain a ~ ParserStateDomain) => [(TokenDomain, State a)]
statementStates =
    map2 (id, State) [
        (ForToken,  ForState),
        (LetToken,  LetState),
        (AssToken,  InjectionState),
        (IfToken,   IfState),
        (ElifToken, ElifState'),
        (ElseToken, ElseState'),
        (EndToken,  EndState')
    ]

-- Fixity and arity:
prefixOps        :: [StackSymbol StackDomain]
infixOps         :: [StackSymbol StackDomain]
circumfixOps     :: [StackSymbol StackDomain]
binCircumfixOps  :: [StackSymbol StackDomain]
prefixOps =
    map pure [
        Not,  -- !_
        UPlus, -- +_
        UMinus -- -_
    ]
infixOps =
    map pure [
        Mult,   -- _*_
        Div,    -- _/_
        IDiv,   -- _//_
        Mod,    -- _%_
        Pow,    -- _**_
        Plus,   -- _+_
        Minus,  -- _-_
        Concat, -- _++_
        LT,     -- _<_
        LE,     -- _<=_
        GT,     -- _>_
        GE,     -- _>=_
        Eq,     -- _==_
        NE,     -- _!=_
        And,    -- _&&_
        Or,     -- _||_
        Pair,   -- _:_
        Comma   -- _,_
    ]
circumfixOps =
    map pure [
        Paren, -- [_
        Brac,  -- {_
        Brack  -- [_
    ]
binCircumfixOps =
    map pure [
        Subscr  -- _[_
    ]

closingTokensMap :: [(StackSymbol StackDomain, TokenDomain)]
closingTokensMap =
    map2 (pure, id) [
        ( Paren,  CParToken   ),
        ( Brac,   CBracToken  ),
        ( Brack,  CBrackToken ),
        ( Subscr, CBrackToken )
    ]
closingInsertionsMap :: [(StackSymbol StackDomain, OutputDomain')]
closingInsertionsMap =
    map2 (pure, id) [
        ( Brac,  Doubles [] ),
        ( Brack, Singles [] )
    ]
closingStatesMap :: (Automaton a, StateDomain a ~ ParserStateDomain) => [(StackSymbol StackDomain, State a)]
closingStatesMap =
    map2 (pure, State) [
        ( Paren, CParState   ), -- _)
        ( Brac,  CBracState  ), -- _}
        ( Brack, CBrackState )  -- _]
    ]

-- Semantics of tokens at a given syntactic position:
opMap :: (Automaton a, StateDomain a ~ ParserStateDomain) => [(State a, TokenDomain, StackSymbol StackDomain)]
opMap =
    map3 (State, id, pure) [
        ( ExState, NotToken,    Not    ),
        ( ExState, PlusToken,   UPlus  ),
        ( ExState, MinusToken,  UMinus ),
        ( ExState, OParToken,   Paren  ),
        ( ExState, OBracToken,  Brac   ),
        ( ExState, OBrackToken, Brack  ),
        ( OpState, OBrackToken, Subscr ),
        ( OpState, MultToken,   Mult   ),
        ( OpState, DivToken,    Div    ),
        ( OpState, IDivToken,   IDiv   ),
        ( OpState, ModToken,    Mod    ),
        ( OpState, PowToken,    Pow    ),
        ( OpState, PlusToken,   Plus   ),
        ( OpState, MinusToken,  Minus  ),
        ( OpState, ConcatToken, Concat ),
        ( OpState, LTToken,     LT     ),
        ( OpState, LEToken,     LE     ),
        ( OpState, GTToken,     GT     ),
        ( OpState, GEToken,     GE     ),
        ( OpState, EqToken,     Eq     ),
        ( OpState, NEToken,     NE     ),
        ( OpState, AndToken,    And    ),
        ( OpState, OrToken,     Or     ),
        ( OpState, PairToken,   Pair   ),
        ( OpState, CommaToken,  Comma  )
    ]

-- Unary operations:
operationsMap =
    map2 (StackSymbol, id) [
        ( Not,    unary not' ),
        ( UPlus,  unary (0+) ),
        ( UMinus, unary (0-) ),
        ( Paren,  unary id   ),
        ( Brac,   dict       ),
        ( Brack,  list       )
    ]
    where
        unary f (Single x) = Single $ Unary f x
        unary _ _ = Error
        dict (Doubles x) = Single $ Dictionary x
        dict _ = Error
        list (Singles x) = Single $ List x
        list _ = Error
-- Binary operations:
binOperationsMap =
    map2 (StackSymbol, id) [
        ( Subscr, binary (@@)               ),
        ( Mult,   binary (*)                ),
        ( Div,    binary (/)                ),
        ( IDiv,   binary quot               ),
        ( Mod,    binary mod                ),
        ( Pow,    binary pow                ),
        ( Plus,   binary (+)                ),
        ( Minus,  binary (-)                ),
        ( Concat, binary (##)               ),
        ( LT,     binary $ Boolean .: (<)   ),
        ( LE,     binary $ Boolean .: (<=)  ),
        ( GT,     binary $ Boolean .: (>)   ),
        ( GE,     binary $ Boolean .: (>=)  ),
        ( Eq,     binary $ Boolean .: (==)  ),
        ( NE,     binary $ Boolean .: (/=)  ),
        ( And,    binary (&&.)              ),
        ( Or,     binary (||.)              ),
        ( Pair,   pair                      ),
        ( Comma,  cons                      )
    ]
    where
        binary f (Single x) (Single y) = Single $ Binary f x y
        binary _ _ _ = Error
        cons (Single x) (Singles l) = Singles $ x : l
        cons (Double x) (Doubles l) = Doubles $ x : l
        cons _ _ = Error
        pair (Single x) (Single y) = Double (x, y)
        pair _ _ = Error

-- Operator priorities:
priorities =
    mapl StackSymbol [
        [ Subscr ],
        [ Not, UPlus, UMinus ],
        [ Paren, Brac, Brack ],
        [ Pow ],
        [ Mult, Div, IDiv, Mod],
        [ Plus, Minus ],
        [ LT, LE, GT, GE ],
        [ Eq, NE ],
        [ And ],
        [ Or ],
        [ Pair ],
        [ Comma ]
    ]
-- Operator associativities: (Left by default)
data Assoc = LeftAssoc | RightAssoc deriving (Eq, Show)
associativities =
    map2 (StackSymbol, id) [
        ( Comma, RightAssoc ),
        ( Pow,   RightAssoc )
    ]