module PreprocAST where

import Tools
import PreprocSetup
import JSONType

type Context = [Declaration]
type Declaration = (String, Data)

infixl 4 <--
context <-- (name, value) = declare context name value
infixl 4 <==
(<==) = declareAll

declare :: Context -> String -> Data -> Context
declare context name value = case context of
    first @ (n, _) : rest
        | n == name -> (name, value) : rest
        | otherwise -> first : declare rest name value
    []              -> [(name, value)]

declareAll :: Context -> [Declaration] -> Context
declareAll context declarations = case declarations of
    (name, value) : rest -> declareAll (declare context name value) rest
    []                   -> context

resolve :: Context -> String -> Data
resolve context name = case context of
    (n, value) : rest
        | n == name   -> value
        | otherwise   -> resolve rest name
    []                -> Null

eval :: Context -> Expression -> Data
eval context expression = case expression of
    Computed x -> x

    Unary  f x   -> f $ eval context x
    Binary f x y -> f (eval context x) (eval context y)
    Name   name  -> resolve context name

    Literal NullLiteral           -> Null
    Literal (BooleanLiteral b)    -> Boolean b
    Literal (NumberLiteral i 0 e) -> IntNumber   $ i * 10^e
    Literal (NumberLiteral i d e) -> FloatNumber $ fromIntegral i * 10 ** fromIntegral e + read ("0." ++ show d)
    Literal (StringLiteral s)     -> String s

    List       exprs     -> Array  [eval context expr | expr <- exprs]
    Dictionary exprPairs -> Object [(toString $ eval context key, eval context val) | (key, val) <- exprPairs]

evalDeclarations :: Context -> [(String, Expression)] -> [Declaration]
evalDeclarations context declarations = [(name, eval context expr) | (name, expr) <- declarations]

process :: Context -> Body -> String
process context body = case body of
    ContentBody   content   -> content
    StatementBody statement -> exec context statement

processAll context = concatMap (process context)

exec :: Context -> Statement -> String
exec context statement = case statement of
    RootStatement global body      -> processAll (context <-- ("data", global)) body
    ConditionStatement conditions  -> execConditions context conditions
    LoopStatement key val dom bod  -> execLoop context (key, val, dom, bod)
    DeclarationStatement vars body -> processAll (context <== evalDeclarations context vars) body
    InjectionStatement expr        -> toString $ eval context expr

execConditions :: Context -> [Condition] -> String
execConditions context conditions = case conditions of
    first : rest
        | toBool $ eval context (condCheck first) -> processAll context $ condBody first
        | otherwise                                   -> execConditions context rest
    _                                                 -> ""

execLoop :: Context -> (Maybe String, String, Expression, [Body]) -> String
execLoop context (mkey, value, domain, body) = case mkey of
    Just key -> concat [processAll (context <-- (value, v) <-- (key, jStr k)) body | (k, v) <- objectDomain]
    Nothing  -> concat [processAll (context <-- (value, v))                   body | v      <- arrayDomain]
    where
        Object objectDomain = asObject (eval context domain)
        Array  arrayDomain  = asArray  (eval context domain)
