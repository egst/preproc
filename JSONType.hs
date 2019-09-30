{-# LANGUAGE ParallelListComp  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

module JSONType where

import Data.List (nub)

import Tools

-- For IsString, IsList instances:
import Data.String --(IsString, fromString)
import GHC.Exts --(IsList, Item, fromList, toList)
--import Data.Boolean
--import Data.Boolean.Overload

-- JSON data type: -------------------------------------------------------------

data JSON =
    Null |
    Boolean Bool |
    IntNumber Int |
    FloatNumber Double |
    String Prelude.String |
    Array [JSON] |
    Object [KeyValuePair]

-- Shorthands: -----------------------------------------------------------------

jNull = Null
jBool = Boolean
jINum = IntNumber
jFNum = FloatNumber
jStr  = String
jArr  = Array
jObj  = Object

type KeyValuePair = (Prelude.String, JSON)
-- Key-value pair constructor:
infixl 5 >:
(>:) :: Prelude.String -> JSON -> KeyValuePair
x >: y = (x, y)

-- Instances to integrate with other Haskell types: ----------------------------

instance Eq JSON where
    Null           == Null           = True
    Boolean     x' == Boolean     y' = x' == y'
    IntNumber   x' == IntNumber   y' = x' == y'
    FloatNumber x' == FloatNumber y' = x' == y'
    IntNumber   x' == FloatNumber y' = fromIntegral x' == y'
    FloatNumber x' == IntNumber   y' = x' == fromIntegral y'
    String      x' == String      y' = x' == y'
    Array       x' == Array       y' = x' == y'
    Object      x' == Object      y' = x' == y'
    _              == _              = False

    -- Simmilar to strict (in)equality in JS.

instance Enum JSON where
    toEnum i = IntNumber $ toEnum i
    fromEnum = toInt

instance Ord JSON where
    IntNumber   x' `compare` IntNumber   y' = compare x' y'
    FloatNumber x' `compare` FloatNumber y' = compare x' y'
    compare x y
        | isFloatNumber x = x               `compare` asFloatNumber y
        | isFloatNumber y = asFloatNumber x `compare` y
        | otherwise       = asIntNumber x   `compare` asIntNumber y

    IntNumber   x' <= IntNumber   y' = x' <= y'
    FloatNumber x' <= FloatNumber y' = x' <= y'
    x <= y
        | isFloatNumber x = x               <= asFloatNumber y
        | isFloatNumber y = asFloatNumber x <= y
        | otherwise       = asIntNumber x   <= asIntNumber y

    -- Performs conversion to IntNumber or FloatNumber, simmilar to JS.

instance Num JSON where
    IntNumber   x' + IntNumber   y' = IntNumber   $ x' + y'
    FloatNumber x' + FloatNumber y' = FloatNumber $ x' + y'
    x + y
        | isFloatNumber x = x + asFloatNumber y
        | isFloatNumber y = asFloatNumber x + y
        | otherwise       = asIntNumber x + asIntNumber y
    IntNumber   x' - IntNumber   y' = IntNumber   $ x' - y'
    FloatNumber x' - FloatNumber y' = FloatNumber $ x' - y'
    x - y
        | isFloatNumber x = x - asFloatNumber y
        | isFloatNumber y = asFloatNumber x - y
        | otherwise       = asIntNumber x - asIntNumber y
    IntNumber   x' * IntNumber   y' = IntNumber   $ x' * y'
    FloatNumber x' * FloatNumber y' = FloatNumber $ x' * y'
    x * y
        | isFloatNumber x = x * asFloatNumber y
        | isFloatNumber y = asFloatNumber x * y
        | otherwise       = asIntNumber x * asIntNumber y
    
    abs    (IntNumber   x') = IntNumber   $ abs x'
    abs    (FloatNumber x') = FloatNumber $ abs x'
    abs     x               = IntNumber   $ abs $ toInt x
    signum (IntNumber   x') = IntNumber   $ signum x'
    signum (FloatNumber x') = FloatNumber $ signum x'
    signum  x               = IntNumber   $ signum $ toInt x
    
    fromInteger = IntNumber . fromInteger

    -- Performs conversion to IntNumber or FloatNumber, simmilar to JS.

instance Real JSON where
    toRational (IntNumber   x') = toRational x'
    toRational (FloatNumber x') = toRational x'
    toRational  x               = toRational $ asIntNumber x

instance Integral JSON where
    quotRem x y = (IntNumber $ toInt x `quot` toInt y, IntNumber $ toInt x `rem` toInt y)
    toInteger x = toInteger $ toInt x

instance Fractional JSON where
    fromRational x' = FloatNumber $ fromRational x'
    x / y = FloatNumber $ toDouble x / toDouble y

    -- Performs conversion to FloatNumber, simmilar to JS.

instance RealFrac JSON where
    properFraction (IntNumber   x') = (fromIntegral x', FloatNumber 0)
    properFraction (FloatNumber x') = (i, FloatNumber f)
        where (i, f) = properFraction x'
    properFraction  x               = properFraction $ asIntNumber x

    -- Performs conversion to IntNumber, simmilar to JS.

instance Floating JSON where
    pi      = FloatNumber pi
    exp   x = FloatNumber $ exp   $ toDouble x
    log   x = FloatNumber $ log   $ toDouble x
    sin   x = FloatNumber $ sin   $ toDouble x
    cos   x = FloatNumber $ cos   $ toDouble x
    asin  x = FloatNumber $ asin  $ toDouble x
    acos  x = FloatNumber $ acos  $ toDouble x
    atan  x = FloatNumber $ atan  $ toDouble x
    sinh  x = FloatNumber $ sinh  $ toDouble x
    cosh  x = FloatNumber $ cosh  $ toDouble x
    asinh x = FloatNumber $ asinh $ toDouble x
    acosh x = FloatNumber $ acosh $ toDouble x
    atanh x = FloatNumber $ atanh $ toDouble x
    
    -- Performs conversion to FloatNumber, simmilar to JS.

instance RealFloat JSON where
    floatRadix     x = floatRadix     $ toDouble x
    floatDigits    x = floatDigits    $ toDouble x
    floatRange     x = floatRange     $ toDouble x
    isNaN          x = isNaN          $ toDouble x
    isInfinite     x = isInfinite     $ toDouble x
    isDenormalized x = isDenormalized $ toDouble x
    isNegativeZero x = isNegativeZero $ toDouble x
    isIEEE         x = isIEEE         $ toDouble x
    decodeFloat    x = decodeFloat    $ toDouble x
    encodeFloat      = (FloatNumber .) . encodeFloat
    
    -- Performs conversion to FloatNumber, simmilar to JS.

instance Show JSON where
    show (String x') = show x'
    show x = toString x

-- IsString, IsList: -----------------------------------------------------------

instance IsString JSON where
    fromString = String

instance IsList JSON where
    type Item JSON = JSON
    fromList = Array
    toList   = toJSONList

class IsBool t where
    true  :: t
    false :: t
    not'  :: t -> t
    (&&.) :: t -> t -> t
    (||.) :: t -> t -> t

instance IsBool JSON where
    true  = Boolean True
    false = Boolean False
    not'  = Boolean . not . toBool
    (&&.) = Boolean .: ((&&) .:. toBool)
    (||.) = Boolean .: ((||) .:. toBool)

-- Type checks: ----------------------------------------------------------------

isNull        :: JSON -> Bool
isBoolean     :: JSON -> Bool
isIntNumber   :: JSON -> Bool
isFloatNumber :: JSON -> Bool
isNumber      :: JSON -> Bool
isString      :: JSON -> Bool
isArray       :: JSON -> Bool
isObject      :: JSON -> Bool
isNull         Null           = True
isNull         _              = False
isBoolean     (Boolean _)     = True
isBoolean      _              = False
isIntNumber   (IntNumber _)   = True
isIntNumber    _              = False
isFloatNumber (FloatNumber _) = True
isFloatNumber  _              = False
isNumber      (IntNumber _)   = True
isNumber      (FloatNumber _) = True
isNumber       _              = False
isString      (String _)      = True
isString       _              = False
isArray       (Array  _)      = True
isArray        _              = False
isObject      (Object _)      = True
isObject       _              = False

-- Type conversions: -----------------------------------------------------------

asBoolean     :: JSON -> JSON
asIntNumber   :: JSON -> JSON
asFloatNumber :: JSON -> JSON
asNumber      :: JSON -> JSON
asString      :: JSON -> JSON
asArray       :: JSON -> JSON
asObject      :: JSON -> JSON

toBool       :: JSON -> Bool
toInt        :: JSON -> Int
toDouble     :: JSON -> Double
toNum        :: Num n => JSON -> n -- TODO
toString     :: JSON -> String
toJSONList   :: JSON -> [JSON]
toBoolList   :: JSON -> [Bool]
toIntList    :: JSON -> [Int]
toDoubleList :: JSON -> [Double]
toNumList    :: Num n => JSON -> [n]
toStringList :: JSON -> [String]

values  :: JSON -> JSON
keys    :: JSON -> JSON
entries :: JSON -> JSON

asBoolean x = case x of
    Null            -> Boolean False
    Boolean _       -> x
    IntNumber x'
        | x' == 0   -> Boolean False
        | otherwise -> Boolean True
    FloatNumber x'
        | x' == 0   -> Boolean False
        | otherwise -> Boolean True
    String x'
        | x' == ""  -> Boolean False
        | otherwise -> Boolean True
    Array x'
        | null x'   -> Boolean False
        | otherwise -> Boolean True
    Object x'
        | null x'   -> Boolean False
        | otherwise -> Boolean True

asIntNumber x = case x of
    Null            -> IntNumber 0
    Boolean x'
        | x'        -> IntNumber 1
        | otherwise -> IntNumber 0
    IntNumber   _   -> x
    FloatNumber _   -> IntNumber $ round x
    String      x'  -> IntNumber $ length x' -- TODO: read x'
    Array       x'  -> IntNumber $ length x'
    Object      x'  -> IntNumber $ length x'

asFloatNumber x = case x of
    FloatNumber _ -> x
    _             -> FloatNumber $ fromIntegral x

asNumber x
    | isFloatNumber x = x
    | otherwise       = asIntNumber x

asString x = case x of
    Null            -> String "null"
    Boolean x'
        | x'        -> String "true"
        | otherwise -> String "false"
    IntNumber   x'  -> String $ show x'
    FloatNumber x'  -> String $ show x'
    String      _   -> x
    Array       x'  -> String $ "[" ++ strArray  x' ++ "]"
    Object      x'  -> String $ "{" ++ strObject x' ++ "}"
    where
        join delim = drop (length delim) . concatMap (delim ++)
        joinPairs outerDelim innerDelim = join outerDelim . map (joinPair innerDelim)
        joinPair delim (first, second) = first ++ delim ++ second
        showPair (first, second) = (show first, show second)
        strArray = join ", " . map show
        strObject = joinPairs ", " ": " . map showPair

asArray = values
asObject x
    | isArray  x = entries x
    | isObject x = x
    | otherwise  = Object []

values x = case x of
    Array  _  -> x
    Object x' -> Array [val | (_, val) <- x']
    _         -> Array []
keys x = case x of
    Array  x' -> Array [IntNumber n | n <- [0 .. length x' - 1]]
    Object x' -> Array [String key | (key, _) <- x']
    _         -> Array []
entries x = Object [(toString key, val) | key <- toList $ keys x | val <- toList $ values x]

toBool x = x'
    where Boolean x' = asBoolean x
toInt x = x'
    where IntNumber x' = asIntNumber x
toDouble x = x'
    where FloatNumber x' = asFloatNumber x
toNum x
    {-| isFloatNumber x = toRational x
    | otherwise       -}= fromIntegral $ asIntNumber x -- TODO
toString x = x'
    where String x' = asString x
toJSONList x = x'
    where Array x' = asArray x
toBoolList   = map toBool   . toJSONList
toIntList    = map toInt    . toJSONList
toDoubleList = map toDouble . toJSONList
toNumList    = map toNum    . toJSONList
toStringList = map toString . toJSONList

-- Operations: -----------------------------------------------------------------

at :: JSON -> JSON -> JSON

infixl 9 @@
infixr 5 ##
(@@) = flip at
(##) = cat

newtype KeyValWrap = KVW (String, JSON)
instance Eq KeyValWrap where
    KVW (x, _) == KVW (y, _) = x == y
unwrap (KVW (k, v)) = (k, v)
merge = reverse .: nub .: reverse .: (++)

cat (String x') (String y') = String $ x' ++ y'
cat (Array  x') (Array  y') = Array  $ x' ++ y'
cat (Object x') (Object y') = Object $ (map unwrap .: merge .:. map KVW) x' y'
cat x y = asString x `cat` asString y

at key json
    | isArray  json && isNumber key = arrayAt  (toInt    key) json
    | isObject json && isString key = objectAt (toString key) json
    | otherwise               = Null

arrayAt i (Array list)
    | i < 0 || i >= length list = Null
    | otherwise                 = list !! i
arrayAt _ _ = Null

objectAt s (Object ((key, val) : rest))
    | s == key = val
    | s /= key = objectAt s (Object rest)
objectAt _ _    = Null

pow :: JSON -> JSON -> JSON
IntNumber x' `pow` IntNumber y'
    | y' > 0    = IntNumber   $ x' ^ y'
    | otherwise = FloatNumber $ fromIntegral x' ^^ y'
FloatNumber x'    `pow` FloatNumber y'    = FloatNumber $ x' ** y'
x@(FloatNumber _) `pow` y@(IntNumber   _) = x `pow` asFloatNumber y
x@(IntNumber   _) `pow` y@(FloatNumber _) = asFloatNumber x `pow` y
x                 `pow` y                 = asIntNumber x `pow` asIntNumber y