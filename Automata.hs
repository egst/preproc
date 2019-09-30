{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances      #-} -- TODO

module Automata where

import Tools

-- Automaton state: ------------------------------------------------------------
-- Provides generic states InitState, FailState, EndState,
-- Sub to call subroutines, and ErrorState to report errors.

data State a = Automaton a =>
    State (StateDomain a) | InitState | EndState |
    Sub { toAutomaton :: a, passInput :: Bool } |
    Return { toState :: State a, returnInput :: Bool } |
    FailState | ErrorState String

passTo     :: (Automaton a) => a       -> State a -- Call sub-routine, keep input
continueTo :: (Automaton a) => a       -> State a -- Call sub-routine, next input
backTo     :: (Automaton a) => State a -> State a -- Return from sub-routine, keep input
returnTo   :: (Automaton a) => State a -> State a -- Return from sub-routine, next input
passTo     a = Sub    a True
continueTo a = Sub    a False
backTo     s = Return s True
returnTo   s = Return s False

instance Eq (StateDomain a) => Eq (State a) where
    State x    == State y    = x == y
    InitState  == InitState  = True
    FailState  == FailState  = True
    EndState   == EndState   = True
    --Sub (a, x) == Sub (a, y) = x == y -- TODO
    _          == _          = False

instance (Show (StateDomain a), Show (InDomain a)) => Show (State a) where
    show (State x) = '#' : show x
    show InitState = "#init"
    show FailState = "#fail"
    show EndState  = "#end"
    show (Sub    a i) = "#sub("    ++ show i ++ ")->InitState"
    show (Return s i) = "#return(" ++ show i ++ ")->" ++ show s

{-
instance Wrapper State where
    fallback = FailState

    wraps (State x) = True
    wraps _         = False

    get (State x) = pure x
    get _         = fallback-}

{-
instance Functor State where
    --fmap f (State x) = State $ f x
    --fmap _ s         = s
    fmap _ _         = FailState

instance Applicative State where
    pure = State

    --State f <*> x = fmap f x
    --s       <*> _ = s
    _ <*> _ = FailState
-}

-- PDA stack symbol: -----------------------------------------------------------
-- Provides generic symbols InitSymbol and FailSymbol.

data StackSymbol stackDomain = StackSymbol stackDomain | InitSymbol | FailSymbol
    deriving Eq

instance Show stackDomain => Show (StackSymbol stackDomain) where
    show (StackSymbol x) = '&' : show x
    show InitSymbol      = "&init"
    show FailSymbol      = "&fail"

instance Functor StackSymbol where
    fmap f (StackSymbol x) = StackSymbol $ f x
    fmap _ InitSymbol      = InitSymbol
    fmap _ FailSymbol      = FailSymbol

instance Applicative StackSymbol where
    pure = StackSymbol

    StackSymbol f <*> x = fmap f x
    InitSymbol    <*> _ = InitSymbol
    FailSymbol    <*> _ = FailSymbol

-- Automaton input symbol: -----------------------------------------------------
-- Provides generic symbols EmptySymbol and EndSymbol.

data Symbol domain = Symbol domain | EmptySymbol | EndSymbol
    deriving Eq

instance Show domain => Show (Symbol domain) where
    show (Symbol x)  = '$' : show x
    show EmptySymbol = "$empty"
    show EndSymbol   = "$end"

instance Functor Symbol where
    fmap f (Symbol x)  = Symbol $ f x
    fmap _ EmptySymbol = EmptySymbol
    fmap _ EndSymbol   = EndSymbol

instance Applicative Symbol where
    pure = Symbol

    Symbol f    <*> x = fmap f x
    EmptySymbol <*> _ = EmptySymbol
    EndSymbol   <*> _ = EndSymbol

-- Automaton output: -----------------------------------------------------------
-- Provides generic output NoOutput.

data Output outputDomain = Output outputDomain | NoOutput
    deriving Eq

instance Show outputDomain => Show (Output outputDomain) where
    show (Output x) = '*' : show x
    show NoOutput   = "*noout"

instance Functor Output where
    fmap f (Output x) = Output $ f x
    fmap _ NoOutput   = NoOutput

instance Applicative Output where
    pure = Output

    Output f <*> x = fmap f x
    NoOutput <*> _ = NoOutput

-- Automaton class: ------------------------------------------------------------
-- Provides generic interface for all automata.

type Transition a = a -> a
type ASymbol a = Symbol (InDomain    a)
type AOutput a = Output (OutDomain   a)

{-
next' :: Automaton a => a -> a
next' a = case state a of
    Sub sub True       -> setState (ErrorState "Cannot pass input to subroutine.") a
    Sub sub False      -> run (a --> sub) ==> a
    Return state True  -> setState (ErrorState "Cannot pass input from subroutine.") a
    Return state False -> setState state $ nextOrLambda a
    State {}           -> nextOrSub a (nextOrLambda a)
    _                  -> nextOrLambda a
-}

nextn :: Automaton a => Int -> a -> a
nextn n a
    | n == 0    = a
    | otherwise = next' $ nextn (n - 1) a

run :: Automaton a => a -> a
run a = case state a of
    EndState      -> a
    FailState     -> a
    ErrorState {} -> a
    Return     {} -> a
    _             -> run $ next' a

next' :: Automaton a => a -> a
next' a = case state $ next a of
    FailState     -> nextOrSub a (lambda a)
    ErrorState {} -> nextOrSub a (lambda a)
    _             -> nextOrSub a (next   a)
    where
        nextOrSub a nextOne = case state nextOne of
            Sub    sub   True  -> run (setInput (input a) nextOne --> sub) ==> nextOne
            Sub    sub   False -> run (nextOne --> sub) ==> nextOne
            Return state True  -> setInput (input a) nextOne
            _                  -> nextOne

returnState :: Automaton a => a -> State a
returnState a = case state a of
    Return { toState = s } -> s
    _                      -> state a

class Automaton a where
    -- Minimal complete definition:
    -- types:   StateDomain, InDomain, OutDomain
    -- methods: state, setState, input, setInput, output, setOutput, transition, setTransition, construct, define
    -- For additional data or different situation representation it's necessary to define:
    -- types:   Situation | TransitionDefinition

    type StateDomain a -- > State a
    type InDomain a    -- > [Symbol (InDomain a)]
    type OutDomain a   -- > Output (OutDomain a)

    type Situation a                      -- Usualy a tuple describing the automaton.
    type TransitionDefinition a           -- Usualy a function of type Situation a -> Situation a.
    type Situation a = (State a, [ASymbol a], AOutput a)
    type TransitionDefinition a = Situation a -> Situation a

    -- Interface:
    state         :: a -> State a
    setState      :: State a -> a -> a
    input         :: a -> [ASymbol a]
    setInput      :: [ASymbol a] -> a -> a
    output        :: a -> AOutput a
    setOutput     :: AOutput a -> a -> a
    transition    :: a -> Transition a
    setTransition :: Transition a -> a -> a

    -- Construct the automaton from state, input, output and transition:
    -- All additional data must be already defined.
    construct :: State a -> [ASymbol a] -> AOutput a -> Transition a -> a

    -- Make transition (a -> a) from a more readable transition definition (Situation a -> Situation a)
    define :: TransitionDefinition a -> Transition a
    --define :: (Situation a -> Situation a) -> Transition a

    infixl 4 -->
    infixl 4 ==>
    infixl 4 >>>

    initial    :: a                          -- Initial automaton.
    reset      :: a -> a                     -- Reset to the initial automaton.
    load       :: [ASymbol a] -> a -> a      -- Load input.
    next       :: a -> a                     -- Next automaton state after reading 1 input symbol.
    --nextn      :: Integral i => i -> a -> a  -- Automaton state after reading n input symbols.
    lambda     :: a -> a                     -- Next automaton state after lambda transition - not reading input.
    --run        :: a -> a                     -- Automaton state after reading input until reaching a final state.
    safeInput  :: a -> [ASymbol a]           -- Automaton remaining non-empty input.
    (>>>)      :: (Automaton b, OutDomain b ~ [InDomain a]) => b -> a -> a -- Load another automaton output as input.
    (-->)      :: (Automaton b, a ~ b) => a -> b -> b -- Go to sub-routine automaton.
    (==>)      :: (Automaton b, a ~ b) => b -> a -> a -- Return from a sub-routine automaton.
    -- See the default implementations for specific details.

    -- Initial automaton with identity transition and all the additional data predefined in "construct":
    initial = construct InitState [] NoOutput id

    -- Keeping the transition and all the additional data predefined in "construct":
    reset = construct InitState [] NoOutput . transition

    -- Reseting the automaton:
    load i = setInput i . reset

    (>>>) b = case (output b, state b) of
        (Output o, EndState) -> load $ map Symbol o
        (_, FailState)       -> setState FailState      . load []
        (_, ErrorState e)    -> setState (ErrorState e) . load []

    next a = transition a a
    
    lambda a = transition a $ setInput (EmptySymbol : input a) a

    -- Subroutines share the same output and read the same input:
    a --> sub = setState InitState $ setOutput (output a) $ setInput (safeInput a) sub
    sub ==> a = setState (returnState sub) $ setOutput (output sub) $ setInput (safeInput sub) a
    
    -- Returning EndSymbol on input end:
    safeInput a
        | null $ input a = [EndSymbol]
        | otherwise      = input a

-- Push-down automaton: --------------------------------------------------------

data PDA stackD stateD inD outD =
    PDA {
        stack         :: [StackSymbol stackD],
        pdaState      :: State (PDA stackD stateD inD outD),
        pdaInput      :: [Symbol inD],
        pdaOutput     :: Output outD,
        pdaTransition :: Transition (PDA stackD stateD inD outD)
    }

safeStack :: PDA stackD stateD inD outD -> [StackSymbol stackD]
safeStack a = case stack a of
    [] -> [InitSymbol]
    _  -> stack a

setStack :: [StackSymbol stackD] -> PDA stackD stateD inD outD -> PDA stackD stateD inD outD
setStack s a = a { stack = s }

push :: Eq stackD => StackSymbol stackD -> PDA stackD stateD inD outD -> PDA stackD stateD inD outD
pop  :: Eq stackD => StackSymbol stackD -> PDA stackD stateD inD outD -> PDA stackD stateD inD outD
push x a = setStack (x : stack a) a
pop  x a = case stack a of
    first : rest | x == first -> setStack rest a
    _ -> a

instance Eq stackD => Automaton (PDA stackD stateD inD outD) where
    type StateDomain (PDA stackD stateD inD outD) = stateD
    type InDomain    (PDA stackD stateD inD outD) = inD
    type OutDomain   (PDA stackD stateD inD outD) = outD
    type TransitionDefinition (PDA stackD stateD inD outD) =
        (StackSymbol stackD, State (PDA stackD stateD inD outD), Symbol inD, Output outD)
            -> ([StackSymbol stackD], State (PDA stackD stateD inD outD), Output outD)

    state             = pdaState
    setState      x a = a { pdaState = x }
    input             = pdaInput
    setInput      x a = a { pdaInput = x }
    output            = pdaOutput
    setOutput     x a = a { pdaOutput = x }
    transition        = pdaTransition
    setTransition x a = a { pdaTransition = x }

    construct = PDA [InitSymbol]

    define def a =
        a {
            stack     = newStack ++ tail (stack a),
            pdaState  = newState,
            pdaInput  = inputRest,
            pdaOutput = newOutput
        }
        where
            (newStack, newState, newOutput) = def (head $ safeStack a, state a, i, output a)
            i : inputRest = safeInput a
    
    -- Subroutines share stack with InitSymbol separator:
    a --> sub = push InitSymbol $ setStack (stack a) $ setState InitState $ setOutput (output a) $ setInput (safeInput a) sub
    sub ==> a = pop InitSymbol $ setStack (stack a) $ setState (returnState sub) $ setOutput (output sub) $ setInput (safeInput sub) a

-- Finite state automaton: -----------------------------------------------------

data FSA stateD inD outD =
    FSA {
        fsaState      :: State (FSA stateD inD outD),
        fsaInput      :: [Symbol inD],
        fsaOutput     :: Output outD,
        fsaTransition :: Transition (FSA stateD inD outD)
    }

instance Automaton (FSA stateD inD outD) where
    type StateDomain          (FSA stateD inD outD) = stateD
    type InDomain             (FSA stateD inD outD) = inD
    type OutDomain            (FSA stateD inD outD) = outD
    type TransitionDefinition (FSA stateD inD outD) = (State (FSA stateD inD outD), Symbol inD, Output outD) -> (State (FSA stateD inD outD), Output outD)

    state             = fsaState
    setState      x a = a { fsaState = x }
    input             = fsaInput
    setInput      x a = a { fsaInput = x }
    output            = fsaOutput
    setOutput     x a = a { fsaOutput = x }
    transition        = fsaTransition
    setTransition x a = a { fsaTransition = x }

    construct = FSA

    define def a =
        a {
            fsaState  = newState,
            fsaInput  = inputRest,
            fsaOutput = newOutput
        }
        where
            (newState, newOutput) = def (state a, i, output a)
            i : inputRest = safeInput a

-- Basic automaton type synonyms: ----------------------------------------------

type Lexer            stateD tokenD             = FSA stateD Char [tokenD]
type Parser           stackD stateD tokenD outD = PDA stackD stateD tokenD outD
type SimpleParser     stateD tokenD outD        = FSA stateD tokenD outD
type SimpleCharParser stateD outD               = SimpleParser stateD Char outD