--- Given Library Code
--- ==================

module Lib where

type ErrorMsg = String
-- for stack underflow errors
msgUnderflow :: ErrorMsg
msgUnderflow = "Stack underflow"

-- for definition with no name
msgZeroLenDef :: ErrorMsg
msgZeroLenDef = "Attempt to use zero-length string as a name"

-- for interpreting compile-only words
msgCompileOnly :: ErrorMsg
msgCompileOnly = "Interpreting a compile-only word"

-- for undefined symbol
msgUndefinedSym :: String -> ErrorMsg
msgUndefinedSym name = "Undefined symbol: '" ++ name ++ "'"

-- for unstructured control keywords
msgUnstructured :: String -> ErrorMsg
msgUnstructured op = "Unstructured control: " ++ op

underflow :: a
underflow = error msgUnderflow

--- The Types
--- ---------

type ForthState = (IStack, Dictionary, Output)
type Transition = (ForthState -> ForthState)
type IStack     = [Integer]
type Dictionary = [(String, Value)]
type Output     = [String]

type CStack     = [(String, Transition)]

data Value = Prim Transition
           | Define
           | EndDef
           | Compile (CStack -> Maybe CStack)
           | Num Integer
           | Unknown String

instance Show Value where
    show (Prim f)    = "Prim"
    show (Compile _) = msgCompileOnly
    show Define      = "Define"
    show EndDef      = msgCompileOnly
    show (Num i)     = show i
    show (Unknown s) = msgUndefinedSym s

--- Dictionary Access
--- -----------------

--- ### Lookups

-- handle input lookups (and integers)
dlookup :: String -> Dictionary -> Value
dlookup word dict
    = case lookup word dict of
        Just x -> x
        _          -> case reads word of
                        [(i,"")] -> Num i
                        _        -> Unknown word

--- ### Insert

-- handle inserting things into the dictionary
dinsert :: String -> Value -> Dictionary -> Dictionary
dinsert key val dict = (key, val):dict

--- Problems
--- ========

--- Lifters
--- -------

liftIStackOp :: (IStack -> Maybe IStack) -> ForthState -> ForthState
liftIStackOp op (i, d, o)
    = case op i of
        Just i' -> (i', d, o)
        Nothing -> underflow

--- ### `liftIntOp`

liftIntOp :: (Integer -> Integer -> Integer) -> IStack -> Maybe IStack
liftIntOp op (x:y:xs) = Just $ (y `op` x) : xs
liftIntOp _  _        = Nothing

--- ### `liftCompOp`

liftCompOp :: (Integer -> Integer -> Bool) -> IStack -> Maybe IStack
liftCompOp op (x:y:xs) = Just ((if (y `op` x) then -1 else 0) : xs)
liftCompOp _ _ = Nothing

--- The Dictionary
--- --------------

initialDictionary :: Dictionary
initialDictionary = initArith ++ initComp ++ initIStackOp ++ initPrintOp
                 ++ initCompileOp

initCompileOp = [ ("✏️📖",    Define)
                , ("📕",    EndDef)
                , ("💫",  Compile cstackFor)
                , ("⏭️", Compile cstackNext)
                , ("🤔",   Compile cstackIf)
                , ("🙊", Compile cstackElse)
                , ("🤖", Compile cstackThen)
                , ("😍",Compile cstackBegin)
                , ("😥",Compile cstackUntil)
                ]

--- ### Arithmetic Operators

initArith :: Dictionary
initArith = [ ("➕",  Prim $ liftIStackOp $ liftIntOp (+)),
              ("➖", Prim $ liftIStackOp $ liftIntOp (-)),
              ("❌", Prim $ liftIStackOp $ liftIntOp (*)),
              ("➗", Prim $ liftIStackOp $ liftIntOp (div))
            ]

--- ### Comparison Operators

initComp :: Dictionary
initComp = [("☹️⚖️", Prim $ liftIStackOp $ liftCompOp (<)),
            ("😀⚖️", Prim $ liftIStackOp $ liftCompOp (>)),
            ("☹️⚖️🤷", Prim $ liftIStackOp $ liftCompOp (<=)),
            ("😀⚖️🤷", Prim $ liftIStackOp $ liftCompOp (>=)),
            ("🤷⚖️", Prim $ liftIStackOp $ liftCompOp (==)),
            ("😠🤷⚖️", Prim $ liftIStackOp $ liftCompOp (/=))
           ]

--- ### Stack Manipulations

initIStackOp :: Dictionary
initIStackOp = [ ("🪞",  Prim $ liftIStackOp istackDup),
                 ("🙃",  Prim $ liftIStackOp istackSwap),
                 ("💦",  Prim $ liftIStackOp istackDrop),
                 ("☀️",  Prim $ liftIStackOp istackRot)
               ]

initPrintOp = [ ("🖨️",  Prim printPop),
                ("🖨️🎉",  Prim printStack)
              ]

istackDup :: IStack -> Maybe IStack
istackDup (i:is) = Just $ i:i:is
istackDup _      = Nothing

istackSwap :: IStack -> Maybe IStack
istackSwap (i:j:is) = Just $ j:i:is
istackSwap _ = Nothing

istackDrop :: IStack -> Maybe IStack
istackDrop (i:is) = Just is
istackDrop _ = Nothing

istackRot :: IStack -> Maybe IStack
istackRot (i:j:k:is) = Just (k:i:j:is)
istackRot _ = Nothing

--- ### Popping the Stack

printPop :: ForthState -> ForthState
printPop (i:istack, dict, out) =
    (istack, dict, show i : out)
printPop _ = underflow

--- ### Printing the Stack

printStack :: ForthState -> ForthState
printStack (is, dict, out) = (is, dict, (unwords . map show . reverse) is:out)

--- Evaluator
--- ---------

eval :: [String] -> ForthState -> ForthState

-- empty input -> return current state and output "ok"
eval [] (istack, dict, out) = (istack, dict, "ok":out)

--- ### Lookup in dictionary

-- otherwise it should be handled by `dlookup` to see if it's a `Num`, `Prim`,
-- `Define`, `Compile`, or `Unknown`
eval (w:ws) state@(istack, dict, out)
    = case dlookup w dict of
        Prim f    -> eval ws (f state)
        Num i     -> eval ws (i:istack, dict, out)
        Define    ->
            case compileDef ws dict of
                Right (rest, dict') -> eval rest (istack, dict', out)
                Left msg -> ([], dict, msg:out)
        otherwise -> -- reset IStack and add error message
            ([], dict, (show otherwise):out)

--- Compiler
--- --------

--- ### Definite Loops
transForLoop :: Transition -> (ForthState -> ForthState)
transForLoop kloop (i:is, d, o) =
    if i < 0 then
        (is, d, o)
    else
        (aux kloop i) (is, d, o)
        where aux k 0 = k
              aux k n = k . (aux k (n-1))
transForLoop _ _ = underflow


cstackFor :: CStack -> Maybe CStack
cstackFor cstack = Just $ ("for", id):cstack

cstackNext :: CStack -> Maybe CStack
cstackNext (("for", kloop):(c, kold):cstack) =
    Just ((c, knew):cstack) where knew = (transForLoop kloop) . kold
cstackNext _ = Nothing

--- ### Conditionals

transIfElse :: Transition -> Transition -> (ForthState -> ForthState)
transIfElse kif kelse (i:is, d, o) =
    if i == 0 then kelse (is, d, o)
    else kif (is, d, o)
transIfElse _ _ _ = underflow

transIf :: Transition -> (ForthState -> ForthState)
transIf kif (i:is, d, o) =
    if i /= 0 then kif (is, d, o)
    else (is, d, o)
transIf _ _ = underflow

cstackIf :: CStack -> Maybe CStack
cstackIf cstack = Just $ ("if", id):cstack

cstackElse :: CStack -> Maybe CStack
cstackElse cstack@(("if", _):_) = Just $ ("else", id):cstack
cstackElse _ = Nothing

cstackThen :: CStack -> Maybe CStack
cstackThen (("else", kelse):("if", kif):(c, kold):cstack) =
  Just ((c, knew):cstack) where knew = (transIfElse kif kelse) . kold
cstackThen (("if", kif):(c, kold):cstack) =
  Just ((c, knew):cstack) where knew = (transIf kif) . kold
cstackThen _ = Nothing

--- ### Indefinite Loops
transBeginLoop :: Transition -> (ForthState -> ForthState)
transBeginLoop kloop (istack, d, o) =
  let (i:is, nd, no) = kloop (istack, d, o)
  in if i == 0 then transBeginLoop kloop (is, nd, no) else (is, nd, no)
transBeginLoop _ _ = underflow

cstackBegin :: CStack -> Maybe CStack
cstackBegin cstack = Just $ ("begin", id):cstack

cstackUntil :: CStack -> Maybe CStack
cstackUntil (("begin", kloop):(c, kold):cstack) =
  Just ((c, knew):cstack) where knew = (transBeginLoop kloop) . kold
cstackUntil _ = Nothing


--- ### Lookup in dictionary

-- | Return rest of words after compilation and a dictionary w/ new defintion
-- Assuming ':' is already stripped away
compileDef :: [String] -> Dictionary -> Either ErrorMsg ([String], Dictionary)
compileDef [] _ = Left msgZeroLenDef
compileDef (name:ws) dict
    = case  compile ws dict [("", id)] of
       Right (rest, f) -> Right (rest, dinsert name (Prim f) dict)
       Left msg -> Left msg

compile :: [String] -> Dictionary -> CStack
           -> Either ErrorMsg ([String], Transition)
compile [] _ _ = Left "The definition does not end"

compile (w:ws) dict cstack
    = case dlookup w dict of
        Prim f    -> compile ws dict (updateTop f cstack)
        Num i     -> compile ws dict (updateTop f cstack)
                     where f = (liftIStackOp (\is -> Just (i:is)))
        Compile cf-> case cf cstack of
                Just cstack' -> compile ws dict cstack'
                Nothing -> Left $ msgUnstructured w
        Define    -> Left "Nested definition is not allowed"
        EndDef    -> case cstack of
            [("", k)] -> Right (ws, k)
            otherwise -> Left $ msgUnstructured w
        otherwise -> Left $ show otherwise

updateTop :: Transition -> CStack -> CStack
updateTop k ((c, kold):cs) = (c, k . kold) : cs
updateTop _ [] = underflow

