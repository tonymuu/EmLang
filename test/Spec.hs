import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.QuickCheck.Exception

import Data.List (isPrefixOf)

import Lib

main :: IO ()
main = defaultMain tests

tests =
  [ testGroup "=G= Lifter Functions"
      [ testProperty "=P= Lift Comparison"             prop_liftComp
      ]
  , testGroup "=G= Dictionary for primitive operators"
      [ testProperty "=P= Arithmetic Operators"        prop_dlookup_initArith
      , testProperty "=P= Comparison Operators"        prop_dlookup_initComp
      , testProperty "=P= IStack Manipulations `dup`"  prop_istack_dup
      , testProperty "=P= IStack Manipulations `swap`" prop_istack_swap
      , testProperty "=P= IStack Manipulations `drop`" prop_istack_drop
      , testProperty "=P= IStack Manipulations `rot`"  prop_istack_rot
      , testProperty "=P= Printing IStack `.S`"        prop_istack_printStack
      ]
  , testGroup "=G= Compiler"
      [ testProperty "=P= Primitive operators"         prop_compile_prim
      , testProperty "=P= Conditionals"                prop_compile_if
      , testProperty "=P= Indefinite Loops"            prop_compile_loop
      ]
  , testGroup "=G= Evaluator"
      [ testProperty "=P= `eval` matches `compile`"    prop_it_evalEqComp
      , testProperty "=P= Max"                         prop_it_max
      , testProperty "=P= Factorial"                   prop_it_fact
      ]
  ]

msgOperatorNotFound :: Property -> Property
msgOperatorNotFound = counterexample "Cannot find operator in dictionary"

msgNoUnderflow :: Property -> Property
msgNoUnderflow = counterexample "No exception thrown for underflow!"

msgIStackNotMatch :: Property -> Property
msgIStackNotMatch = counterexample "IStack does not match:"

msgCStackNotMatch :: Property -> Property
msgCStackNotMatch = counterexample "CStack does not match:"

msgOutputNotMatch :: Property -> Property
msgOutputNotMatch = counterexample "Output does not match:"

isUnderflow :: a -> Property
isUnderflow = msgNoUnderflow . ioProperty . (fmap aux) . tryEvaluate
    where aux (Left e) = msgUnderflow `isPrefixOf` (show e)
          aux _ = False

newtype ArithOp = ArithOp (String, Integer -> Integer -> Integer)
instance Show ArithOp where
    show (ArithOp (s,_)) = "Given operator: " ++ show s
instance Arbitrary ArithOp where
    arbitrary = ArithOp <$> elements [("➕", (+)), ("➖", (-)),
                                      ("❌", (*)), ("➗", div)]

newtype CompOp = CompOp (String, Integer -> Integer -> Bool)
instance Show CompOp where
    show (CompOp (s,_)) = "Given operator: " ++ show s
instance Arbitrary CompOp where
    arbitrary = CompOp <$> elements [("☹️⚖️",  (<)),  ("😀⚖️",  (>)),
                                     ("☹️⚖️🤷", (<=)), ("😀⚖️🤷", (>=)),
                                     ("😠🤷⚖️", (/=)), ("🤷⚖️",  (==))]

newtype ArbIStack = ArbIStack IStack
instance Show ArbIStack where
    show (ArbIStack s) = "Given IStack (Top at left):\n" ++ show s
instance Arbitrary ArbIStack where
    arbitrary = ArbIStack <$> (arbitrary :: Gen IStack)

newtype GEQ2IStack = GEQ2IStack IStack
instance Show GEQ2IStack where
    show (GEQ2IStack s) = "Given IStack (Top at left):\n" ++ show s
instance Arbitrary GEQ2IStack where
    arbitrary = GEQ2IStack <$>
                    (arbitrary `suchThat` -- with >=2 nonzero elements
                         (\xs -> (length xs) >= 2 && all (/= 0) xs))

newtype ArbBooleanIStack = ArbBooleanIStack IStack
instance Show (ArbBooleanIStack) where
    show (ArbBooleanIStack s) = "Given IStack (Top at left):\n" ++ show s
instance Arbitrary ArbBooleanIStack where
    arbitrary = ArbBooleanIStack <$> -- Gen size+1 elements to avoid underflow
                (sized $ \n -> vectorOf (n+1) $ elements [-1, 0])

emptyForthState :: ForthState
emptyForthState = ([], initialDictionary, [])

testIntOps = map show [0..9]
testIStackOps = [ "➕", "➖", "❌" -- skip "➗" to avoid DivByZero
                , "☹️⚖️", "😀⚖️", "☹️⚖️🤷", "😀⚖️🤷", "🤷⚖️", "😠🤷⚖️"
                , "🪞", "🙃", "💦", "☀️"
                , "🖨️", "🖨️🎉"
                ]
newtype ArbSafePrimCode = ArbSafePrimCode [String]
instance Show ArbSafePrimCode where
    show (ArbSafePrimCode code) = "Given code:\n" ++ show code
instance Arbitrary ArbSafePrimCode where
    arbitrary = sized $ \n -> do
        num <- vectorOf (3*n) (elements testIntOps)
        ops <- vectorOf    n  (elements testIStackOps)
        return $ ArbSafePrimCode $ num ++ ops

data ITE = Nil | IfElseThen ITE ITE | IfThen ITE | Seq ITE ITE
instance Arbitrary ITE where
    arbitrary = sized aux
        where aux 0 = return Nil
              aux n = oneof [ return Nil
                            , IfElseThen <$> (aux $ n-1) <*> (aux $ n-1)
                            , IfThen <$> (aux $ n-1)
                            , Seq <$> (aux $ n `div` 2) <*> (aux $ n `div` 2)
                            ]

-- | Generated code only test `if ... else ... then` and primitives.
-- Note at the end of each branch, it will push back Boolean value
-- so that given an ArbBooleanIStack, the code won't change the stack.
newtype ArbStructuredITECode = ArbStructuredITECode [String]
instance Show (ArbStructuredITECode) where
    show (ArbStructuredITECode code) = "Given code:\n" ++ show code
instance Arbitrary ArbStructuredITECode where
    arbitrary = do
        ite <- arbitrary :: Gen ITE
        return $ ArbStructuredITECode $ aux ite
        where aux Nil = []
              aux (IfElseThen t e)
                 = ("🤔":(aux t)) ++ ("-1":"🙊":(aux e)) ++ ["0", "🤖"]
              aux (IfThen t) = ("🤔":(aux t)) ++ ["2", "🤖"]
                            ++  (words "🪞 2 🤷⚖️ 🪞 🤔 🙃 💦 🤖")
              aux (Seq i1 i2) = (aux i1) ++ (aux i2)

data Loop = LoopNil | LoopBegin Loop | LoopSeq Loop Loop
instance Arbitrary Loop where
    arbitrary = sized aux
        where aux 0 = return LoopNil
              aux n = oneof [ return LoopNil
                            , LoopBegin <$> (aux $ n-1)
                            , LoopSeq <$> (aux $ n `div` 2) <*> (aux $ n `div` 2)
                            ]

-- | Generated code only test `begin ... until` and primitives.
-- The constructed code is essentially dropping only top element.
newtype ArbStructuredLoopCode = ArbStructuredLoopCode [String]
instance Show (ArbStructuredLoopCode) where
    show (ArbStructuredLoopCode code) = "Given code:\n" ++ show code
instance Arbitrary ArbStructuredLoopCode where
    arbitrary = do
        loop <- arbitrary :: Gen Loop
        return $ ArbStructuredLoopCode $ aux loop
        where aux LoopNil = ["💦"]
              aux (LoopBegin t) = ("😍":"🪞":(aux t)) ++ (words "1 ➕ 🪞 😥 💦")
              aux (LoopSeq i1 i2) = (aux i1) ++ "🪞":(aux i2)


-- | Generate code that replace top two elements with the larger one
newtype MaxCode = MaxCode [String]
instance Show (MaxCode) where
    show (MaxCode code) = "Given code:\n" ++ unwords code
instance Arbitrary MaxCode where
    arbitrary = do
         m <- arbitrary :: Gen Int
         n <- arbitrary :: Gen Int
         return $ MaxCode $ (show n):(show m):(words defMaxCall)
defMaxCall
  = "✏️📖 max 🪞 ☀️ 🪞 ☀️ 😀⚖️ 🤔 🙃 🤖 💦 📕 max"

-- | Generate code that replace the top element with its factorial
newtype FactorialCode = FactorialCode [String]
instance Show (FactorialCode) where
    show (FactorialCode code) = "Given code:\n" ++ unwords code
instance Arbitrary FactorialCode where
    arbitrary = sized aux
        where aux n = return $ FactorialCode $ (show n):(words defFactCall)

defFactCall :: String
defFactCall
  = "✏️📖 fact \
     \ 🪞 1 ☹️⚖️🤷 🤔 \
         \ 💦 1 \
     \ 🙊 \
         \ 🪞 \
         \ 😍 \
             \ 1 ➖ 🪞 ☀️ ❌ 🙃 \
         \ 🪞 1 ☹️⚖️🤷 😥 \
         \ 💦 \
     \ 🤖 📕 fact"

--- Problems
--- ========

--- Lifters
--- -------

--- ### `liftCompOp`

prop_liftComp :: CompOp -> ArbIStack -> Property
prop_liftComp (CompOp (_,op)) (ArbIStack s@(x:y:xs))
    = case liftCompOp op s of
        Just (z:zs) ->
            counterexample "Comparison result is wrong:" ((z /= 0) === (op y x)) .&&.
            counterexample "Rest of IStack is modified:" (zs === xs)
        Just [] -> counterexample "Result IStack is empty:" $ property False
prop_liftComp (CompOp (_,op)) (ArbIStack s) =
    counterexample "Should be Nothing for stack underflow" (liftCompOp op s === Nothing)

--- The Dictionary
--- --------------

prop_dlookup_initArith :: ArithOp -> GEQ2IStack -> Property
prop_dlookup_initArith (ArithOp (str,op)) (GEQ2IStack s) =
    case dlookup str initArith of
       Prim f ->
           let (s',_,_) = f (s,initArith,[])
           in  Just s' === liftIntOp op s
       _ -> msgOperatorNotFound $ property False

prop_dlookup_initComp :: CompOp -> GEQ2IStack -> Property
prop_dlookup_initComp (CompOp (str,op)) (GEQ2IStack s@(x:y:xs)) =
    case dlookup str initComp of
        Prim f ->
            let (s',_,_) = f (s,initArith,[])
            in case s' of
                (z:zs) ->
                      counterexample "Comparison result is wrong:" ((z /= 0) === (op y x)) .&&.
                      counterexample "Rest of IStack is modified:" (zs === xs)
                [] -> counterexample "Result IStack is empty:" $ property False
        _ -> msgOperatorNotFound $ property False

--- ### Stack Manipulations

--- #### dup

prop_istack_dup :: ArbIStack -> Property
prop_istack_dup (ArbIStack istack@(i:_))
    = msgIStackNotMatch $ (istackDup istack) === (Just $ i:istack)
prop_istack_dup (ArbIStack istack)
    = msgIStackNotMatch $ (istackDup istack) === Nothing

--- #### swap

prop_istack_swap :: ArbIStack -> Property
prop_istack_swap (ArbIStack istack@(i2:i1:rest))
    = msgIStackNotMatch $ (istackSwap istack) === (Just $ i1:i2:rest)
prop_istack_swap (ArbIStack istack)
    = msgIStackNotMatch $ (istackSwap istack) === Nothing

--- #### drop

prop_istack_drop :: ArbIStack -> Property
prop_istack_drop (ArbIStack istack@(_:rest))
    = msgIStackNotMatch $ (istackDrop istack) === (Just rest)
prop_istack_drop (ArbIStack istack)
    = msgIStackNotMatch $ (istackDrop istack) === Nothing

--- #### rot

prop_istack_rot :: ArbIStack -> Property
prop_istack_rot (ArbIStack istack@(i3:i2:i1:rest))
    = msgIStackNotMatch $ (istackRot istack) === (Just $ i1:i3:i2:rest)
prop_istack_rot (ArbIStack istack)
    = msgIStackNotMatch $ (istackRot istack) === Nothing

--- ### Printing the Stack
prop_istack_printStack :: ArbIStack -> Property
prop_istack_printStack (ArbIStack istack)
    = let (istack', _, out') = printStack (istack, initialDictionary, [])
      in msgIStackNotMatch (istack' === istack) .&&.
         msgOutputNotMatch
            (out' === [(unwords . map show . reverse) istack])

--- Compiler
--- --------

--- ### Primitive Operators

prop_compile_prim :: ArbSafePrimCode -> Property
prop_compile_prim (ArbSafePrimCode code)
    = case compile (code ++ ["📕"]) initialDictionary [("", id)] of
        Right ([], k) -> let (istack' , _, _) = k emptyForthState
                             istack'' = foldl aux [] code
                         in msgIStackNotMatch $ istack' === istack''
        Left msg -> counterexample msg $ property False
      where aux stack word
                = case dlookup word initialDictionary of
                    Prim f -> stack'
                        where (stack',_,_) = f (stack, initialDictionary,[])
                    Num i -> i:stack

--- ### Conditionals

prop_compile_if :: ArbBooleanIStack -> ArbStructuredITECode -> Property
prop_compile_if (ArbBooleanIStack istack) (ArbStructuredITECode code)
    = case compile (code ++ ["📕"]) initialDictionary [("", id)] of
        Right ([], k) -> let (istack', _, _) = k (istack, initialDictionary, [])
                         in  msgIStackNotMatch $ istack' === istack
        Left msg -> counterexample msg $ property False

-- TODO test unstructured ITE

--- ### Indefinite Loops

prop_compile_loop :: ArbBooleanIStack -> ArbStructuredLoopCode -> Property
prop_compile_loop (ArbBooleanIStack istack) (ArbStructuredLoopCode code)
    = case compile (code ++ ["📕"]) initialDictionary [("", id)] of
        Right ([], k) -> case istack of
            [] -> isUnderflow $ k (istack, initialDictionary, [])
            (i:is) ->
                let (istack', _, _) = k (istack, initialDictionary, [])
                in msgIStackNotMatch $ istack' === is
        Left msg -> counterexample msg $ property False

-- TODO test unstructured Loop

--- The Evaluator
--- -------------

prop_it_evalEqComp :: ArbSafePrimCode -> Property
prop_it_evalEqComp (ArbSafePrimCode code)
    = case compile (code ++ ["📕"]) initialDictionary [("", id)] of
        Right ([], k) -> let (istack' , _, _) = k emptyForthState
                             (istack'', _, _) = eval code emptyForthState
                         in msgIStackNotMatch $ istack' === istack''
        Left msg -> counterexample msg $ property False

prop_it_max :: MaxCode -> Property
prop_it_max (MaxCode code@(n:m:_))
    = let (istack, _, _) = eval code emptyForthState
       in msgIStackNotMatch $ istack === [max (read m) (read n)]

prop_it_fact :: FactorialCode -> Property
prop_it_fact (FactorialCode code@(i:_))
    = let (istack, _, _) = eval code emptyForthState
       in msgIStackNotMatch $ istack === [fact (read i)]
          where fact x = if x <= 1 then 1 else x * (fact (x - 1))
