{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where

    O == O =  True
    (S m) == (S n) = m == n
    _ == _ = False

instance Ord Nat where

    O <= _ = True
    _ <= O = False
    (S m) <= (S n) = m <= n

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min O _ = O
    min _ O = O
    min (S m) (S n) = S (min m n)

    max O n = n
    max m O = m
    max (S m) (S n) = S (max m n)



----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero (S _)= False


-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n

even :: Nat -> Bool
even O = True
even (S O) = False
even (S (S n)) = even n

odd :: Nat -> Bool
odd O = False
odd (S O) = True
odd (S (S n)) = odd n


----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
O <+> n = n
(S m) <+> n = S(m <+> n)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus O _ = O
monus n O = n
monus (S m) (S n) = monus m n

(-*) :: Nat -> Nat -> Nat
(-*) = monus

-- multiplication
times :: Nat -> Nat -> Nat
times O _ = O
times (S m) n = n <+> (times m n)

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow _ O = S O
pow m (S n) = m <*> (pow m n)

exp :: Nat -> Nat -> Nat
exp = pow

(<^>) :: Nat -> Nat -> Nat
(<^>) = pow

-- quotient
(</>) :: Nat -> Nat -> Nat
_ </> O = error "divisao por zero"
m </> n
    | m < n = O
    | otherwise = S ((m -* n) -* n)
    

-- remainder
(<%>) :: Nat -> Nat -> Nat
_ <%> O = error "divisao por zero"
m <%> n 
    | m < n = m
    | otherwise = (m -* n) <%> n

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (_, O) = error "Divisao por zero"
eucdiv (m, n)
    | m < n = (O, m)
    | otherwise = (S q, r)
  where
    (q, r) = eucdiv (m -* n, n)

-- divides
(<|>) :: Nat -> Nat -> Bool
n <|> m = isZero (m <%> n)

divides = (<|>)


-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist x y = (x -* y) <+> (y -* x)

(|-|) = dist

factorial :: Nat -> Nat
factorial O = S O 
factorial (S n) = (S n) <*> (factorial n)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg (S _) = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O _ = error "Log base 0"
lo (S O) a 
  | a == O = error "Log de zero"
  | otherwise = O
lo b a
    | a < b = O
    | otherwise = S (lo b (a </> b))

----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat n
    | n < 0     = error "nao da pra converter nergativo pra nat"
    | n == 0    = O
    | otherwise = S (toNat (n - 1))

fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat (S n) = 1 + fromNat n


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (-*)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = undefined
      | x == 0    = undefined
      | otherwise = undefined

