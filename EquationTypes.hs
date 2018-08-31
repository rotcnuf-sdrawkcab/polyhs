module EquationTypes (
  Equation (Equation),
  QuadraticEquation (QuadraticEquation),
  Solution (NoSolutions, SingleSolution, MultipleSolutions, AnySolution),
  Term (Term),
  TNum (TDouble, TInt, TComplex),
  TSign (TPlus, TMinus),
  isNegativeTerm,
  isZeroTerm,
  getTermPower,
  getTermCoefficient,
  quadraticDiscriminant,
  quadraticCoefficients
) where

data Equation = Equation
  { lhs :: [Term]
  , rhs :: [Term]
  } deriving (Eq)

data Solution = NoSolutions
  | AnySolution
  | SingleSolution TNum
  | MultipleSolutions [TNum]

data QuadraticEquation = QuadraticEquation Term Term Term

data Term = Term TNum TNum deriving (Eq)
data TSign = TPlus | TMinus deriving (Show, Eq)
data TNum = TDouble Double | TInt Int | TComplex Double Double deriving (Eq)

instance Show (TNum) where
  show (TDouble d)    = show d
  show (TInt i)       = show i
  show (TComplex 0 0) = show 0
  show (TComplex 0 1) = "i"
  show (TComplex r 0) = (show r)
  show (TComplex 0 i) = (show i) ++ "i"
  show (TComplex r 1) = (show r) ++ " + i"
  show (TComplex r (-1))      = (show r) ++ " - i"
  show (TComplex r i) | i < 0 = (show r) ++ " - " ++ (show.negate $ i) ++ "i"
  show (TComplex r i)         = (show r) ++ " + " ++ (show i) ++ "i"

instance Show (Term) where
  show (Term (TInt 0) _)      = "0"
  show (Term (TDouble 0.0) _) = "0"
  show (Term x (TInt 0))      = show x
  show (Term x (TDouble 0))   = show x
  show (Term (TInt 1) (TInt 1))       = "x"
  show (Term (TDouble 1) (TInt 1))    = "x"
  show (Term (TInt 1) (TDouble 1))    = "x"
  show (Term (TDouble 1) (TDouble 1)) = "x"
  show (Term (TInt (-1)) (TInt 1))       = "-x"
  show (Term (TDouble (-1)) (TInt 1))    = "-x"
  show (Term (TInt (-1)) (TDouble 1))    = "-x"
  show (Term (TDouble (-1)) (TDouble 1)) = "-x"
  show (Term x (TInt 1))      = (show x) ++ "*x"
  show (Term x (TDouble 1.0)) = (show x) ++ "*x"
  show (Term (TInt 1) y)      = "x^" ++ (show y)
  show (Term (TDouble 1.0) y) = "x^" ++ (show y)
  show (Term (TInt (-1)) y)      = "-x^" ++ (show y)
  show (Term (TDouble (-1.0)) y) = "-x^" ++ (show y)
  show (Term x y)             = (show x) ++ "*x^" ++ (show y)

instance Num (TNum) where
  (TInt x) + (TInt y)       = TInt (x + y)
  (TInt x) + (TDouble y)    = TDouble ((fromIntegral x) + y)
  (TDouble x) + (TInt y)    = TDouble (x + (fromIntegral y))
  (TDouble x) + (TDouble y) = TDouble (x + y)
  (TInt x) - (TInt y)       = TInt (x - y)
  (TInt x) - (TDouble y)    = TDouble ((fromIntegral x) - y)
  (TDouble x) - (TInt y)    = TDouble (x - (fromIntegral y))
  (TDouble x) - (TDouble y) = TDouble (x - y)
  (TInt x) * (TInt y)       = TInt (x * y)
  (TInt x) * (TDouble y)    = TDouble ((fromIntegral x) * y)
  (TDouble x) * (TInt y)    = TDouble (x * (fromIntegral y))
  (TDouble x) * (TDouble y) = TDouble (x * y)
  negate (TInt x)     = TInt (negate x)
  negate (TDouble x)  = TDouble (negate x)
  abs (TInt x)        = TInt (abs x)
  abs (TDouble x)     = TDouble (abs x)
  signum (TInt x)     = TInt (signum x)
  signum (TDouble x)  = TDouble (signum x)
  fromInteger x       = TInt (fromIntegral x)

instance Num (Term) where
  (Term x y) + (Term x1 y1) | y == y1 = (Term (x + x1) y)
  (Term x y) + (Term x1 y1) = error "Cannot add terms with different powers."
  (Term x y) - (Term x1 y1) | y == y1 = (Term (x - x1) y)
  (Term x y) - (Term x1 y1) = error "Cannot subtract terms with different powers."
  (Term x y) * (Term x1 y1) = (Term (x * x1) (y + y1))
  negate (Term x y) = (Term (negate x) y)
  abs (Term x y)    = (Term (abs x) y)
  signum (Term x y) = (Term (signum x) y)
  fromInteger x     = (Term (fromIntegral x) (TInt 0))

instance Show (Equation) where
  show (Equation lhs rhs) = (termListToString lhs) ++ " = " ++ (termListToString rhs)

instance Show (QuadraticEquation) where
  show (QuadraticEquation a b c) = (termListToString [a, b, c]) ++ " = 0"

termListToString :: [Term] -> String
termListToString []         = ""
termListToString (x:xs:xss) = (show x) ++ " + " ++ (termListToString (xs:xss))
termListToString (x:[])     = show x

isNegativeTerm :: Term -> Bool
isNegativeTerm (Term (TInt x) _)    = x < 0
isNegativeTerm (Term (TDouble x) _) = x < 0.0

isZeroTerm :: Term -> Bool
isZeroTerm (Term (TInt 0) _) = True
isZeroTerm (Term (TDouble 0.0) _) = True
isZeroTerm _ = False

getTermPower :: Term -> Int
getTermPower (Term _ (TInt x)) = x

getTermCoefficient :: Term -> Double
getTermCoefficient (Term (TInt x) _) = fromIntegral x
getTermCoefficient (Term (TDouble x) _) = x

tnumToDouble :: TNum -> Double
tnumToDouble (TDouble x) = x
tnumToDouble (TInt x)    = fromIntegral x

quadraticDiscriminant :: QuadraticEquation -> Double
quadraticDiscriminant (QuadraticEquation (Term a _) (Term b _) (Term c _)) = tnumToDouble $ (b * b) - 4 * a * c

quadraticCoefficients :: QuadraticEquation -> (Double, Double, Double)
quadraticCoefficients (QuadraticEquation a b c) = (
  getTermCoefficient a,
  getTermCoefficient b,
  getTermCoefficient c)
