module EquationHelpers
  ( equateToZero
  , hasDoublePowers
  , sumAllTermsWithPowerOf
  , equationToQuadratic
  , maxBetweenIntAndTermPower
  , equationDegree
  , reduceEquation
  ) where

import EquationTypes

equateToZero :: Equation -> Equation
equateToZero (Equation x []) = (Equation x [0])
equateToZero (Equation x (y:ys)) | isZeroTerm y = equateToZero (Equation x ys)
equateToZero (Equation x (y:ys)) = equateToZero (Equation (x ++ ([negate y])) ys)

hasPowersGreaterThan :: Equation -> Double -> Bool
hasPowersGreaterThan (Equation lhs rhs) power = hasPowersGT power lhs || hasPowersGT power lhs
  where hasPowersGT power = any ((> power).fromIntegral.getTermPower)

hasDoublePower :: Term -> Bool
hasDoublePower (Term _ (TDouble _)) = True
hasDoublePower _ = False

-- Hack for ease of use.
hasDoublePowers :: Equation -> Bool
hasDoublePowers (Equation lhs rhs) = any hasDoublePower lhs || any hasDoublePower rhs

sumAllTermsWithPowerOf :: Int -> [Term] -> Term
sumAllTermsWithPowerOf power terms = foldl (+) (Term (TInt 0) (TInt power)) $ filteredTermsOfPower
  where filteredTermsOfPower = filter ((== power).getTermPower) terms

equationToQuadratic :: Equation -> QuadraticEquation
equationToQuadratic (Equation lhs [0]) = QuadraticEquation
  (sumAllTermsWithPowerOf 2 lhs)
  (sumAllTermsWithPowerOf 1 lhs)
  (sumAllTermsWithPowerOf 0 lhs)
equationToQuadratic eq = equationToQuadratic $ equateToZero eq

--- Just a hack before we have actual Double exponents
maxBetweenIntAndTermPower :: Int -> Term -> Int
maxBetweenIntAndTermPower x (Term _ (TInt y)) = max x y

equationDegree :: Equation -> Int
equationDegree (Equation lhs rhs) = foldl maxBetweenIntAndTermPower 0 nonZeroTerms
  where nonZeroTerms = filter ((/= 0).getTermCoefficient) (lhs ++ rhs)

reduceEquation :: Equation -> Equation
reduceEquation eq = reduceEquation' $ equateToZero eq
  where reduceEquation' (Equation lhs rhs) = Equation (reduceSide lhs) ([0])
        degree = equationDegree eq
        reduceSide side = reverse $ if null (nonZeroTerms side) then [0] else nonZeroTerms side
        reduceSide' side = map ((flip sumAllTermsWithPowerOf) side) [0 .. degree]
        nonZeroTerms termList = filter ((/= 0).getTermCoefficient) (reduceSide' termList)


