{-# LANGUAGE LambdaCase #-}

module Tn.Context
  ( Context (..),
    ContextError (..),
    ResultT,
    runWithContext,
    runWithLocals,
    push,
    getLocal,
    shiftLocal,
    harmonizeUnits,
    convertToUnits,
  )
where

import Safe
import Text.Parsec hiding (State)
import Text.Parsec.Error
import Tn.Conv
import Tn.Dims
import Tn.Scalar
import Tn.Unit

data Context = Context ConvGraph [Scalar]

data ContextError
  = ArityMismatch
  | DisparateUnits
  | DivByZero
  | InvalidExponent
  | NoConversion
  | TypeMismatch
  | SyntaxError ParseError
  deriving (Eq)

instance Show ContextError where
  show ArityMismatch = "arity mismatch"
  show DisparateUnits = "disparate units"
  show DivByZero = "divide by zero"
  show InvalidExponent = "invalid exponent"
  show NoConversion = "no conversion found"
  show TypeMismatch = "type mismatch"
  show (SyntaxError err) =
    case unwords [e | (Message e) <- errorMessages err] of
      "" -> "syntax error"
      errmsg ->
        if null file
          then errmsg
          else printf "%s on line %d of %s" errmsg line file
    where
      pos = errorPos err
      file = sourceName pos
      line = sourceLine pos

instance Exception ContextError

type ResultT = ExceptT ContextError (State Context)

runWithContext :: ResultT a -> Context -> Either ContextError a
runWithContext it = evalState (runExceptT it)

runWithLocals :: ResultT a -> [Scalar] -> ResultT a
runWithLocals it locals = do
  (Context gr _) <- get

  -- execute within a new context with different locals
  either throwError return $ runWithContext it (Context gr locals)

push :: [Scalar] -> Context -> Context
push locals (Context gr _) = Context gr locals

getLocal :: Int -> ResultT Scalar
getLocal i = do
  (Context _ locals) <- get

  -- default to 0 if no local exists
  return $ atDef 0 locals i

shiftLocal :: ResultT Scalar
shiftLocal = gets arg >>= \(x, st) -> do put st; return x
  where
    arg (Context gr []) = (0, Context gr [])
    arg (Context gr (x : xs)) = (x, Context gr xs)

-- Converting units uses the following algorithm:
--
--   1. Partition the units in the numerator and denominator
--   2. Harmonize numerator units (e.g "ft min" -> "in min")
--   3. Harmonize denominator units
--   4. Multiply original scalar (without units) with harmonized scalars
--
-- For multiplication and division, the process is done. For addition,
-- subtraction, and pure unit conversion, we need to ensure that the
-- units are identical for both sides of the equation.
--
-- The numerator and denominator are harmonized separately, otherwise
-- some units are impossible to represent (e.g., PSI).

buildConv :: [(Unit, Unit, Int)] -> ResultT (Maybe Conv)
buildConv m = do
  (Context gr _) <- get

  -- attempt to merge all unit conversions
  case sequence [findConv (from, e) to gr | (from, to, e) <- m] of
    Just (c : cs) -> return $ Just (foldl' (<>) c cs)
    _ -> return Nothing

convertPartitionedUnits :: Units -> Units -> ResultT Scalar
convertPartitionedUnits from to = do
  let (rx, from') = baseUnits from
  let (ry, to') = baseUnits to

  case unitsToConv from' to' of
    [] -> return $ Scalar (rx / ry) (Just to)
    convs ->
      buildConv convs >>= \case
        Just conv -> return $ Scalar (applyConv rx conv / ry) (Just to)
        _ -> throwError NoConversion

harmonizeUnits :: Scalar -> Units -> ResultT Scalar
harmonizeUnits x@(Scalar _ Nothing) _ = return x
harmonizeUnits x@(Scalar _ (Just ux)) uy = do
  let (nx, dx) = partitionDims ux
  let (ny, dy) = partitionDims uy

  -- harmonize the units of the numerator and denominator
  nx' <- convertPartitionedUnits nx ny
  dx' <- convertPartitionedUnits dx dy

  -- join the results together
  return $ dropUnits x * (nx' * dx')

convertToUnits :: Scalar -> Units -> ResultT Scalar
convertToUnits (Scalar x Nothing) uy = return $ Scalar x (Just uy)
convertToUnits x@(Scalar _ _) uy = do
  x' <- harmonizeUnits x uy

  -- ensure the desired units exactly match
  if scalarUnits x' == Just uy
    then return x'
    else throwError DisparateUnits
