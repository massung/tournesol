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
  show InvalidExponent = "invalid exponent (non-natural or has units)"
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
--   1. Convert to base units
--   2. Convert between base units using the conversion graph
--   3. Convert from base units to the desired units

buildConv :: [(Unit, Unit, Int)] -> ResultT (Maybe Conv)
buildConv m = do
  (Context gr _) <- get

  -- attempt to merge all unit conversions
  case sequence [findConv (from, e) to gr | (from, to, e) <- m] of
    Just (c : cs) -> return $ Just (foldl' (<>) c cs)
    _ -> return Nothing

harmonizeUnits :: Scalar -> ResultT Scalar
harmonizeUnits s@(Scalar _ Nothing) = return s
harmonizeUnits s@(Scalar x (Just u)) = do
  case uncurry unitsToConv $ swap (partitionDims u) of
    [] -> return s
    convs ->
      buildConv convs >>= \case
        Just conv -> return $ Scalar (applyConv x conv) (Just $ convUnits convs u)
        _ -> throw NoConversion

convertPartitionedUnits :: Rational -> Units -> Units -> ResultT Scalar
convertPartitionedUnits x ux uy =
  let (rx, ux') = baseUnits ux
      (ry, uy') = baseUnits uy

      -- find the conversion from ux' -> uy'
      toConv = unitsToConv ux' uy'
   in if null toConv
        then return $ Scalar (x * rx / ry) (Just uy)
        else
          buildConv toConv >>= \case
            Just conv -> return $ Scalar (applyConv (x * rx) conv / ry) (Just uy)
            _ -> throwError NoConversion

convertToUnits :: Scalar -> Units -> ResultT Scalar
convertToUnits (Scalar x Nothing) uy = return $ Scalar x (Just uy)
convertToUnits (Scalar x (Just ux)) uy =
  if baseDims ux /= baseDims uy
    then throwError DisparateUnits
    else
      let (nx, dx) = partitionDims ux
          (ny, dy) = partitionDims uy
       in do
            -- perform the conversion of the numerator then denominator
            (Scalar x' n') <- convertPartitionedUnits x nx ny
            (Scalar x'' d') <- convertPartitionedUnits x' dx dy

            -- join the resulting units together
            return $ Scalar x'' (n' <> d')

-- convertSharedUnits :: Scalar -> Units -> ResultT Scalar
-- convertSharedUnits (Scalar x Nothing) uy = return $ Scalar x (Just uy)
-- convertSharedUnits (Scalar x (Just ux)) uy = do
--   x@(Scalar _ ux') <- harmonizeUnits (Scalar x (Just $ ux <> uy))

--   -- convert numerator and denominator
--   let (nx, dx) = partitionDims $ fromJust ux'
--       (ny, dy) = partitionDims uy

--       -- get shared units to convert for both numerator and denominator
--       nConv = unitsToConv nx ny
--       dConv = unitsToConv dx dy
--    in doConv nConv x >>= doConv dConv
--   where
--     doConv :: [(Unit, Unit, Int)] -> Scalar -> ResultT Scalar
--     doConv [] x = return x
--     doConv convs (Scalar x u) =
--       buildConv convs >>= \case
--         Just conv -> return $ Scalar (applyConv x conv) (convUnits convs <$> u)
--         _ -> throw NoConversion
