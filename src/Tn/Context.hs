{-# LANGUAGE LambdaCase #-}

module Tn.Context where

import Safe
import Text.Parsec hiding (State)
import Text.Parsec.Error
import Tn.Conv
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

convertToUnits :: Scalar -> Units -> ResultT Scalar
convertToUnits (Scalar x Nothing) uy = return $ Scalar x (Just uy)
convertToUnits (Scalar x (Just ux)) uy =
  if baseDims ux /= baseDims uy
    then throwError DisparateUnits
    else
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

convertSharedUnits :: Scalar -> Units -> ResultT Scalar
convertSharedUnits (Scalar x Nothing) uy = return $ Scalar x (Just uy)
convertSharedUnits s@(Scalar x (Just ux)) uy = do
  let toConv = unitsToConv ux uy
   in if null toConv
        then return s
        else
          buildConv toConv >>= \case
            Just conv -> return $ Scalar (applyConv x conv) (Just $ convUnits toConv ux)
            _ -> throw NoConversion
