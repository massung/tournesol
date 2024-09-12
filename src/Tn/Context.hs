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
  show (SyntaxError err) = printf "%s on line %d" msg line
    where
      pos = errorPos err
      line = sourceLine pos
      msg = case unwords [e | (Message e) <- errorMessages err] of
        "" -> "syntax error"
        errmsg -> errmsg

instance Exception ContextError

type ResultT = ExceptT ContextError (State Context)

runWithContext :: ResultT Scalar -> Context -> Either ContextError Scalar
runWithContext it = evalState (runExceptT it)

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

buildConv :: [(Unit, Unit, Int)] -> ResultT (Maybe Conv)
buildConv m = do
  (Context gr _) <- get

  -- attempt to merge all unit conversions
  case sequence [findConv (from, e) to gr | (from, to, e) <- m] of
    Just (c : cs) -> return $ Just (foldl' (<>) c cs)
    _ -> return Nothing

convertUnits :: Scalar -> Units -> ResultT Scalar
convertUnits (Scalar x Nothing) uy = return $ Scalar x (Just uy)
convertUnits (Scalar x (Just ux)) uy =
  if not (ux ~= uy)
    then throwError DisparateUnits
    else
      let m = unitsToConv ux uy
       in if null m
            then return $ Scalar x (Just uy)
            else
              buildConv m >>= \case
                Just conv -> return $ Scalar (applyConv x conv) (Just uy)
                _ -> throwError NoConversion

convertSharedUnits :: Scalar -> Units -> ResultT Scalar
convertSharedUnits (Scalar x Nothing) uy = return $ Scalar x (Just uy)
convertSharedUnits s@(Scalar x (Just ux)) uy = do
  let m = unitsToConv ux uy
   in if null m
        then return s -- Scalar x (ux <> uy)
        else
          buildConv m >>= \case
            Just conv -> return $ Scalar (applyConv x conv) (Just $ convUnits m ux)
            _ -> throwError NoConversion
