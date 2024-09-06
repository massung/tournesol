module Tn.Ops where

import Tn.Conv
import Tn.Dims
import Tn.Scalar
import Tn.Scope
import Tn.Unit

type OpResultT = ExceptT String (State Scope)

runWithScope :: Scope -> OpResultT Scalar -> Either String Scalar
runWithScope scope op = evalState (runExceptT op) scope

unitsConv :: [(Unit, Unit, Int)] -> ConvGraph -> Maybe Conv
unitsConv unitMap gr =
  case sequence [findConv (from, e) to gr | (from, to, e) <- unitMap] of
    Just (c : cs) -> Just $ foldl' (<>) c cs
    _ -> Nothing

convertUnits :: Scalar -> Units -> OpResultT Scalar
convertUnits (Scalar x Nothing) uy = return $ Scalar x (Just uy)
convertUnits (Scalar x (Just ux)) uy =
  if ux ~= uy
    then performConvertion
    else throwError "disparate units"
  where
    convMap = unitsToConv ux uy
    performConvertion = do
      gr <- get <&> _convs

      -- no conversions needed then the base units match (eg, W s == J)
      if null convMap
        then return $ Scalar x (Just uy)
        else case unitsConv convMap gr of
          Just conv -> return $ Scalar (applyConv x conv) (Just uy)
          _ -> throwError "no conversion"

convertSharedUnits :: Scalar -> Units -> OpResultT Scalar
convertSharedUnits (Scalar x Nothing) uy = return $ Scalar x (Just uy)
convertSharedUnits s@(Scalar x (Just ux)) uy = do
  gr <- get <&> _convs

  -- determine which units are of the same dimensions
  let m = unitsToConv ux uy

  -- combine all the conversions together and apply them
  if null m
    then return s
    else case unitsConv m gr of
      Just conv -> return $ Scalar (applyConv x conv) (Just $ convUnits m ux)
      _ -> throwError "no conversion"

(+%) :: Scalar -> Scalar -> OpResultT Scalar
(+%) x@(Scalar _ Nothing) y = return $ x + y
(+%) x y@(Scalar _ Nothing) = return $ x + y
(+%) x@(Scalar _ (Just ux)) y = convertUnits y ux <&> (x +)

(*%) :: Scalar -> Scalar -> OpResultT Scalar
(*%) x@(Scalar _ Nothing) y = return $ x * y
(*%) x y@(Scalar _ Nothing) = return $ x * y
(*%) x@(Scalar _ (Just ux)) y = convertSharedUnits y ux <&> (x *)

(-%) :: Scalar -> Scalar -> OpResultT Scalar
(-%) x y = x +% negate y

(/%) :: Scalar -> Scalar -> OpResultT Scalar
(/%) x y = x *% recip y

(^%) :: Scalar -> Scalar -> OpResultT Scalar
(^%) (Scalar x ux) (Scalar y uy) =
  if isNothing uy && isJust uy
    then return $ Scalar (x ^^ n) (fmap (*^ n) ux)
    else throwError "invalid exponent"
  where
    n = fromInteger $ numerator y

(<=>%) :: Scalar -> Scalar -> OpResultT Scalar
(<=>%) x y = x -% y <&> signum

(==%) :: Scalar -> Scalar -> OpResultT Scalar
(==%) x y = x <=>% y <&> fromIntegral . fromEnum . (== 0)

(/=%) :: Scalar -> Scalar -> OpResultT Scalar
(/=%) x y = x <=>% y <&> fromIntegral . fromEnum . (/= 0)

(<=%) :: Scalar -> Scalar -> OpResultT Scalar
(<=%) x y = x <=>% y <&> fromIntegral . fromEnum . (<= 0)

(<%) :: Scalar -> Scalar -> OpResultT Scalar
(<%) x y = x <=>% y <&> fromIntegral . fromEnum . (< 0)

(>=%) :: Scalar -> Scalar -> OpResultT Scalar
(>=%) x y = x <=>% y <&> fromIntegral . fromEnum . (>= 0)

(>%) :: Scalar -> Scalar -> OpResultT Scalar
(>%) x y = x <=>% y <&> fromIntegral . fromEnum . (> 0)
