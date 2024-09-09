module Tn.Ops where

import Tn.Context
import Tn.Dims
import Tn.Scalar

(+%) :: Scalar -> Scalar -> ResultT Scalar
(+%) x@(Scalar _ Nothing) y = return $ x + y
(+%) x y@(Scalar _ Nothing) = return $ x + y
(+%) x@(Scalar _ (Just ux)) y = convertUnits y ux <&> (x +)

(*%) :: Scalar -> Scalar -> ResultT Scalar
(*%) x@(Scalar _ Nothing) y = return $ x * y
(*%) x y@(Scalar _ Nothing) = return $ x * y
(*%) x@(Scalar _ (Just ux)) y = convertSharedUnits y ux <&> (x *)

(-%) :: Scalar -> Scalar -> ResultT Scalar
(-%) x y = x +% negate y

(/%) :: Scalar -> Scalar -> ResultT Scalar
(/%) _ 0 = throwError DivByZero
(/%) x y = x *% recip y

(^%) :: Scalar -> Scalar -> ResultT Scalar
(^%) _ 0 = return 1
(^%) x 1 = return x
(^%) _ (Scalar _ (Just _)) = throwError InvalidExponent
(^%) (Scalar x ux) (Scalar y _) =
  if denominator y /= 1
    then throwError InvalidExponent
    else
      let n = fromInteger $ numerator y
       in return $ Scalar (x ^^ n) (fmap (*^ n) ux)

(<=>%) :: Scalar -> Scalar -> ResultT Scalar
(<=>%) x y = x -% y <&> signum

(==%) :: Scalar -> Scalar -> ResultT Scalar
(==%) x y = x <=>% y <&> fromIntegral . fromEnum . (== 0)

(/=%) :: Scalar -> Scalar -> ResultT Scalar
(/=%) x y = x <=>% y <&> fromIntegral . fromEnum . (/= 0)

(<=%) :: Scalar -> Scalar -> ResultT Scalar
(<=%) x y = x <=>% y <&> fromIntegral . fromEnum . (<= 0)

(<%) :: Scalar -> Scalar -> ResultT Scalar
(<%) x y = x <=>% y <&> fromIntegral . fromEnum . (< 0)

(>=%) :: Scalar -> Scalar -> ResultT Scalar
(>=%) x y = x <=>% y <&> fromIntegral . fromEnum . (>= 0)

(>%) :: Scalar -> Scalar -> ResultT Scalar
(>%) x y = x <=>% y <&> fromIntegral . fromEnum . (> 0)
