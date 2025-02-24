module Tn.Ops where

import Tn.Context
import Tn.Dims
import Tn.Scalar
import Tn.Unit

(+%) :: Scalar -> Scalar -> ResultT Scalar
(+%) x@(Scalar _ Nothing) y = return $ x + y
(+%) x y@(Scalar _ Nothing) = return $ x + y
(+%) x@(Scalar _ (Just ux)) y = convertToUnits y ux <&> (x +)

(*%) :: Scalar -> Scalar -> ResultT Scalar
(*%) x@(Scalar _ Nothing) y = return $ x * y
(*%) x y@(Scalar _ Nothing) = return $ x * y
(*%) x@(Scalar _ (Just ux)) y = harmonizeUnits y ux <&> (x *)

-- convertSharedUnits y ux <&> (x *)

(-%) :: Scalar -> Scalar -> ResultT Scalar
(-%) x y = x +% negate y

(/%) :: Scalar -> Scalar -> ResultT Scalar
(/%) _ 0 = throwError DivByZero
(/%) x y = x *% recip y

(^%) :: Scalar -> Scalar -> ResultT Scalar
(^%) _ 0 = return 1
(^%) x 1 = return x
(^%) _ (Scalar _ (Just _)) = throwError InvalidExponent
(^%) (Scalar x ux) (Scalar y _) = case properFraction y of
  (n, 0) -> return $ Scalar (x ^^ n) (fmap (*^ n) ux)
  _ -> throwError InvalidExponent

(<=>%) :: Scalar -> Scalar -> ResultT Scalar
(<=>%) x y = x -% y <&> signum

(==%) :: Scalar -> Scalar -> ResultT Scalar
(==%) x y = x <=>% y <&> fromIntegral . fromEnum . (== 0)

(~=%) :: Scalar -> Scalar -> ResultT Scalar
(~=%) x y = x -% y <&> fromIntegral . fromEnum . (< 1e-8) . abs

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

sqrtScalar :: Scalar -> ResultT Scalar
sqrtScalar (Scalar x u) = mapM root u <&> Scalar (sqrt x)
  where
    root :: Units -> ResultT Units
    root u'@(Dims m) =
      if all ((== 0) . (.&. 1)) m
        then return $ mapDims (`div` 2) u'
        else throwError InvalidExponent
