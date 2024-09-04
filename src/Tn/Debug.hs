{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Tn.Debug
  ( module Tn.Builtins,
    module Tn.Conv,
    module Tn.Dims,
    module Tn.Ops,
    module Tn.Scalar,
    module Tn.Scope,
    module Tn.Unit
  ) where

import Tn.Builtins
import Tn.Conv
import Tn.Dims
import Tn.Ops
import Tn.Scalar
import Tn.Scope
import Tn.Unit
import Text.Parsec
import Tn.Parser

instance IsString Scalar where
  fromString s = case runParser scalarParser defaultScope "" s of
    Left _ -> invalidScalar
    Right x -> x
