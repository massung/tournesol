{-# LANGUAGE TemplateHaskell #-}

module Tn.Default where

import Data.FileEmbed
import Tn.Parser
import Tn.Script

-- defaultScript :: IO Script
-- defaultScript = foldM parseScript mempty [units]
--   where
--     units = ("units.tn", $(embedStringFile "scripts/default/units.tn"))
