module Tn.Symbol
  ( Symbol,
    intern,
    unintern,
  )
where

import qualified Data.Map.Strict as M

-- interned symbol
data Symbol = Symbol !Int !String

instance Eq Symbol where
  (==) (Symbol a _) (Symbol b _) = a == b

instance Ord Symbol where
  (<=) (Symbol _ a) (Symbol _ b) = a <= b

instance Hashable Symbol where
  hashWithSalt salt (Symbol i _) = hashWithSalt salt i
  hash (Symbol i _) = hash i

instance IsString Symbol where
  fromString = intern

instance Show Symbol where
  show = unintern

data SymbolTable = SymbolTable !Int !(Map String Int)

-- all calls to this need to use the same thunk, so cannot be inlined!
symbolTable :: MVar SymbolTable
symbolTable = unsafePerformIO $ do
  newMVar $ SymbolTable 1 mempty
{-# NOINLINE symbolTable #-}

intern :: String -> Symbol
intern s = s `deepseq` unsafePerformIO $ do
  modifyMVar symbolTable $ \(SymbolTable next m) ->
    let m' = M.union m $ M.singleton s next
        i = fromJust $ M.lookup s m'
     in return (SymbolTable (next + 1) m', Symbol i s)
{-# NOINLINE intern #-}

unintern :: Symbol -> String
unintern (Symbol _ s) = s
