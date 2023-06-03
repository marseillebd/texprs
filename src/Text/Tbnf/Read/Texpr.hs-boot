module Text.Tbnf.Read.Texpr
  ( runReader
  , Input(..)
  , ReaderError(..)
  )
  where

import Data.Texpr (Texprs)
import Text.Location (Position)
import Text.Tbnf.Read.Monad (ReaderError(..),Stream)
import Text.Tbnf.Tree (CompiledTbnf)

-- | Create a stream of 'Data.Texpr.Texpr's from an input 'Data.Texpr.Texpr'
-- stream that matches the given grammar, or report an error.
runReader ::
     CompiledTbnf
  -> Texprs -- ^ input
  -> Either ReaderError (Texprs, Input) -- ^ result with remaining input

-- | The type of input that should be fed to this reader.
data Input = Input
  { loc :: {-# UNPACK #-} !Position -- ^ last position seen before the next token
  , toks :: Texprs -- ^ the token stream
  }

instance Stream Input where
