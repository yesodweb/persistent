{-# LANGUAGE DeriveLift #-}

module Database.Persist.Types.SourceSpan (SourceSpan (..)) where

import Data.Text (Text)
import Language.Haskell.TH.Syntax (Lift)

-- | A pair of (start line/col, end line/col) coordinates. The end column will
-- be one past the final character (i.e. the span (1,1)->(1,1) is zero
-- characters long).
--
-- SourceSpans are 1-indexed in both lines and columns.
--
-- Conceptually identical to GHC's @RealSourceSpan@.
--
-- @since 2.16.0.0
data SourceSpan = SourceSpan
    { spanFile :: !Text
    , spanStartLine :: !Int
    , spanStartCol :: !Int
    , spanEndLine :: !Int
    , spanEndCol :: !Int
    }
    deriving (Show, Eq, Read, Ord, Lift)
