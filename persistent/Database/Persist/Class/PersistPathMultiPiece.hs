module Database.Persist.Class.PersistPathMultiPiece
    ( PersistPathMultiPiece (..)
    ) where

import Data.Text (Text)
import Database.Persist.Class.PersistEntity (PersistEntity (..), Key)
import Database.Persist.PersistValue (PersistValue (PersistList))
import Web.PathPieces (PathMultiPiece (..), PathPiece (..))

class PersistEntity a => PersistPathMultiPiece a where
    keyFromPieces :: [Text] -> Maybe (Key a)
    keyFromPieces pieces = do
        Right key <- keyFromValues <$> mapM fromPathPiece pieces
        pure key
    keyToPieces :: Key a -> [Text]
    keyToPieces = map toPathPiece . keyToValues

instance PersistPathMultiPiece a => PathMultiPiece (Key a) where
    fromPathMultiPiece = keyFromPieces
    toPathMultiPiece = keyToPieces
