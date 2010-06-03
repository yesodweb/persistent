{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
module Yesod.Contrib.Formable where

import Text.Formlets
import Text.Hamlet
import Text.Hamlet.Monad (hamletToText, htmlContentToText)
import Data.Functor.Identity
import qualified Data.Text as T
import Data.Text.Lazy (toChunks)
import Data.Maybe (isJust, fromJust)
import Data.Time (Day)
import Control.Applicative
import Control.Applicative.Error
import Data.Monoid

-- orphans
instance Monad m => Monoid (Hamlet url m ()) where
    mempty = return ()
    mappend = (>>)

class Formable a where
    formable :: (Functor m, Applicative m, Monad m)
             => Formlet (Hamlet url IO ()) m a

class Fieldable a where
    fieldable :: (Functor m, Applicative m, Monad m)
              => String -> Formlet (Hamlet url IO ()) m a

hamletToHtml :: Hamlet a Identity () -> HtmlContent
hamletToHtml =
    Encoded . T.concat . toChunks . runIdentity . hamletToText undefined

pack' :: String -> HtmlContent
pack' = Unencoded . T.pack

repack :: HtmlContent -> HtmlContent
repack = Encoded . htmlContentToText

instance Fieldable [Char] where
    fieldable label = input' go
      where
        go name val = [$hamlet|
%tr
    %th $pack'.label$
    %td
        %input!type=text!name=$pack'.name$!value=$pack'.val$
|]

instance Fieldable HtmlContent where
    fieldable label =
        fmap (Encoded . T.pack)
      . input' go
      . fmap (T.unpack . htmlContentToText)
      where
        go name val = [$hamlet|
%tr
    %th $pack'.label$
    %td
        %textarea!name=$pack'.name$
            $pack'.val$
|]

instance Fieldable Day where
    fieldable label x = input' go (fmap show x) `check` asDay
      where
        go name val = [$hamlet|
%tr
    %th $pack'.label$
    %td
        %input!type=date!name=$pack'.name$!value=$pack'.val$
|]
        asDay s = maybeRead' s "Invalid day"
