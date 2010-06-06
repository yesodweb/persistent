{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
module Yesod.Contrib.Crud where

import Yesod hiding (Form)
import Database.Persist
import Control.Applicative.Error
import Yesod.Contrib.Formable
import Yesod.Contrib.Persist
import Text.Formlets
import Control.Arrow (second)
import Data.Monoid (mempty)

data Crud = Crud
    { crudCreate :: forall a. Bool -> GHandler Crud a RepHtml
    , crudEdit   :: forall a. Bool -> String -> GHandler Crud a RepHtml
    , crudDelete :: forall a. Bool -> String -> GHandler Crud a RepHtml
    }

{-
mkCrud' :: ( Persist a (YesodDB m (GHandler Crud m))
           , YesodPersist m
           , Yesod m
           , Formable a
           )
        => t
        -> Crud' m
mkCrud' _showObj = Crud $ Crud'
    { crudCreate' = crudHelper "Create" (Nothing :: Maybe (Key a, a)) (undefined :: m)
    , crudEdit' = undefined
    , crudDelete' = undefined
    }
-}

crudHelper
    :: (Formable a, Yesod master, YesodPersist master,
        Persist a (YesodDB master (GHandler sub master)))
    => String -> Maybe (Key a, a) -> master -> Bool -> GHandler sub master RepHtml
crudHelper title me _ isPost = do
    _ <- getYesod :: GHandler sub master master
    (errs, form) <- runForm $ formable $ fmap snd me
    errs' <- case (isPost, errs) of
                (True, Success a) -> do
                    case me of
                        Just (eid, _) -> runDB $ replace eid a
                        Nothing -> runDB $ insert a >> return ()
                    error "FIXME" -- redirect RedirectTemporary $ crudRead a
                (True, Failure e) -> return $ Just e
                (False, _) -> return Nothing
    let _maybe' = fmap snd me
{- FIXME
%p
    %a!href=@crudList.maybe'@ Return to list
-}
    applyLayout title mempty [$hamlet|
%h1 $cs.title$
$maybe errs' es
    %ul
        $forall es e
            %li $cs.e$
%form!method=post
    %table
        ^form^
        %tr
            %td!colspan=2
                %input!type=submit
|]

runForm :: Form xml (GHandler sub y) a -> GHandler sub y (Failing a, xml)
runForm f = do
    req <- getRequest
    (pp, _) <- liftIO $ reqRequestBody req
    let env = map (second Left) pp
    let (a, b, _) = runFormState env f
    a' <- a
    return (a', b)
