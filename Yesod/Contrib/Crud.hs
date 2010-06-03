{-# LANGUAGE TypeFamilies, QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Contrib.Crud where

import Yesod hiding (Form)
import Database.Persist
import Control.Applicative.Error
import Web.Routes.Quasi (Routes)
import Yesod.Contrib.Formable
import Text.Formlets
import Control.Arrow (second)
import Control.Monad.Trans.Reader (ReaderT)

class RunDB y where
    type DBConn y
    runDB :: ReaderT (DBConn y) (Handler y) a -> Handler y a

class Crudable a where
    type CrudApp a
    crudList :: Maybe a -> Routes (CrudApp a)
    crudCreate :: Maybe a -> Routes (CrudApp a)
    crudRead :: a -> Routes (CrudApp a)
    crudEdit :: a -> Routes (CrudApp a)
    crudDelete :: a -> Routes (CrudApp a)

crudHelper
    :: (Crudable a, Formable a, Yesod (CrudApp a), RunDB (CrudApp a),
        Persist a (ReaderT (DBConn (CrudApp a)) (Handler (CrudApp a))))
    => String -> Bool -> Maybe (Key a, a) -> Handler (CrudApp a) RepHtml
crudHelper title isPost me = do
    (errs, form) <- runForm $ formable $ fmap snd me
    errs' <- case (isPost, errs) of
                (True, Success a) -> do
                    case me of
                        Just (eid, _) -> runDB $ replace eid a
                        Nothing -> runDB $ insert a >> return ()
                    redirect RedirectTemporary $ crudRead a
                (True, Failure e) -> return $ Just e
                (False, _) -> return Nothing
    applyLayout title (return ()) [$hamlet|
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

runForm :: Form xml (Handler y) a -> Handler y (Failing a, xml)
runForm f = do
    req <- getRequest
    (pp, _) <- liftIO $ reqRequestBody req
    let env = map (second Left) pp
    let (a, b, c) = runFormState env f
    a' <- a
    return (a', b)
