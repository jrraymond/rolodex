{- # Haskell Rolodex
{-# LANGUAGE KitchenSink #-}
-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

import           Control.Monad.Logger                 (runNoLoggingT)
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Either                          (isRight)
import qualified Data.Function                        as F (on)
import qualified Data.List                            as L (groupBy)
import           Data.Maybe                           (fromJust, fromMaybe,
                                                       isJust)
import           Data.Monoid                          ((<>))
import           Data.Text                            (Text)
import qualified Data.Text                            as T (pack)
import           Database.Esqueleto
import           Database.Persist.Sqlite              (withSqlitePool)
import           Database.Persist.TH
import           GHC.Generics                         hiding (from)
import           GHC.Int                              (Int64)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.RequestLogger
import           Servant

import           DerivedTypes


share [mkPersist sqlSettings, mkMigrate "migrateTables"] [persistLowerCase|
Contact
  email       Email
  phoneNumber Phone
  deriving Eq Read Show Generic

Attribute
  contactId   ContactId
  name        Text
  value       Text
  UniqueAttr  contactId name
  deriving Eq Read Show Generic
|]

data ContactIn =
  ContactIn { email      :: Text
            , phone      :: Text
            , attributes :: [(Text,Text)] }
            deriving (Show,Read,Eq,Generic)

type ContactOut = (Key Contact, ContactIn)

instance ToJSON ContactIn
instance FromJSON ContactIn
instance ToJSON Contact
instance ToJSON Attribute


mkContactOut :: Entity Contact -> [Entity Attribute] -> ContactOut
mkContactOut (Entity k (Contact (Email em) (Phone ph))) attrs =
  (k,ContactIn em ph (map (\(Entity _ (Attribute _ n v)) -> (n,v)) attrs))


type UserAPI = "search" :> QueryParam "for" Text :> Get '[JSON] [ContactOut]
          :<|> "add" :> ReqBody '[JSON] ContactIn :> Post '[JSON] (Either Text ContactIn)
          :<|> "delete" :> ReqBody '[JSON] [Key Contact] :> Post '[JSON] Int64
          :<|> "update" :> ReqBody '[JSON] (Key Contact,[(Text,Text)]) :> Post '[JSON] ()


server :: ConnectionPool -> Server UserAPI
server pool = searchR :<|> addR :<|> deleteR :<|> updateR
  where
    updateR attrs = runSqlPool (updateQ attrs) pool
    deleteR ks = runSqlPool (deleteQ ks) pool
    searchR n = queryToContactIns <$> runSqlPool (searchQ n) pool
    addR nc = case validateEmail . email $ nc of
               Left err -> return $ Left err
               Right em -> case validatePhone . phone $ nc of
                             Left err -> return $ Left err
                             Right ph -> do let q = addQ em ph (attributes nc)
                                            _ <- runSqlPool q pool
                                            return $ Right nc


updatePhone :: MonadIO m => Key Contact -> Phone -> SqlPersistT m ()
updatePhone k ph =
  update $ \c -> do
    set c [ ContactPhoneNumber =. val ph ]
    where_ (c ^. ContactId ==. val k)


updateEmail :: MonadIO m => Key Contact -> Email -> SqlPersistT m ()
updateEmail k em =
  update $ \c -> do
    set c [ ContactEmail =. val em ]
    where_ (c ^. ContactId ==. val k)


{- Creates attribute if the name of the attribute does not exist.
- Replaces value of attribute otherwise.
- If the contact does not exist, nothing will be added.
-}
upsertAttributes :: MonadIO m => Key Contact -> [(Text,Text)] -> SqlPersistT m ()
upsertAttributes cid = mapM_ stmt
  where
    stmt (n,v) =
      let k = T.pack . show . fromSqlKey $ cid
          vals = "((select id from contact where id = " <> k <> "),\"" <> n <> "\",\"" <> v <> "\")"
          q = "INSERT OR REPLACE INTO attribute (contact_id,name,value) VALUES " <> vals <> ";"
      in rawExecute q ([] :: [PersistValue])


fromRight :: Either a b -> b
fromRight = either (error "fromRight of Left") id


{- Updates contact by id with the given list of attributes.
- If an invalid email or phone are given then they are not updated.
-}
updateQ :: MonadIO m => (Key Contact, [(Text, Text)]) -> ReaderT SqlBackend m ()
updateQ (k,attrs) = do
  let attrs' = filter (\(n,_) -> n /= "email" && n /= "phone") attrs
      em = validateEmail <$> lookup "email" attrs
      ph = validatePhone <$> lookup "phone" attrs
  when (fromMaybe False $ isRight <$> em) $ updateEmail k (fromRight (fromJust em))
  when (fromMaybe False $ isRight <$> ph) $ updatePhone k (fromRight (fromJust ph))
  upsertAttributes k attrs'


{- Deletes all contacts and their attributes if the contact id is in
- the given list. Returns the number of contacts deleted
-}
deleteQ :: MonadIO m => [Key Contact] -> ReaderT SqlBackend m Int64
deleteQ k = do
  n <- deleteCount (from (\c -> where_ (c ^. ContactId `in_` valList k)))
  delete (from (\a -> where_ (a ^.AttributeContactId `in_` valList k)))
  return n


{- Adds a new contact with the email and phone.
- Returns the id of the new contact.
- precondition: email and phone are valid
- -}
addQ :: MonadIO m => Email -> Phone -> [(Text, Text)] -> ReaderT SqlBackend m (Key Contact)
addQ em ph opts = do
  k <- insert (Contact em ph)
  _ <- insertMany (map (uncurry  (Attribute k)) opts)
  return k


{- Returns list of contacts paired with their optional attributes
- who have any attribute that matches the text given.
- If the text is Nothing, then returns all contacts.
-}
searchQ :: MonadIO m =>
           Maybe Text ->
           SqlPersistT m [(Entity Contact, Maybe (Entity Attribute))]
searchQ Nothing =
  select $
    from $ \(c `LeftOuterJoin` a) -> do
      on (just (c ^. ContactId) ==. a ?. AttributeContactId)
      orderBy [ asc (c ^. ContactId), asc (a ?. AttributeName) ]
      return (c,a)
searchQ (Just n) =
  select $
    from $ \(c `LeftOuterJoin` a) -> do
      on (just (c ^. ContactId) ==. a ?. AttributeContactId)
      where_ ((c ^. ContactEmail ==. val (Email n)) ||.
              (c ^. ContactPhoneNumber) ==. val (Phone n) ||.
              (a ?. AttributeValue) ==. just (val n))
      return (c,a)


queryToContactIns :: [(Entity Contact, Maybe (Entity Attribute))] -> [ContactOut]
queryToContactIns = toContactIn . squash . groupByContact
  where
    groupByContact = L.groupBy ((==) `F.on` fst)
    keepJusts = map fromJust . filter isJust . map snd
    squash = map (\x -> (fst (head x), keepJusts x))
    toContactIn = map (uncurry mkContactOut)


userAPI :: Proxy UserAPI
userAPI = Proxy


app :: ConnectionPool -> Application
app pool = logStdout (serve userAPI (server pool))


main :: IO ()
main = runNoLoggingT . withSqlitePool "rolodex" 100 $ \pool ->
  liftIO $ runSqlPool (runMigration migrateTables) pool >>
  run 8080 (app pool)
