{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module DerivedTypes where

import           Data.Aeson
import           Data.Char               (isDigit)
import           Data.Either.Combinators (mapBoth)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Database.Persist.TH
import           GHC.Generics            (Generic)
import qualified Text.Email.Validate     as E (isValid, validate)


newtype Phone = Phone { runPhone :: Text } deriving (Eq,Read,Show,Generic)
newtype Email = Email { runEmail :: Text } deriving (Eq,Read,Show,Generic)


instance ToJSON Phone
instance FromJSON Phone
instance ToJSON Email
instance FromJSON Email


derivePersistField "Email"
derivePersistField "Phone"


validEmail :: Text -> Bool
validEmail = E.isValid . T.encodeUtf8


validateEmail :: Text -> Either Text Email
validateEmail = mapBoth T.pack (Email . T.pack . show) . E.validate . T.encodeUtf8


validatePhone :: Text -> Either Text Phone
validatePhone t0
  | T.length t == 10 = Right (Phone t)
  | otherwise = Left t
  where t = T.filter isDigit t0
