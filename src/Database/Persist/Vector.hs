{-# LANGUAGE AllowAmbiguousTypes, DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeApplications #-}

module Database.Persist.Vector
    ( StaticInt(..)
    , PgVector
    , pgFromVector
    , pgToVector
    ) where

import Data.Aeson (FromJSON, ToJSON, parseJSON)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Database.Persist
    ( PersistField, PersistValue(PersistLiteral_), LiteralType(Escaped)
    , fromPersistValue, toPersistValue
    )
import Database.Persist.Sql (PersistFieldSql, SqlType(SqlOther), sqlType)
import Text.Read (readMaybe)

class StaticInt n where
    getStaticInt :: Int

newtype PgVector n = PgVector (Vector Float)
    deriving newtype (Eq, Show, ToJSON)

instance StaticInt n => FromJSON (PgVector n) where
    parseJSON = fmap pgFromVector . parseJSON

instance StaticInt n => PersistField (PgVector n) where
    toPersistValue (PgVector n) = PersistLiteral_ Escaped . T.encodeUtf8 . T.pack $ show n
    fromPersistValue (PersistLiteral_ _ b)
        | Just v <- readMaybe . T.unpack $ T.decodeUtf8With T.lenientDecode b = pure $ pgFromVector v
    fromPersistValue _ = Left "Invalid PgVector"

instance StaticInt n => PersistFieldSql (PgVector n) where
    sqlType _ = SqlOther $ "vector(" <> T.pack (show $ getStaticInt @n) <> ")"

pgFromVector :: forall n. StaticInt n => Vector Float -> PgVector n
pgFromVector v = case compare (V.length v) (getStaticInt @n) of
    LT -> PgVector $ v <> V.replicate (getStaticInt @n - V.length v) 0
    EQ -> PgVector v
    GT -> PgVector $ V.take (getStaticInt @n) v

pgToVector :: PgVector n -> Vector Float
pgToVector (PgVector v) = v

