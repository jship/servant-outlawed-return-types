{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Outlawed where

import Data.Aeson (ToJSON)
--import Data.Constraint (Dict(Dict))
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID)
import GHC.Generics (type (:*:), type (:+:), Generic(Rep), K1, M1, U1, V1)
import GHC.TypeLits (ErrorMessage((:$$:), (:<>:), ShowType, Text), TypeError)
--import Network.HTTP.Types (StdMethod(GET))
import Servant.API (type (:<|>), type (:>), Get, JSON, PostNoContent, ReqBody, Verb)
--import Servant.API (Headers)

type MyAPI =
       "sensitive" :> Get '[JSON] VerySensitiveData
  :<|> "sensitive" :> ReqBody '[JSON] VerySensitiveData :> PostNoContent
  :<|> "underhanded" :> Get '[JSON] UnderhandedData

-- | Uncomment the @where@ clause (plus imports) to see the type errors.
myAPI :: Proxy MyAPI
myAPI = Proxy
--  where
--  _dict :: Dict (NoOutlawedTypes MyAPI)
--  _dict = Dict

data VerySensitiveData = VerySensitiveData
  { secret1 :: Text
  , secret2 :: Text
  } deriving stock (Generic)
    deriving anyclass (ToJSON)

newtype UnderhandedData = UnderhandedData
  { stuff :: VerySensitiveData
  } deriving stock (Generic)
    deriving anyclass (ToJSON)

type NoOutlawedTypes :: Type -> Constraint
type NoOutlawedTypes a = NoOutlawedTypes' a a

type NoOutlawedTypes' :: Type -> Type -> Constraint
class NoOutlawedTypes' a r
instance {-# OVERLAPPABLE #-}
  ( Generic a, GNoOutlawedTypes (Rep a) r
  ) => NoOutlawedTypes' a r
instance (CustomError VerySensitiveData r) => NoOutlawedTypes' VerySensitiveData r
instance (NoOutlawedTypes' a r, NoOutlawedTypes' b r) => NoOutlawedTypes' (a :<|> b) r
instance (NoOutlawedTypes' b r) => NoOutlawedTypes' (a :> b) r
instance
  ( NoOutlawedTypes' a (Verb method code contentTypes a)
  ) => NoOutlawedTypes' (Verb method code contentTypes a) r
instance NoOutlawedTypes' Int r
instance NoOutlawedTypes' Double r
instance NoOutlawedTypes' Char r
instance NoOutlawedTypes' Text r
instance NoOutlawedTypes' UTCTime r
instance NoOutlawedTypes' Day r
instance NoOutlawedTypes' UUID r

type GNoOutlawedTypes :: forall {k}. (k -> Type) -> Type -> Constraint
class GNoOutlawedTypes f r
instance GNoOutlawedTypes V1 r
instance GNoOutlawedTypes U1 r
instance (GNoOutlawedTypes f r, GNoOutlawedTypes g r) => GNoOutlawedTypes (f :+: g) r
instance (GNoOutlawedTypes f r, GNoOutlawedTypes g r) => GNoOutlawedTypes (f :*: g) r
instance NoOutlawedTypes' ty r => GNoOutlawedTypes (K1 tag ty) r
instance GNoOutlawedTypes f r => GNoOutlawedTypes (M1 tag meta f) r

type CustomError :: Type -> Type -> Constraint
type CustomError badTy containingTy =
  TypeError
    ( 'Text "This API forbids returning values of type:"
        ':$$: 'Text ""
        ':$$: 'Text "  " ':<>: 'ShowType badTy
        ':$$: 'Text ""
        ':$$: 'Text "API return type that contains the forbidden type:"
        ':$$: 'Text ""
        ':$$: 'Text "  " ':<>: 'ShowType containingTy
        ':$$: 'Text ""
    )
