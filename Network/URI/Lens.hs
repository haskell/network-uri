{-# LANGUAGE Rank2Types #-}
-- | Network uri lenses
module Network.URI.Lens
  ( uriRegNameLens
  , uriUserInfoLens
  , uriPortLens
  , uriAuthorityLens
  , uriSchemeLens
  , uriPathLens
  , uriQueryLens
  , uriFragmentLens
  ) where

import           Network.URI

type Lens' s a = Lens s s a a
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens sa sbt afb s = sbt s <$> afb (sa s)

uriRegNameLens :: Lens' URIAuth String
uriRegNameLens = lens uriRegName (\parent newVal -> parent {uriRegName = newVal})

uriUserInfoLens :: Lens' URIAuth String
uriUserInfoLens =
  lens uriUserInfo (\parent newVal -> parent {uriUserInfo = newVal})

uriPortLens :: Lens' URIAuth String
uriPortLens = lens uriPort (\parent newVal -> parent {uriPort = newVal})

uriAuthorityLens :: Lens' URI (Maybe URIAuth)
uriAuthorityLens =
  lens uriAuthority (\parent newVal -> parent {uriAuthority = newVal})

uriSchemeLens :: Lens' URI String
uriSchemeLens = lens uriScheme (\parent newVal -> parent {uriScheme = newVal})

uriPathLens :: Lens' URI String
uriPathLens = lens uriPath (\parent newVal -> parent {uriPath = newVal})

uriQueryLens :: Lens' URI String
uriQueryLens = lens uriQuery (\parent newVal -> parent {uriQuery = newVal})

uriFragmentLens :: Lens' URI String
uriFragmentLens =
  lens uriFragment (\parent newVal -> parent {uriFragment = newVal})
