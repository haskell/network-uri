{-# LANGUAGE Rank2Types #-}
-- | Network uri lenses
module Network.URI.Lens
  ( regNameLens
  , userInfoLens
  , portLens
  , uriAuthLens
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

regNameLens :: Lens' URIAuth String
regNameLens = lens uriRegName (\parent newVal -> parent {uriRegName = newVal})

userInfoLens :: Lens' URIAuth String
userInfoLens =
  lens uriUserInfo (\parent newVal -> parent {uriUserInfo = newVal})

portLens :: Lens' URIAuth String
portLens = lens uriPort (\parent newVal -> parent {uriPort = newVal})

uriAuthLens :: Lens' URI (Maybe URIAuth)
uriAuthLens =
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
