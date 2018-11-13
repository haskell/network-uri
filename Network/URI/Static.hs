#if __GLASGOW_HASKELL__ < 800
module Network.URI.Static () where
#else

{-# LANGUAGE RecordWildCards, TemplateHaskellQuotes, ViewPatterns #-}

module Network.URI.Static
    (
    -- * Absolute URIs
      staticURI
    , uri
    -- * Relative URIs
    , relativeReference
    , staticRelativeReference
    ) where

import Language.Haskell.TH (unType)
import Language.Haskell.TH.Lib (TExpQ)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Network.URI (URI(..), parseURI, parseRelativeReference)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XQuasiQuotes

----------------------------------------------------------------------------
-- Absolute URIs
----------------------------------------------------------------------------

-- | 'staticURI' parses a specified string at compile time
--   and return an expression representing the URI when it's a valid URI.
--   Otherwise, it emits an error.
--
-- >>> $$(staticURI "http://www.google.com/")
-- http://www.google.com/
--
-- >>> $$(staticURI "http://www.google.com/##")
-- <BLANKLINE>
-- <interactive>...
-- ... Invalid URI: http://www.google.com/##
-- ...
staticURI :: String    -- ^ String representation of a URI
          -> TExpQ URI -- ^ URI
staticURI (parseURI -> Just u) = [|| u ||]
staticURI s = fail $ "Invalid URI: " ++ s

-- | 'uri' is a quasi quoter for 'staticURI'.
--
-- >>> [uri|http://www.google.com/|]
-- http://www.google.com/
--
-- >>> [uri|http://www.google.com/##|]
-- <BLANKLINE>
-- <interactive>...
-- ... Invalid URI: http://www.google.com/##
-- ...
uri :: QuasiQuoter
uri = QuasiQuoter {
    quoteExp = fmap unType . staticURI,
    quotePat = undefined,
    quoteType = undefined,
    quoteDec = undefined
}

----------------------------------------------------------------------------
-- Relative URIs
----------------------------------------------------------------------------

-- | 'staticRelativeReference' parses a specified string at compile time and
--   return an expression representing the URI when it's a valid relative
--   reference. Otherwise, it emits an error.
--
-- >>> $$(staticRelativeReference "/foo?bar=baz#quux")
-- /foo?bar=baz#quux
--
-- >>> $$(staticRelativeReference "http://www.google.com/")
-- <BLANKLINE>
-- <interactive>...
-- ... Invalid relative reference: http://www.google.com/
-- ...
staticRelativeReference :: String -- ^ String representation of a reference
                        -> TExpQ URI -- ^ Refererence
staticRelativeReference (parseRelativeReference -> Just ref) = [|| ref ||]
staticRelativeReference ref = fail $ "Invalid relative reference: " ++ ref

-- | 'relativeReference' is a quasi quoter for 'staticRelativeReference'.
--
-- >>> [relativeReference|/foo?bar=baz#quux|]
-- /foo?bar=baz#quux
--
-- >>> [relativeReference|http://www.google.com/|]
-- <BLANKLINE>
-- <interactive>...
-- ... Invalid relative reference: http://www.google.com/
-- ...
relativeReference :: QuasiQuoter
relativeReference = QuasiQuoter {
    quoteExp = fmap unType . staticRelativeReference,
    quotePat = undefined,
    quoteType = undefined,
    quoteDec = undefined
}

#endif
