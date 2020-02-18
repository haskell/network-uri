#if __GLASGOW_HASKELL__ < 800
{-# LANGUAGE RecordWildCards, TemplateHaskell, ViewPatterns #-}
#else
{-# LANGUAGE RecordWildCards, TemplateHaskellQuotes, ViewPatterns #-}
#endif
#if MIN_VERSION_template_haskell(2,12,0)
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Network.URI.Static
    (
    -- * Absolute URIs
      uri
#if __GLASGOW_HASKELL__ >= 708
    , staticURI
#endif
    , staticURI'
    -- * Relative URIs
    , relativeReference
#if __GLASGOW_HASKELL__ >= 708
    , staticRelativeReference
#endif
    , staticRelativeReference'
    ) where

import Language.Haskell.TH.Lib (ExpQ)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Network.URI (URI(..), parseURI, parseRelativeReference)

#if __GLASGOW_HASKELL__ >= 708
import Language.Haskell.TH.Lib (TExpQ)
import Language.Haskell.TH.Syntax (unTypeQ)
#endif

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XQuasiQuotes

----------------------------------------------------------------------------
-- Absolute URIs
----------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 708
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
#endif

-- | 'staticURI'' parses a specified string at compile time.
--
-- The typed template haskell 'staticURI' is available only with GHC-7.8+.
staticURI' :: String    -- ^ String representation of a URI
           -> ExpQ      -- ^ URI
#if __GLASGOW_HASKELL__ >= 708
staticURI' = unTypeQ . staticURI
#else
staticURI' (parseURI -> Just u) = [| u |]
staticURI' s = fail $ "Invalid URI: " ++ s
#endif

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
    quoteExp =  staticURI',
    quotePat = undefined,
    quoteType = undefined,
    quoteDec = undefined
}

----------------------------------------------------------------------------
-- Relative URIs
----------------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 708
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
#endif

-- | 'staticRelativeReference'' parses a specified string at compile time and
--   return an expression representing the URI when it's a valid relative
--   reference. Otherwise, it emits an error.
--
-- The typed template haskell 'staticRelativeReference' is available only with GHC-7.8+.
staticRelativeReference' :: String -- ^ String representation of a reference
                         -> ExpQ   -- ^ Refererence
#if __GLASGOW_HASKELL__ >= 708
staticRelativeReference' = unTypeQ . staticRelativeReference
#else
staticRelativeReference' (parseRelativeReference -> Just ref) = [| ref |]
staticRelativeReference' ref = fail $ "Invalid relative reference: " ++ ref
#endif

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
    quoteExp = staticRelativeReference',
    quotePat = undefined,
    quoteType = undefined,
    quoteDec = undefined
}
