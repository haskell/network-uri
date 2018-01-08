{-# OPTIONS_GHC -Wmissing-signatures #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.URI.Strong (
  URI(..)
  , RelativeReference(..)
  , URIAuth(..)
  , nullURI
  , validURI
  , rectify, rectifyAuth

  -- * Parsing
  , parseURI
  , parseURIReference
  , parseRelativeReference
  , parseAbsoluteURI

  -- * Test for strings containing various kinds of URI
  , isURI
  , isURIReference
  , isRelativeReference
  , isAbsoluteURI
  , isIPv6address
  , isIPv4address

  -- * Relative URIs
  , relativeTo
  , nonStrictRelativeTo
  , relativeFrom

  -- * Operations on URI strings
  -- | Support for putting strings into URI-friendly
  --   escaped format and getting them back again.
  --   This can't be done transparently in all cases, because certain
  --   characters have different meanings in different kinds of URI.
  --   The URI spec [3], section 2.4, indicates that all URI components
  --   should be escaped before they are assembled as a URI:
  --   \"Once produced, a URI is always in its percent-encoded form\"
  , uriToString, uriAuthToString
  , isReserved, isUnreserved
  , isAllowedInURI, isUnescapedInURI
  , isUnescapedInURIComponent
  , escapeURIChar
  , escapeURIString
  , unEscapeString
  , pathSegments

    -- * URI Normalization functions
  , normalizeCase
  , normalizeEscape
  , normalizePathSegments
  ) where

import qualified Network.URI as U

import Control.Monad (guard)
import Data.Data (Data)
import GHC.Generics
import Data.Typeable

type URIAuth = U.URIAuth

data URI = URI U.URI
    deriving (Eq, Ord, Typeable, Data, Generic)

data RelativeReference = RelativeReference U.URI
    deriving (Eq, Ord, Typeable, Data, Generic)

class URIReferenceData a where
  uriAuthority :: a -> Maybe URIAuth
  uriPath :: a -> String
  uriQuery :: a -> String
  uriFragment :: a -> String

uriScheme :: URI -> String
uriScheme (URI u) = U.uriScheme u

instance URIReferenceData URI where
  uriAuthority (URI u) = U.uriAuthority u
  uriPath (URI u) = U.uriPath u
  uriQuery (URI u) = U.uriQuery u
  uriFragment (URI u) = U.uriFragment u

instance URIReferenceData RelativeReference where
  uriAuthority (RelativeReference u) = U.uriAuthority u
  uriPath (RelativeReference u) = U.uriPath u
  uriQuery (RelativeReference u) = U.uriQuery u
  uriFragment (RelativeReference u) = U.uriFragment u

instance Show URI where
  show (URI u) = show u

instance Show RelativeReference where
  show (RelativeReference u) = show u

-- Note this nullURI a bit strange; it is actually invalid. Use
-- validURI to determine if a URI object is actually legitimate.
nullURI :: URI
nullURI = URI U.nullURI

-- TODO: validURI should also check that the other fields are valid,
-- namely that the delimiter characters are present on any nonempty
-- fields.
validURI :: URI -> Bool
validURI (URI u) = U.uriIsAbsolute u

rectify :: URI -> URI
rectify (URI u) = URI (U.rectify u)

rectifyRelativeReference :: RelativeReference -> RelativeReference
rectifyRelativeReference (RelativeReference u) = RelativeReference (U.rectify u)

rectifyAuth :: URIAuth -> URIAuth
rectifyAuth = U.rectifyAuth

nullRelativeReference :: RelativeReference
nullRelativeReference = RelativeReference U.nullURI

parseURI :: String -> Maybe URI
parseURI s =
  do u <- U.parseURI s
     guard (U.uriIsAbsolute u)
     return (URI u)

parseURIReference :: String -> Maybe (Either URI RelativeReference)
parseURIReference s =
  do u <- U.parseURIReference s
     if U.uriIsAbsolute u then
       return (Left (URI u))
     else
       return (Right (RelativeReference u))

parseRelativeReference :: String -> Maybe RelativeReference
parseRelativeReference s =
  do u <- U.parseRelativeReference s
     return (RelativeReference u)

parseAbsoluteURI :: String -> Maybe URI
parseAbsoluteURI s =
  do u <- U.parseAbsoluteURI s
     return (URI u)

isJust :: Maybe t -> Bool
isJust Nothing = False
isJust _ = True

isURI :: String -> Bool
isURI = isJust . parseURI

isURIReference :: String -> Bool
isURIReference s = isURI s || isRelativeReference s

isRelativeReference :: String -> Bool
isRelativeReference = isJust . parseRelativeReference

isAbsoluteURI :: String -> Bool
isAbsoluteURI s = isJust $
  do u <- parseURI s
     Control.Monad.guard (uriFragment u == "")
     return u

isIPv6address :: String -> Bool
isIPv6address = U.isIPv6address

isIPv4address :: String -> Bool
isIPv4address = U.isIPv4address

relativeTo :: RelativeReference -> URI -> URI
relativeTo (RelativeReference rel) (URI abs) =
  wrapURI (U.relativeTo rel abs)

nonStrictRelativeTo :: RelativeReference -> URI -> URI
nonStrictRelativeTo (RelativeReference rel) (URI abs) =
  wrapURI (U.nonStrictRelativeTo rel abs)

wrapURI :: U.URI -> URI
wrapURI u =
  if U.uriIsAbsolute u then
    URI u
  else
    error "Programming error"

wrapRelativeReference :: U.URI -> RelativeReference
wrapRelativeReference u =
  if U.uriIsRelative u then
    RelativeReference u
  else
    error "Programming error"

relativeFrom :: URI -> URI -> RelativeReference
relativeFrom (URI target) (URI base) =
  wrapRelativeReference (U.relativeFrom target base)

uriToString :: (String -> String) -> URI -> ShowS
uriToString f (URI u) =
  U.uriToString f u

relativeReferenceToString :: (String -> String) -> RelativeReference
                             -> ShowS
relativeReferenceToString f (RelativeReference u) =
  U.uriToString f u

uriAuthToString :: (String -> String) -> Maybe U.URIAuth -> ShowS
uriAuthToString = U.uriAuthToString

isReserved :: Char -> Bool
isReserved = U.isReserved

isUnreserved :: Char -> Bool
isUnreserved = U.isUnreserved

isAllowedInURI :: Char -> Bool
isAllowedInURI = U.isAllowedInURI

isUnescapedInURI :: Char -> Bool
isUnescapedInURI = U.isUnescapedInURI

isUnescapedInURIComponent :: Char -> Bool
isUnescapedInURIComponent = U.isUnescapedInURIComponent

escapeURIChar :: (Char -> Bool) -> Char -> String
escapeURIChar = U.escapeURIChar

escapeURIString :: (Char -> Bool) -> String -> String
escapeURIString = U.escapeURIString

unEscapeString :: String -> String
unEscapeString = U.unEscapeString

pathSegments :: URI -> [String]
pathSegments (URI u) = U.pathSegments u

pathSegmentsRef :: RelativeReference -> [String]
pathSegmentsRef (RelativeReference u) = U.pathSegments u

normalizeCase :: String -> String
normalizeCase = U.normalizeCase

normalizeEscape :: String -> String
normalizeEscape = U.normalizeEscape

normalizePathSegments :: String -> String
normalizePathSegments = U.normalizePathSegments
