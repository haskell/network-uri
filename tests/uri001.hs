{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
--------------------------------------------------------------------------------
--  $Id: URITest.hs,v 1.8 2005/07/19 22:01:27 gklyne Exp $
--
--  Copyright (c) 2004, G. KLYNE.  All rights reserved.
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  URITest
--  Copyright   :  (c) 2004, Graham Klyne
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Graham Klyne
--  Stability   :  provisional
--  Portability :  H98
--
--  This Module contains test cases for module URI.
--
--  To run this test without using Cabal to build the package
--  (2013-01-05, instructions tested on macOS):
--  1. Install Haskell platform
--  2. cabal install tasty tasty-hunit tasty-quickcheck QuickCheck
--  3. ghc -XDeriveDataTypeable -XDeriveGeneric -package QuickCheck -package tasty -package HUnit ../Network/URI.hs uri001.hs
--  5. ./uri001
--
--------------------------------------------------------------------------------

module Main where

import Network.URI
    ( URI(..), URIAuth(..)
    , nullURI
    , rectify, rectifyAuth
    , parseURI, parseURIReference, parseRelativeReference, parseAbsoluteURI
    , parseAbsoluteURI
    , isURI, isURIReference, isRelativeReference, isAbsoluteURI
    , uriIsAbsolute, uriIsRelative
    , relativeTo, nonStrictRelativeTo
    , relativeFrom
    , uriToString, uriAuthToString
    , isUnescapedInURIComponent
    , isUnescapedInURI, escapeURIString, unEscapeString
    , normalizeCase, normalizeEscape, normalizePathSegments
    , pathSegments
    )

import Data.Bits ((.&.), (.|.))
import Data.Char (ord, chr)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.List (intercalate)
import System.IO (openFile, IOMode(WriteMode), hClose)
import qualified Test.Tasty as TF
import qualified Test.Tasty.HUnit as TF
import Test.Tasty.HUnit ((@?=), (@=?))
import qualified Test.Tasty.QuickCheck as TF
import Test.QuickCheck ((==>), Property)
#if __GLASGOW_HASKELL__ >= 760
import Text.Read (readMaybe)
#endif

data URIType = AbsId    -- URI form (absolute, no fragment)
             | AbsRf    -- Absolute URI reference
             | RelRf    -- Relative URI reference
             | InvRf    -- Invalid URI reference
isValidT :: URIType -> Bool
isValidT InvRf = False
isValidT _     = True

isAbsRfT :: URIType -> Bool
isAbsRfT AbsId = True
isAbsRfT AbsRf = True
isAbsRfT _     = False

isRelRfT :: URIType -> Bool
isRelRfT RelRf = True
isRelRfT _     = False

isAbsIdT :: URIType -> Bool
isAbsIdT AbsId = True
isAbsIdT _     = False

testEq :: (Eq a, Show a) => String -> a -> a -> TF.Assertion
testEq lab a1 a2 = TF.assertEqual lab a1 a2

testURIRef :: URIType -> String -> TF.Assertion
testURIRef t u = sequence_
  [ testEq ("test_isURIReference:"++u) (isValidT t) (isURIReference u)
  , testEq ("test_isRelativeReference:"++u)  (isRelRfT t) (isRelativeReference  u)
  , testEq ("test_isAbsoluteURI:"++u)  (isAbsIdT t) (isAbsoluteURI  u)
  ]

testURIRefComponents :: Maybe URI -> String -> TF.Assertion
testURIRefComponents uv us =
    uv @?= (parseURIReference us)


testURIRefSuite = TF.testGroup "Test URIrefs" testURIRefList
testURIRefList =
  [ TF.testCase "testURIRef001" $ testURIRef AbsRf "http://example.org/aaa/bbb#ccc"
  , TF.testCase "testURIRef002" $ testURIRef AbsId "mailto:local@domain.org"
  , TF.testCase "testURIRef003" $ testURIRef AbsRf "mailto:local@domain.org#frag"
  , TF.testCase "testURIRef004" $ testURIRef AbsRf "HTTP://EXAMPLE.ORG/AAA/BBB#CCC"
  , TF.testCase "testURIRef005" $ testURIRef RelRf "//example.org/aaa/bbb#ccc"
  , TF.testCase "testURIRef006" $ testURIRef RelRf "/aaa/bbb#ccc"
  , TF.testCase "testURIRef007" $ testURIRef RelRf "bbb#ccc"
  , TF.testCase "testURIRef008" $ testURIRef RelRf "#ccc"
  , TF.testCase "testURIRef009" $ testURIRef RelRf "#"
  , TF.testCase "testURIRef010" $ testURIRef RelRf "/"
    -- escapes
  , TF.testCase "testURIRef011" $ testURIRef AbsRf "http://example.org/aaa%2fbbb#ccc"
  , TF.testCase "testURIRef012" $ testURIRef AbsRf "http://example.org/aaa%2Fbbb#ccc"
  , TF.testCase "testURIRef013" $ testURIRef RelRf "%2F"
  , TF.testCase "testURIRef014" $ testURIRef RelRf "aaa%2Fbbb"
    -- ports
  , TF.testCase "testURIRef015" $ testURIRef AbsRf "http://example.org:80/aaa/bbb#ccc"
  , TF.testCase "testURIRef016" $ testURIRef AbsRf "http://example.org:/aaa/bbb#ccc"
  , TF.testCase "testURIRef017" $ testURIRef AbsRf "http://example.org./aaa/bbb#ccc"
  , TF.testCase "testURIRef018" $ testURIRef AbsRf "http://example.123./aaa/bbb#ccc"
    -- bare authority
  , TF.testCase "testURIRef019" $ testURIRef AbsId "http://example.org"
    -- IPv6 literals (from RFC2732):
  , TF.testCase "testURIRef021" $ testURIRef AbsId "http://[FEDC:BA98:7654:3210:FEDC:BA98:7654:3210]:80/index.html"
  , TF.testCase "testURIRef022" $ testURIRef AbsId "http://[1080:0:0:0:8:800:200C:417A]/index.html"
  , TF.testCase "testURIRef023" $ testURIRef AbsId "http://[3ffe:2a00:100:7031::1]"
  , TF.testCase "testURIRef024" $ testURIRef AbsId "http://[1080::8:800:200C:417A]/foo"
  , TF.testCase "testURIRef025" $ testURIRef AbsId "http://[::192.9.5.5]/ipng"
  , TF.testCase "testURIRef026" $ testURIRef AbsId "http://[::FFFF:129.144.52.38]:80/index.html"
  , TF.testCase "testURIRef027" $ testURIRef AbsId "http://[2010:836B:4179::836B:4179]"
  , TF.testCase "testURIRef028" $ testURIRef RelRf "//[2010:836B:4179::836B:4179]"
  , TF.testCase "testURIRef029" $ testURIRef InvRf "[2010:836B:4179::836B:4179]"
  , TF.testCase "testURIRef030" $ testURIRef AbsId "http://[fe80::ff:fe00:1%25eth0]"
    -- RFC2396 test cases
  , TF.testCase "testURIRef031" $ testURIRef RelRf "./aaa"
  , TF.testCase "testURIRef032" $ testURIRef RelRf "../aaa"
  , TF.testCase "testURIRef033" $ testURIRef AbsId "g:h"
  , TF.testCase "testURIRef034" $ testURIRef RelRf "g"
  , TF.testCase "testURIRef035" $ testURIRef RelRf "./g"
  , TF.testCase "testURIRef036" $ testURIRef RelRf "g/"
  , TF.testCase "testURIRef037" $ testURIRef RelRf "/g"
  , TF.testCase "testURIRef038" $ testURIRef RelRf "//g"
  , TF.testCase "testURIRef039" $ testURIRef RelRf "?y"
  , TF.testCase "testURIRef040" $ testURIRef RelRf "g?y"
  , TF.testCase "testURIRef041" $ testURIRef RelRf "#s"
  , TF.testCase "testURIRef042" $ testURIRef RelRf "g#s"
  , TF.testCase "testURIRef043" $ testURIRef RelRf "g?y#s"
  , TF.testCase "testURIRef044" $ testURIRef RelRf ";x"
  , TF.testCase "testURIRef045" $ testURIRef RelRf "g;x"
  , TF.testCase "testURIRef046" $ testURIRef RelRf "g;x?y#s"
  , TF.testCase "testURIRef047" $ testURIRef RelRf "."
  , TF.testCase "testURIRef048" $ testURIRef RelRf "./"
  , TF.testCase "testURIRef049" $ testURIRef RelRf ".."
  , TF.testCase "testURIRef050" $ testURIRef RelRf "../"
  , TF.testCase "testURIRef051" $ testURIRef RelRf "../g"
  , TF.testCase "testURIRef052" $ testURIRef RelRf "../.."
  , TF.testCase "testURIRef053" $ testURIRef RelRf "../../"
  , TF.testCase "testURIRef054" $ testURIRef RelRf "../../g"
  , TF.testCase "testURIRef055" $ testURIRef RelRf "../../../g"
  , TF.testCase "testURIRef056" $ testURIRef RelRf "../../../../g"
  , TF.testCase "testURIRef057" $ testURIRef RelRf "/./g"
  , TF.testCase "testURIRef058" $ testURIRef RelRf "/../g"
  , TF.testCase "testURIRef059" $ testURIRef RelRf "g."
  , TF.testCase "testURIRef060" $ testURIRef RelRf ".g"
  , TF.testCase "testURIRef061" $ testURIRef RelRf "g.."
  , TF.testCase "testURIRef062" $ testURIRef RelRf "..g"
  , TF.testCase "testURIRef063" $ testURIRef RelRf "./../g"
  , TF.testCase "testURIRef064" $ testURIRef RelRf "./g/."
  , TF.testCase "testURIRef065" $ testURIRef RelRf "g/./h"
  , TF.testCase "testURIRef066" $ testURIRef RelRf "g/../h"
  , TF.testCase "testURIRef067" $ testURIRef RelRf "g;x=1/./y"
  , TF.testCase "testURIRef068" $ testURIRef RelRf "g;x=1/../y"
  , TF.testCase "testURIRef069" $ testURIRef RelRf "g?y/./x"
  , TF.testCase "testURIRef070" $ testURIRef RelRf "g?y/../x"
  , TF.testCase "testURIRef071" $ testURIRef RelRf "g#s/./x"
  , TF.testCase "testURIRef072" $ testURIRef RelRf "g#s/../x"
  , TF.testCase "testURIRef073" $ testURIRef RelRf ""
  , TF.testCase "testURIRef074" $ testURIRef RelRf "A'C"
  , TF.testCase "testURIRef075" $ testURIRef RelRf "A$C"
  , TF.testCase "testURIRef076" $ testURIRef RelRf "A@C"
  , TF.testCase "testURIRef077" $ testURIRef RelRf "A,C"
    -- Invalid
  , TF.testCase "testURIRef080" $ testURIRef InvRf "http://foo.org:80Path/More"
  , TF.testCase "testURIRef081" $ testURIRef InvRf "::"
  , TF.testCase "testURIRef082" $ testURIRef InvRf " "
  , TF.testCase "testURIRef083" $ testURIRef InvRf "%"
  , TF.testCase "testURIRef084" $ testURIRef InvRf "A%Z"
  , TF.testCase "testURIRef085" $ testURIRef InvRf "%ZZ"
  , TF.testCase "testURIRef086" $ testURIRef InvRf "%AZ"
  , TF.testCase "testURIRef087" $ testURIRef InvRf "A C"
    -- testURIRef088 -- (case removed)
    -- testURIRef089 -- (case removed)
  , TF.testCase "testURIRef090" $ testURIRef InvRf "A\"C"
  , TF.testCase "testURIRef091" $ testURIRef InvRf "A`C"
  , TF.testCase "testURIRef092" $ testURIRef InvRf "A<C"
  , TF.testCase "testURIRef093" $ testURIRef InvRf "A>C"
  , TF.testCase "testURIRef094" $ testURIRef InvRf "A^C"
  , TF.testCase "testURIRef095" $ testURIRef InvRf "A\\C"
  , TF.testCase "testURIRef096" $ testURIRef InvRf "A{C"
  , TF.testCase "testURIRef097" $ testURIRef InvRf "A|C"
  , TF.testCase "testURIRef098" $ testURIRef InvRf "A}C"
    -- testURIRef099,
    --
    -- From RFC2396:
    -- rel_segment   = 1*( unreserved | escaped |
    --                     ";" | "@" | "&" | "=" | "+" | "$" | "," )
    -- unreserved    = alphanum | mark
    -- mark          = "-" | "_" | "." | "!" | "~" | "*" | "'" |
    --                 "(" | ")"
    -- Note RFC 2732 allows '[', ']' ONLY for reserved purpose of IPv6 literals,
    -- or does it?
  , TF.testCase "testURIRef101" $ testURIRef InvRf "A[C"
  , TF.testCase "testURIRef102" $ testURIRef InvRf "A]C"
  , TF.testCase "testURIRef103" $ testURIRef InvRf "A[**]C"
  , TF.testCase "testURIRef104" $ testURIRef InvRf "http://[xyz]/"
  , TF.testCase "testURIRef105" $ testURIRef InvRf "http://]/"
  , TF.testCase "testURIRef106" $ testURIRef InvRf "http://example.org/[2010:836B:4179::836B:4179]"
  , TF.testCase "testURIRef107" $ testURIRef InvRf "http://example.org/abc#[2010:836B:4179::836B:4179]"
  , TF.testCase "testURIRef108" $ testURIRef InvRf "http://example.org/xxx/[qwerty]#a[b]"
    -- Random other things that crop up
  , TF.testCase "testURIRef111" $ testURIRef AbsRf "http://example/Andr&#567;"
  , TF.testCase "testURIRef112" $ testURIRef AbsId "file:///C:/DEV/Haskell/lib/HXmlToolbox-3.01/examples/"
  , TF.testCase "testURIRef113" $ testURIRef AbsId "http://46229EFFE16A9BD60B9F1BE88B2DB047ADDED785/demo.mp3"
  , TF.testCase "testURIRef114" $ testURIRef InvRf "http://example.org/xxx/qwerty#a#b"
  , TF.testCase "testURIRef115" $ testURIRef InvRf "dcp.tcp.pft://192.168.0.1:1002:3002?fec=1&crc=0"
  , TF.testCase "testURIRef116" $ testURIRef AbsId "dcp.tcp.pft://192.168.0.1:1002?fec=1&crc=0"
  , TF.testCase "testURIRef117" $ testURIRef AbsId "foo://"
    -- URIs prefixed with IPv4 addresses
  , TF.testCase "testURIRef118" $ testURIRef AbsId "http://192.168.0.1.example.com/"
  , TF.testCase "testURIRef119" $ testURIRef AbsId "http://192.168.0.1.example.com./"
    -- URI prefixed with 3 octets of an IPv4 address and a subdomain part with a leading digit.
  , TF.testCase "testURIRef120" $ testURIRef AbsId "http://192.168.0.1test.example.com/"
    -- URI with IPv(future) address
  , TF.testCase "testURIRef121" $ testURIRef AbsId "http://[v9.123.abc;456.def]/"
  , TF.testCase "v.future authority" $
      (Just (URIAuth "" "[v9.123.abc;456.def]" ":42")) @=?
      (maybe Nothing uriAuthority . parseURI $ "http://[v9.123.abc;456.def]:42/")
    -- IRI test cases not currently supported
    -- URIs with non-ASCII characters (IRIs), are not supported by Network.URI, but
    -- captured here for possible future reference when IRI support may be added.
  -- , TF.testCase "testURIRef123" $ testURIRef AbsId "http://example.com/test123/䡥汬漬⁗潲汤/index.html"
  -- , TF.testCase "testURIRef124" $ testURIRef AbsId "http://example.com/test124/Москва/index.html"
  ]

-- test decomposition of URI into components
testComponent01 = testURIRefComponents
        ( Just $ URI
            { uriScheme    = "http:"
            , uriAuthority = Just (URIAuth "user:pass@" "example.org" ":99")
            , uriPath      = "/aaa/bbb"
            , uriQuery     = "?qqq"
            , uriFragment  = "#fff"
            } )
        "http://user:pass@example.org:99/aaa/bbb?qqq#fff"
testComponent02 = testURIRefComponents
        Nothing
        "http://user:pass@example.org:99aaa/bbb"
testComponent03 = testURIRefComponents
        ( Just $ URI
            { uriScheme    = "http:"
            , uriAuthority = Just (URIAuth "user:pass@" "example.org" ":99")
            , uriPath      = ""
            , uriQuery     = "?aaa/bbb"
            , uriFragment  = ""
            } )
        "http://user:pass@example.org:99?aaa/bbb"
testComponent04 = testURIRefComponents
        ( Just $ URI
            { uriScheme    = "http:"
            , uriAuthority = Just (URIAuth "user:pass@" "example.org" ":99")
            , uriPath      = ""
            , uriQuery     = ""
            , uriFragment  = "#aaa/bbb"
            } )
        "http://user:pass@example.org:99#aaa/bbb"
testComponent05 = testURIRefComponents
        ( Just $ URI
            { uriScheme    = "news:"
            , uriAuthority = Nothing
            , uriPath      = "comp.infosystems.www.servers.unix"
            , uriQuery     = ""
            , uriFragment  = ""
            } )
        "news:comp.infosystems.www.servers.unix"
testComponent06 = testURIRefComponents
        ( Just $ URI
            { uriScheme    = "mailto:"
            , uriAuthority = Nothing
            , uriPath      = "John.Doe@example.com"
            , uriQuery     = ""
            , uriFragment  = ""
            } )
        "mailto:John.Doe@example.com"
testComponent07 = testURIRefComponents
        ( Just $ URI
            { uriScheme    = "tel:"
            , uriAuthority = Nothing
            , uriPath      = "+1-816-555-1212"
            , uriQuery     = ""
            , uriFragment  = ""
            } )
        "tel:+1-816-555-1212"
-- These test cases contributed by Robert Buck (mathworks.com)
testComponent11 = testURIRefComponents
        ( Just $ URI
            { uriScheme    = "about:"
            , uriAuthority = Nothing
            , uriPath      = ""
            , uriQuery     = ""
            , uriFragment  = ""
            } )
        "about:"
testComponent12 = testURIRefComponents
        ( Just $ URI
            { uriScheme    = "file:"
            , uriAuthority = Just (URIAuth "" "windowsauth" "")
            , uriPath      = "/d$"
            , uriQuery     = ""
            , uriFragment  = ""
            } )
        "file://windowsauth/d$"

testComponentSuite = TF.testGroup "Test URIrefs"
  [ TF.testCase "testComponent01" testComponent01
  , TF.testCase "testComponent02" testComponent02
  , TF.testCase "testComponent03" testComponent03
  , TF.testCase "testComponent04" testComponent04
  , TF.testCase "testComponent05" testComponent05
  , TF.testCase "testComponent06" testComponent06
  , TF.testCase "testComponent07" testComponent07
  , TF.testCase "testComponent11" testComponent11
  , TF.testCase "testComponent12" testComponent12
  ]

-- Get reference relative to given base
--   relativeRef :: String -> String -> String
--
-- Get absolute URI given base and relative reference
--   absoluteURI :: String -> String -> String
--
-- Test cases taken from: http://www.w3.org/2000/10/swap/uripath.py
-- (Thanks, Dan Connolly)
--
-- NOTE:  absoluteURI base (relativeRef base u) is always equivalent to u.
-- cf. http://lists.w3.org/Archives/Public/uri/2003Jan/0008.html

testRelSplit :: String -> String -> String -> String -> TF.Assertion
testRelSplit label base uabs urel =
    testEq label urel (mkrel puabs pubas)
    where
        mkrel (Just u1) (Just u2) = show (u1 `relativeFrom` u2)
        mkrel Nothing   _         = "Invalid URI: "++urel
        mkrel _         Nothing   = "Invalid URI: "++uabs
        puabs = parseURIReference uabs
        pubas = parseURIReference base

testRelJoin  :: String -> String -> String -> String -> TF.Assertion
testRelJoin label base urel uabs =
    testEq label uabs (mkabs purel pubas)
    where
        mkabs (Just u1) (Just u2) = show (u1 `relativeTo` u2)
        mkabs Nothing   _         = "Invalid URI: "++urel
        mkabs _         Nothing   = "Invalid URI: "++uabs
        purel = parseURIReference urel
        pubas = parseURIReference base

testRelative :: String -> String -> String -> String -> TF.Assertion
testRelative label base uabs urel = sequence_
    [
    testRelSplit (label++"(rel)") base uabs urel,
    testRelJoin  (label++"(abs)") base urel uabs
    ]

testRelative01 = testRelative "testRelative01"
                    "foo:xyz" "bar:abc" "bar:abc"
testRelative02 = testRelative "testRelative02"
                    "http://example/x/y/z" "http://example/x/abc" "../abc"
testRelative03 = testRelative "testRelative03"
                    "http://example2/x/y/z" "http://example/x/abc" "//example/x/abc"
                    -- "http://example2/x/y/z" "http://example/x/abc" "http://example/x/abc"
testRelative04 = testRelative "testRelative04"
                    "http://ex/x/y/z" "http://ex/x/r" "../r"
testRelative05 = testRelative "testRelative05"
                    "http://ex/x/y/z" "http://ex/r" "/r"
                    -- "http://ex/x/y/z" "http://ex/r" "../../r"
testRelative06 = testRelative "testRelative06"
                    "http://ex/x/y/z" "http://ex/x/y/q/r" "q/r"
testRelative07 = testRelative "testRelative07"
                    "http://ex/x/y" "http://ex/x/q/r#s" "q/r#s"
testRelative08 = testRelative "testRelative08"
                    "http://ex/x/y" "http://ex/x/q/r#s/t" "q/r#s/t"
testRelative09 = testRelative "testRelative09"
                    "http://ex/x/y" "ftp://ex/x/q/r" "ftp://ex/x/q/r"
testRelative10 = testRelative "testRelative10"
                    -- "http://ex/x/y" "http://ex/x/y" "y"
                    "http://ex/x/y" "http://ex/x/y" ""
testRelative11 = testRelative "testRelative11"
                    -- "http://ex/x/y/" "http://ex/x/y/" "./"
                    "http://ex/x/y/" "http://ex/x/y/" ""
testRelative12 = testRelative "testRelative12"
                    -- "http://ex/x/y/pdq" "http://ex/x/y/pdq" "pdq"
                    "http://ex/x/y/pdq" "http://ex/x/y/pdq" ""
testRelative13 = testRelative "testRelative13"
                    "http://ex/x/y/" "http://ex/x/y/z/" "z/"
testRelative14 = testRelative "testRelative14"
                    -- "file:/swap/test/animal.rdf" "file:/swap/test/animal.rdf#Animal" "animal.rdf#Animal"
                    "file:/swap/test/animal.rdf" "file:/swap/test/animal.rdf#Animal" "#Animal"
testRelative15 = testRelative "testRelative15"
                    "file:/e/x/y/z" "file:/e/x/abc" "../abc"
testRelative16 = testRelative "testRelative16"
                    "file:/example2/x/y/z" "file:/example/x/abc" "/example/x/abc"
testRelative17 = testRelative "testRelative17"
                    "file:/ex/x/y/z" "file:/ex/x/r" "../r"
testRelative18 = testRelative "testRelative18"
                    "file:/ex/x/y/z" "file:/r" "/r"
testRelative19 = testRelative "testRelative19"
                    "file:/ex/x/y" "file:/ex/x/q/r" "q/r"
testRelative20 = testRelative "testRelative20"
                    "file:/ex/x/y" "file:/ex/x/q/r#s" "q/r#s"
testRelative21 = testRelative "testRelative21"
                    "file:/ex/x/y" "file:/ex/x/q/r#" "q/r#"
testRelative22 = testRelative "testRelative22"
                    "file:/ex/x/y" "file:/ex/x/q/r#s/t" "q/r#s/t"
testRelative23 = testRelative "testRelative23"
                    "file:/ex/x/y" "ftp://ex/x/q/r" "ftp://ex/x/q/r"
testRelative24 = testRelative "testRelative24"
                    -- "file:/ex/x/y" "file:/ex/x/y" "y"
                    "file:/ex/x/y" "file:/ex/x/y" ""
testRelative25 = testRelative "testRelative25"
                    -- "file:/ex/x/y/" "file:/ex/x/y/" "./"
                    "file:/ex/x/y/" "file:/ex/x/y/" ""
testRelative26 = testRelative "testRelative26"
                    -- "file:/ex/x/y/pdq" "file:/ex/x/y/pdq" "pdq"
                    "file:/ex/x/y/pdq" "file:/ex/x/y/pdq" ""
testRelative27 = testRelative "testRelative27"
                    "file:/ex/x/y/" "file:/ex/x/y/z/" "z/"
testRelative28 = testRelative "testRelative28"
                    "file:/devel/WWW/2000/10/swap/test/reluri-1.n3"
                    "file://meetings.example.com/cal#m1" "//meetings.example.com/cal#m1"
                    -- "file:/devel/WWW/2000/10/swap/test/reluri-1.n3"
                    -- "file://meetings.example.com/cal#m1" "file://meetings.example.com/cal#m1"
testRelative29 = testRelative "testRelative29"
                    "file:/home/connolly/w3ccvs/WWW/2000/10/swap/test/reluri-1.n3"
                    "file://meetings.example.com/cal#m1" "//meetings.example.com/cal#m1"
                    -- "file:/home/connolly/w3ccvs/WWW/2000/10/swap/test/reluri-1.n3"
                    -- "file://meetings.example.com/cal#m1" "file://meetings.example.com/cal#m1"
testRelative30 = testRelative "testRelative30"
                    "file:/some/dir/foo" "file:/some/dir/#blort" "./#blort"
testRelative31 = testRelative "testRelative31"
                    "file:/some/dir/foo" "file:/some/dir/#" "./#"
testRelative32 = testRelative "testRelative32"
                    "http://ex/x/y" "http://ex/x/q:r" "./q:r"
                    -- see RFC2396bis, section 5       ^^
testRelative33 = testRelative "testRelative33"
                    "http://ex/x/y" "http://ex/x/p=q:r" "./p=q:r"
                    -- "http://ex/x/y" "http://ex/x/p=q:r" "p=q:r"
testRelative34 = testRelative "testRelative34"
                    "http://ex/x/y?pp/qq" "http://ex/x/y?pp/rr" "?pp/rr"
testRelative35 = testRelative "testRelative35"
                    "http://ex/x/y?pp/qq" "http://ex/x/y/z" "y/z"
testRelative36 = testRelative "testRelative36"
                    "mailto:local"
                    "mailto:local/qual@domain.org#frag"
                    "local/qual@domain.org#frag"
testRelative37 = testRelative "testRelative37"
                    "mailto:local/qual1@domain1.org"
                    "mailto:local/more/qual2@domain2.org#frag"
                    "more/qual2@domain2.org#frag"
testRelative38 = testRelative "testRelative38"
                    "http://ex/x/z?q" "http://ex/x/y?q" "y?q"
testRelative39 = testRelative "testRelative39"
                    "http://ex?p" "http://ex/x/y?q" "/x/y?q"
testRelative40 = testRelative "testRelative40"
                    "foo:a/b" "foo:a/c/d" "c/d"
testRelative41 = testRelative "testRelative41"
                    "foo:a/b" "foo:/c/d" "/c/d"
testRelative42 = testRelative "testRelative42"
                    "foo:a/b?c#d" "foo:a/b?c" ""
testRelative43 = testRelative "testRelative42"
                    "foo:a" "foo:b/c" "b/c"
testRelative44 = testRelative "testRelative44"
                    "foo:/a/y/z" "foo:/a/b/c" "../b/c"
testRelative45 = testRelJoin "testRelative45"
                    "foo:a" "./b/c" "foo:b/c"
testRelative46 = testRelJoin "testRelative46"
                    "foo:a" "/./b/c" "foo:/b/c"
testRelative47 = testRelJoin "testRelative47"
                    "foo://a//b/c" "../../d" "foo://a/d"
testRelative48 = testRelJoin "testRelative48"
                    "foo:a" "." "foo:"
testRelative49 = testRelJoin "testRelative49"
                    "foo:a" ".." "foo:"

-- add escape tests
testRelative50 = testRelative "testRelative50"
                    "http://example/x/y%2Fz" "http://example/x/abc" "abc"
testRelative51 = testRelative "testRelative51"
                    "http://example/a/x/y/z" "http://example/a/x%2Fabc" "../../x%2Fabc"
testRelative52 = testRelative "testRelative52"
                    "http://example/a/x/y%2Fz" "http://example/a/x%2Fabc" "../x%2Fabc"
testRelative53 = testRelative "testRelative53"
                    "http://example/x%2Fy/z" "http://example/x%2Fy/abc" "abc"
testRelative54 = testRelative "testRelative54"
                    "http://ex/x/y" "http://ex/x/q%3Ar" "q%3Ar"
testRelative55 = testRelative "testRelative55"
                    "http://example/x/y%2Fz" "http://example/x%2Fabc" "/x%2Fabc"
-- Apparently, TimBL prefers the following way to 41, 42 above
-- cf. http://lists.w3.org/Archives/Public/uri/2003Feb/0028.html
-- He also notes that there may be different relative functions
-- that satisfy the basic equivalence axiom:
-- cf. http://lists.w3.org/Archives/Public/uri/2003Jan/0008.html
testRelative56 = testRelative "testRelative56"
                    "http://example/x/y/z" "http://example/x%2Fabc" "/x%2Fabc"
testRelative57 = testRelative "testRelative57"
                    "http://example/x/y%2Fz" "http://example/x%2Fabc" "/x%2Fabc"

-- Other oddball tests
    -- Check segment normalization code:
testRelative60 = testRelJoin "testRelative60"
                    "ftp://example/x/y" "http://example/a/b/../../c" "http://example/c"
testRelative61 = testRelJoin "testRelative61"
                    "ftp://example/x/y" "http://example/a/b/c/../../" "http://example/a/"
testRelative62 = testRelJoin "testRelative62"
                    "ftp://example/x/y" "http://example/a/b/c/./" "http://example/a/b/c/"
testRelative63 = testRelJoin "testRelative63"
                    "ftp://example/x/y" "http://example/a/b/c/.././" "http://example/a/b/"
testRelative64 = testRelJoin "testRelative64"
                    "ftp://example/x/y" "http://example/a/b/c/d/../../../../e" "http://example/e"
testRelative65 = testRelJoin "testRelative65"
                    "ftp://example/x/y" "http://example/a/b/c/d/../.././../../e" "http://example/e"
    -- Check handling of queries and fragments with non-relative paths
testRelative70 = testRelative "testRelative70"
                    "mailto:local1@domain1?query1" "mailto:local2@domain2"
                    "local2@domain2"
testRelative71 = testRelative "testRelative71"
                    "mailto:local1@domain1" "mailto:local2@domain2?query2"
                    "local2@domain2?query2"
testRelative72 = testRelative "testRelative72"
                    "mailto:local1@domain1?query1" "mailto:local2@domain2?query2"
                    "local2@domain2?query2"
testRelative73 = testRelative "testRelative73"
                    "mailto:local@domain?query1" "mailto:local@domain?query2"
                    "?query2"
testRelative74 = testRelative "testRelative74"
                    "mailto:?query1" "mailto:local@domain?query2"
                    "local@domain?query2"
testRelative75 = testRelative "testRelative75"
                    "mailto:local@domain?query1" "mailto:local@domain?query2"
                    "?query2"
testRelative76 = testRelative "testRelative76"
                    "foo:bar" "http://example/a/b?c/../d"  "http://example/a/b?c/../d"
testRelative77 = testRelative "testRelative77"
                    "foo:bar" "http://example/a/b#c/../d"  "http://example/a/b#c/../d"
{- These (78-81) are some awkward test cases thrown up by a question on the URI list:
     http://lists.w3.org/Archives/Public/uri/2005Jul/0013
   Note that RFC 3986 discards path segments after the final '/' only when merging two
   paths - otherwise the final segment in the base URI is maintained.  This leads to
   difficulty in constructing a reversible relativeTo/relativeFrom pair of functions.
-}
testRelative78 = testRelative "testRelative78"
                    "http://www.example.com/data/limit/.." "http://www.example.com/data/limit/test.xml"
                    "test.xml"
testRelative79 = testRelative "testRelative79"
                    "file:/some/dir/foo" "file:/some/dir/#blort" "./#blort"
testRelative80 = testRelative "testRelative80"
                    "file:/some/dir/foo" "file:/some/dir/#" "./#"
testRelative81 = testRelative "testRelative81"
                    "file:/some/dir/.." "file:/some/dir/#blort" "./#blort"

-- testRelative  base abs rel
-- testRelSplit  base abs rel
-- testRelJoin   base rel abs
testRelative91 = testRelSplit "testRelative91"
                    "http://example.org/base/uri" "http:this"
                    "this"
testRelative92 = testRelJoin "testRelative92"
                    "http://example.org/base/uri" "http:this"
                    "http:this"
testRelative93 = testRelJoin "testRelative93"
                    "http:base" "http:this"
                    "http:this"
testRelative94 = testRelJoin "testRelative94"
                    "f:/a" ".//g"
                    "f://g"
testRelative95 = testRelJoin "testRelative95"
                    "f://example.org/base/a" "b/c//d/e"
                    "f://example.org/base/b/c//d/e"
testRelative96 = testRelJoin "testRelative96"
                    "mid:m@example.ord/c@example.org" "m2@example.ord/c2@example.org"
                    "mid:m@example.ord/m2@example.ord/c2@example.org"
testRelative97 = testRelJoin "testRelative97"
                    "file:///C:/DEV/Haskell/lib/HXmlToolbox-3.01/examples/" "mini1.xml"
                    "file:///C:/DEV/Haskell/lib/HXmlToolbox-3.01/examples/mini1.xml"
testRelative98 = testRelative "testRelative98"
                    "foo:a/y/z" "foo:a/b/c" "../b/c"
testRelative99 = testRelJoin "testRelative99"
                    "f:/a/" "..//g"
                    "f://g"


testRelativeSuite = TF.testGroup "Test Relative URIs" testRelativeList
testRelativeList  =
  [ TF.testCase "testRelative01" testRelative01
  , TF.testCase "testRelative02" testRelative02
  , TF.testCase "testRelative03" testRelative03
  , TF.testCase "testRelative04" testRelative04
  , TF.testCase "testRelative05" testRelative05
  , TF.testCase "testRelative06" testRelative06
  , TF.testCase "testRelative07" testRelative07
  , TF.testCase "testRelative08" testRelative08
  , TF.testCase "testRelative09" testRelative09
  , TF.testCase "testRelative10" testRelative10
  , TF.testCase "testRelative11" testRelative11
  , TF.testCase "testRelative12" testRelative12
  , TF.testCase "testRelative13" testRelative13
  , TF.testCase "testRelative14" testRelative14
  , TF.testCase "testRelative15" testRelative15
  , TF.testCase "testRelative16" testRelative16
  , TF.testCase "testRelative17" testRelative17
  , TF.testCase "testRelative18" testRelative18
  , TF.testCase "testRelative19" testRelative19
  , TF.testCase "testRelative20" testRelative20
  , TF.testCase "testRelative21" testRelative21
  , TF.testCase "testRelative22" testRelative22
  , TF.testCase "testRelative23" testRelative23
  , TF.testCase "testRelative24" testRelative24
  , TF.testCase "testRelative25" testRelative25
  , TF.testCase "testRelative26" testRelative26
  , TF.testCase "testRelative27" testRelative27
  , TF.testCase "testRelative28" testRelative28
  , TF.testCase "testRelative29" testRelative29
  , TF.testCase "testRelative30" testRelative30
  , TF.testCase "testRelative31" testRelative31
  , TF.testCase "testRelative32" testRelative32
  , TF.testCase "testRelative33" testRelative33
  , TF.testCase "testRelative34" testRelative34
  , TF.testCase "testRelative35" testRelative35
  , TF.testCase "testRelative36" testRelative36
  , TF.testCase "testRelative37" testRelative37
  , TF.testCase "testRelative38" testRelative38
  , TF.testCase "testRelative39" testRelative39
  , TF.testCase "testRelative40" testRelative40
  , TF.testCase "testRelative41" testRelative41
  , TF.testCase "testRelative42" testRelative42
  , TF.testCase "testRelative43" testRelative43
  , TF.testCase "testRelative44" testRelative44
  , TF.testCase "testRelative45" testRelative45
  , TF.testCase "testRelative46" testRelative46
  , TF.testCase "testRelative47" testRelative47
  , TF.testCase "testRelative48" testRelative48
  , TF.testCase "testRelative49" testRelative49
    --
  , TF.testCase "testRelative50" testRelative50
  , TF.testCase "testRelative51" testRelative51
  , TF.testCase "testRelative52" testRelative52
  , TF.testCase "testRelative53" testRelative53
  , TF.testCase "testRelative54" testRelative54
  , TF.testCase "testRelative55" testRelative55
  , TF.testCase "testRelative56" testRelative56
  , TF.testCase "testRelative57" testRelative57
    --
  , TF.testCase "testRelative60" testRelative60
  , TF.testCase "testRelative61" testRelative61
  , TF.testCase "testRelative62" testRelative62
  , TF.testCase "testRelative63" testRelative63
  , TF.testCase "testRelative64" testRelative64
  , TF.testCase "testRelative65" testRelative65
    --
  , TF.testCase "testRelative70" testRelative70
  , TF.testCase "testRelative71" testRelative71
  , TF.testCase "testRelative72" testRelative72
  , TF.testCase "testRelative73" testRelative73
  , TF.testCase "testRelative74" testRelative74
  , TF.testCase "testRelative75" testRelative75
  , TF.testCase "testRelative76" testRelative76
  , TF.testCase "testRelative77" testRelative77
  -- Awkward cases:
  , TF.testCase "testRelative78" testRelative78
  , TF.testCase "testRelative79" testRelative79
  , TF.testCase "testRelative80" testRelative80
  , TF.testCase "testRelative81" testRelative81
    --
  -- , TF.testCase "testRelative90" testRelative90
  , TF.testCase "testRelative91" testRelative91
  , TF.testCase "testRelative92" testRelative92
  , TF.testCase "testRelative93" testRelative93
  , TF.testCase "testRelative94" testRelative94
  , TF.testCase "testRelative95" testRelative95
  , TF.testCase "testRelative96" testRelative96
  , TF.testCase "testRelative97" testRelative97
  , TF.testCase "testRelative98" testRelative98
  , TF.testCase "testRelative99" testRelative99
  ]

-- RFC2396 relative-to-absolute URI tests

rfcbase  = "http://a/b/c/d;p?q"
-- normal cases, RFC2396bis 5.4.1
testRFC01 = testRelJoin "testRFC01" rfcbase "g:h" "g:h"
testRFC02 = testRelJoin "testRFC02" rfcbase "g" "http://a/b/c/g"
testRFC03 = testRelJoin "testRFC03" rfcbase "./g" "http://a/b/c/g"
testRFC04 = testRelJoin "testRFC04" rfcbase "g/" "http://a/b/c/g/"
testRFC05 = testRelJoin "testRFC05" rfcbase "/g" "http://a/g"
testRFC06 = testRelJoin "testRFC06" rfcbase "//g" "http://g"
testRFC07 = testRelJoin "testRFC07" rfcbase "?y" "http://a/b/c/d;p?y"
testRFC08 = testRelJoin "testRFC08" rfcbase "g?y" "http://a/b/c/g?y"
testRFC09 = testRelJoin "testRFC09" rfcbase "?q#s" "http://a/b/c/d;p?q#s"
testRFC23 = testRelJoin "testRFC10" rfcbase "#s" "http://a/b/c/d;p?q#s"
testRFC10 = testRelJoin "testRFC11" rfcbase "g#s" "http://a/b/c/g#s"
testRFC11 = testRelJoin "testRFC12" rfcbase "g?y#s" "http://a/b/c/g?y#s"
testRFC12 = testRelJoin "testRFC13" rfcbase ";x" "http://a/b/c/;x"
testRFC13 = testRelJoin "testRFC14" rfcbase "g;x" "http://a/b/c/g;x"
testRFC14 = testRelJoin "testRFC15" rfcbase "g;x?y#s" "http://a/b/c/g;x?y#s"
testRFC24 = testRelJoin "testRFC16" rfcbase "" "http://a/b/c/d;p?q"
testRFC15 = testRelJoin "testRFC17" rfcbase "." "http://a/b/c/"
testRFC16 = testRelJoin "testRFC18" rfcbase "./" "http://a/b/c/"
testRFC17 = testRelJoin "testRFC19" rfcbase ".." "http://a/b/"
testRFC18 = testRelJoin "testRFC20" rfcbase "../" "http://a/b/"
testRFC19 = testRelJoin "testRFC21" rfcbase "../g" "http://a/b/g"
testRFC20 = testRelJoin "testRFC22" rfcbase "../.." "http://a/"
testRFC21 = testRelJoin "testRFC23" rfcbase "../../" "http://a/"
testRFC22 = testRelJoin "testRFC24" rfcbase "../../g" "http://a/g"
-- abnormal cases, RFC2396bis 5.4.2
testRFC31 = testRelJoin "testRFC31" rfcbase "?q" rfcbase
testRFC32 = testRelJoin "testRFC32" rfcbase "../../../g" "http://a/g"
testRFC33 = testRelJoin "testRFC33" rfcbase "../../../../g" "http://a/g"
testRFC34 = testRelJoin "testRFC34" rfcbase "/./g" "http://a/g"
testRFC35 = testRelJoin "testRFC35" rfcbase "/../g" "http://a/g"
testRFC36 = testRelJoin "testRFC36" rfcbase "g." "http://a/b/c/g."
testRFC37 = testRelJoin "testRFC37" rfcbase ".g" "http://a/b/c/.g"
testRFC38 = testRelJoin "testRFC38" rfcbase "g.." "http://a/b/c/g.."
testRFC39 = testRelJoin "testRFC39" rfcbase "..g" "http://a/b/c/..g"
testRFC40 = testRelJoin "testRFC40" rfcbase "./../g" "http://a/b/g"
testRFC41 = testRelJoin "testRFC41" rfcbase "./g/." "http://a/b/c/g/"
testRFC42 = testRelJoin "testRFC42" rfcbase "g/./h" "http://a/b/c/g/h"
testRFC43 = testRelJoin "testRFC43" rfcbase "g/../h" "http://a/b/c/h"
testRFC44 = testRelJoin "testRFC44" rfcbase "g;x=1/./y" "http://a/b/c/g;x=1/y"
testRFC45 = testRelJoin "testRFC45" rfcbase "g;x=1/../y" "http://a/b/c/y"
testRFC46 = testRelJoin "testRFC46" rfcbase "g?y/./x" "http://a/b/c/g?y/./x"
testRFC47 = testRelJoin "testRFC47" rfcbase "g?y/../x" "http://a/b/c/g?y/../x"
testRFC48 = testRelJoin "testRFC48" rfcbase "g#s/./x" "http://a/b/c/g#s/./x"
testRFC49 = testRelJoin "testRFC49" rfcbase "g#s/../x" "http://a/b/c/g#s/../x"
testRFC50 = testRelJoin "testRFC50" rfcbase "http:x" "http:x"

-- Null path tests
-- See RFC2396bis, section 5.2,
-- "If the base URI's path component is the empty string, then a single
--  slash character is copied to the buffer"
testRFC60 = testRelative "testRFC60" "http://ex"     "http://ex/x/y?q" "/x/y?q"
testRFC61 = testRelJoin  "testRFC61" "http://ex"     "x/y?q"           "http://ex/x/y?q"
testRFC62 = testRelative "testRFC62" "http://ex?p"   "http://ex/x/y?q" "/x/y?q"
testRFC63 = testRelJoin  "testRFC63" "http://ex?p"   "x/y?q"           "http://ex/x/y?q"
testRFC64 = testRelative "testRFC64" "http://ex#f"   "http://ex/x/y?q" "/x/y?q"
testRFC65 = testRelJoin  "testRFC65" "http://ex#f"   "x/y?q"           "http://ex/x/y?q"
testRFC66 = testRelative "testRFC66" "http://ex?p"   "http://ex/x/y#g" "/x/y#g"
testRFC67 = testRelJoin  "testRFC67" "http://ex?p"   "x/y#g"           "http://ex/x/y#g"
testRFC68 = testRelative "testRFC68" "http://ex"     "http://ex/"      "/"
testRFC69 = testRelJoin  "testRFC69" "http://ex"     "./"              "http://ex/"
testRFC70 = testRelative "testRFC70" "http://ex"     "http://ex/a/b"   "/a/b"
testRFC71 = testRelative "testRFC71" "http://ex/a/b" "http://ex"       "./"

testRFC2396Suite = TF.testGroup "Test RFC2396 examples" testRFC2396List
testRFC2396List  =
  [ TF.testCase "testRFC01" testRFC01
  , TF.testCase "testRFC02" testRFC02
  , TF.testCase "testRFC03" testRFC03
  , TF.testCase "testRFC04" testRFC04
  , TF.testCase "testRFC05" testRFC05
  , TF.testCase "testRFC06" testRFC06
  , TF.testCase "testRFC07" testRFC07
  , TF.testCase "testRFC08" testRFC08
  , TF.testCase "testRFC09" testRFC09
  , TF.testCase "testRFC10" testRFC10
  , TF.testCase "testRFC11" testRFC11
  , TF.testCase "testRFC12" testRFC12
  , TF.testCase "testRFC13" testRFC13
  , TF.testCase "testRFC14" testRFC14
  , TF.testCase "testRFC15" testRFC15
  , TF.testCase "testRFC16" testRFC16
  , TF.testCase "testRFC17" testRFC17
  , TF.testCase "testRFC18" testRFC18
  , TF.testCase "testRFC19" testRFC19
  , TF.testCase "testRFC20" testRFC20
  , TF.testCase "testRFC21" testRFC21
  , TF.testCase "testRFC22" testRFC22
  , TF.testCase "testRFC23" testRFC23
  , TF.testCase "testRFC24" testRFC24
    -- testRFC30,
  , TF.testCase "testRFC31" testRFC31
  , TF.testCase "testRFC32" testRFC32
  , TF.testCase "testRFC33" testRFC33
  , TF.testCase "testRFC34" testRFC34
  , TF.testCase "testRFC35" testRFC35
  , TF.testCase "testRFC36" testRFC36
  , TF.testCase "testRFC37" testRFC37
  , TF.testCase "testRFC38" testRFC38
  , TF.testCase "testRFC39" testRFC39
  , TF.testCase "testRFC40" testRFC40
  , TF.testCase "testRFC41" testRFC41
  , TF.testCase "testRFC42" testRFC42
  , TF.testCase "testRFC43" testRFC43
  , TF.testCase "testRFC44" testRFC44
  , TF.testCase "testRFC45" testRFC45
  , TF.testCase "testRFC46" testRFC46
  , TF.testCase "testRFC47" testRFC47
  , TF.testCase "testRFC48" testRFC48
  , TF.testCase "testRFC49" testRFC49
  , TF.testCase "testRFC50" testRFC50
    --
  , TF.testCase "testRFC60" testRFC60
  , TF.testCase "testRFC61" testRFC61
  , TF.testCase "testRFC62" testRFC62
  , TF.testCase "testRFC63" testRFC63
  , TF.testCase "testRFC64" testRFC64
  , TF.testCase "testRFC65" testRFC65
  , TF.testCase "testRFC66" testRFC66
  , TF.testCase "testRFC67" testRFC67
  , TF.testCase "testRFC68" testRFC68
  , TF.testCase "testRFC69" testRFC69
  , TF.testCase "testRFC70" testRFC70
  ]

-- And some other oddballs:
mailbase = "mailto:local/option@domain.org?notaquery#frag"
testMail01 = testRelJoin "testMail01"
            mailbase "more@domain"
            "mailto:local/more@domain"
testMail02 = testRelJoin "testMail02"
            mailbase "#newfrag"
            "mailto:local/option@domain.org?notaquery#newfrag"
testMail03 = testRelJoin "testMail03"
            mailbase "l1/q1@domain"
            "mailto:local/l1/q1@domain"

testMail11 = testRelJoin "testMail11"
             "mailto:local1@domain1?query1" "mailto:local2@domain2"
             "mailto:local2@domain2"
testMail12 = testRelJoin "testMail12"
             "mailto:local1@domain1" "mailto:local2@domain2?query2"
             "mailto:local2@domain2?query2"
testMail13 = testRelJoin "testMail13"
             "mailto:local1@domain1?query1" "mailto:local2@domain2?query2"
             "mailto:local2@domain2?query2"
testMail14 = testRelJoin "testMail14"
             "mailto:local@domain?query1" "mailto:local@domain?query2"
             "mailto:local@domain?query2"
testMail15 = testRelJoin "testMail15"
             "mailto:?query1" "mailto:local@domain?query2"
             "mailto:local@domain?query2"
testMail16 = testRelJoin "testMail16"
             "mailto:local@domain?query1" "?query2"
             "mailto:local@domain?query2"
testInfo17 = testRelJoin "testInfo17"
             "info:name/1234/../567" "name/9876/../543"
             "info:name/name/543"
testInfo18 = testRelJoin "testInfo18"
             "info:/name/1234/../567" "name/9876/../543"
             "info:/name/name/543"

testOddballSuite = TF.testGroup "Test oddball examples" testOddballList
testOddballList  =
  [ TF.testCase "testMail01" testMail01
  , TF.testCase "testMail02" testMail02
  , TF.testCase "testMail03" testMail03
  , TF.testCase "testMail11" testMail11
  , TF.testCase "testMail12" testMail12
  , TF.testCase "testMail13" testMail13
  , TF.testCase "testMail14" testMail14
  , TF.testCase "testMail15" testMail15
  , TF.testCase "testMail16" testMail16
  , TF.testCase "testInfo17" testInfo17
  ]

--  Normalization tests

--  Case normalization; cf. RFC2396bis section 6.2.2.1
--  NOTE:  authority case normalization is not performed
testNormalize01 = "http://EXAMPLE.com/Root/%2A?%2B#%2C" @=?
                  (normalizeCase "HTTP://EXAMPLE.com/Root/%2a?%2b#%2c")

testNormalize02 :: TF.TestTree
testNormalize02 = TF.testGroup "testNormalize02"
  [ TF.testCase "testNormalize02a" (normalizeCase "relative" @=? "relative")
  , TF.testCase "testNormalize02b" (normalizeCase "foo/bar" @=? "foo/bar")
  , TF.testCase "testNormalize02c" (normalizeCase "./a:b" @=? "./a:b")
  ]

--  Encoding normalization; cf. RFC2396bis section 6.2.2.2
testNormalize11 = "HTTP://EXAMPLE.com/Root/~Me/" @=?
                  normalizeEscape "HTTP://EXAMPLE.com/Root/%7eMe/"
testNormalize12 = "foo:%40AZ%5b%60az%7b%2f09%3a-._~" @=?
                  normalizeEscape "foo:%40%41%5a%5b%60%61%7a%7b%2f%30%39%3a%2d%2e%5f%7e"
testNormalize13 = "foo:%3a%2f%3f%23%5b%5d%40" @=?
                  normalizeEscape "foo:%3a%2f%3f%23%5b%5d%40"

--  Path segment normalization; cf. RFC2396bis section 6.2.2.4
testNormalize21 = "http://example/c" @=?
                  normalizePathSegments "http://example/a/b/../../c"
testNormalize22 = "http://example/a/" @=?
                  normalizePathSegments "http://example/a/b/c/../../"
testNormalize23 = "http://example/a/b/c/" @=?
                  normalizePathSegments "http://example/a/b/c/./"
testNormalize24 = "http://example/a/b/" @=?
                  normalizePathSegments "http://example/a/b/c/.././"
testNormalize25 = "http://example/e" @=?
                  normalizePathSegments "http://example/a/b/c/d/../../../../e"
testNormalize26 = "http://example/e" @=?
                  normalizePathSegments "http://example/a/b/c/d/../.././../../e"
testNormalize27 = "http://example/e" @=?
                  normalizePathSegments "http://example/a/b/../.././../../e"
testNormalize28 = "foo:e" @=?
                  normalizePathSegments "foo:a/b/../.././../../e"

testNormalizeSuite = TF.testGroup "testNormalizeSuite"
  [ TF.testCase "testNormalize01" testNormalize01
  , testNormalize02
  , TF.testCase "testNormalize11" testNormalize11
  , TF.testCase "testNormalize12" testNormalize12
  , TF.testCase "testNormalize13" testNormalize13
  , TF.testCase "testNormalize21" testNormalize21
  , TF.testCase "testNormalize22" testNormalize22
  , TF.testCase "testNormalize23" testNormalize23
  , TF.testCase "testNormalize24" testNormalize24
  , TF.testCase "testNormalize25" testNormalize25
  , TF.testCase "testNormalize26" testNormalize26
  , TF.testCase "testNormalize27" testNormalize27
  , TF.testCase "testNormalize28" testNormalize28
  ]

-- URI formatting (show) tests

ts02URI = URI   { uriScheme    = "http:"
                , uriAuthority = Just (URIAuth "user:pass@" "example.org" ":99")
                , uriPath      = "/aaa/bbb"
                , uriQuery     = "?ccc"
                , uriFragment  = "#ddd/eee"
                }

ts04URI = URI   { uriScheme    = "http:"
                , uriAuthority = Just (URIAuth "user:anonymous@" "example.org" ":99")
                , uriPath      = "/aaa/bbb"
                , uriQuery     = "?ccc"
                , uriFragment  = "#ddd/eee"
                }

ts02str = "http://user:...@example.org:99/aaa/bbb?ccc#ddd/eee"
ts03str = "http://user:pass@example.org:99/aaa/bbb?ccc#ddd/eee"
ts04str = "http://user:...@example.org:99/aaa/bbb?ccc#ddd/eee"

testShowURI01 = ""      @=? show nullURI
testShowURI02 = ts02str @=? show ts02URI
testShowURI03 = ts03str @=? uriToString id ts02URI ""
testShowURI04 = ts04str @=? show ts04URI

testShowURI = TF.testGroup "testShowURI"
  [ TF.testCase "testShowURI01" testShowURI01
  , TF.testCase "testShowURI02" testShowURI02
  , TF.testCase "testShowURI03" testShowURI03
  , TF.testCase "testShowURI04" testShowURI04
  ]


-- URI escaping tests

te01str = "http://example.org/az/09-_/.~:/?#[]@!$&'()*+,;="
te02str = "http://example.org/a</b>/c%/d /e"
te02esc = "http://example.org/a%3C/b%3E/c%25/d%20/e"

testEscapeURIString01 =
    te01str @=? escapeURIString isUnescapedInURI te01str

testEscapeURIString02 =
    te02esc @=? escapeURIString isUnescapedInURI te02str

testEscapeURIString03 =
    te01str @=? unEscapeString te01str

testEscapeURIString04 =
    te02str @=? unEscapeString te02esc

testEscapeURIString05 =
    "http%3A%2F%2Fexample.org%2Faz%2F09-_%2F.~%3A%2F%3F%23%5B%5D%40%21%24%26%27%28%29%2A%2B%2C%3B%3D" @=?
    escapeURIString isUnescapedInURIComponent te01str

testEscapeURIString06 =
    "hello%C3%B8%C2%A9%E6%97%A5%E6%9C%AC" @=?
    escapeURIString isUnescapedInURIComponent "helloø©日本"

-- From report by Alexander Ivanov:
-- should return " 䡥汬漬⁗潲汤", but returns "Hello, World" instead
-- print $ urlDecode $ urlEncode " 䡥汬漬⁗潲汤"
assertUnescapeEscapeInverse _lab x = x @=? (unEscapeString $ escapeURIString isUnescapedInURIComponent x)
testUnescapeEscape01 = assertUnescapeEscapeInverse "testUnescapeEscape01" " 䡥汬漬⁗潲汤"
-- should return "Москва"
-- print $ urlDecode $ urlEncode "Москва"
testUnescapeEscape02 = assertUnescapeEscapeInverse "testUnescapeEscape02" "Москва"

validUnicodePoint :: Char -> Bool
validUnicodePoint c =
  case ord c of
    a | a >= 0xFDD0 && a <= 0xFDEF -> False
    a | a .&. 0xFFFE == 0xFFFE -> False
    _ -> True

propEscapeUnEscapeLoop :: String -> Property
propEscapeUnEscapeLoop s =
  all validUnicodePoint s ==>
  s == (unEscapeString $! escaped)
        where
        escaped = escapeURIString (const False) s
        {-# NOINLINE escaped #-}

-- Test some Unicode chars high in the Basic Multilingual Plane.
propEscapeUnEscapeLoopHiChars :: Char -> Property
propEscapeUnEscapeLoopHiChars c' =
  let c = chr $ ord c' .|. 0xff00 in
  validUnicodePoint c ==>
  [c] == (unEscapeString $! escaped c)
        where
        escaped c = escapeURIString (const False) [c]
        {-# NOINLINE escaped #-}

testEscapeURIString = TF.testGroup "testEscapeURIString"
  [ TF.testCase "testEscapeURIString01" testEscapeURIString01
  , TF.testCase "testEscapeURIString02" testEscapeURIString02
  , TF.testCase "testEscapeURIString03" testEscapeURIString03
  , TF.testCase "testEscapeURIString04" testEscapeURIString04
  , TF.testCase "testEscapeURIString05" testEscapeURIString05
  , TF.testCase "testEscapeURIString06" testEscapeURIString06
  , TF.testCase "testUnescapeEscape01" testUnescapeEscape01
  , TF.testCase "testUnescapeEscape02" testUnescapeEscape02
  , TF.testProperty "propEscapeUnEscapeLoop" propEscapeUnEscapeLoop
  , TF.testProperty "propEscapeUnEscapeLoopHiChars" propEscapeUnEscapeLoopHiChars
  ]

-- URI string normalization tests

tn01str = "eXAMPLE://a/b/%7bfoo%7d"
tn01nrm = "example://a/b/%7Bfoo%7D"

tn02str = "example://a/b/%63/"
tn02nrm = "example://a/b/c/"

tn03str = "example://a/./b/../b/c/foo"
tn03nrm = "example://a/b/c/foo"

tn04str = "eXAMPLE://a/b/%7bfoo%7d"     -- From RFC2396bis, 6.2.2
tn04nrm = "example://a/b/%7Bfoo%7D"

tn06str = "file:/x/..//y"
tn06nrm = "file://y"

tn07str = "file:x/..//y/"
tn07nrm = "file:/y/"

testNormalizeURIString01 =
    tn01nrm @=? normalizeCase tn01str
testNormalizeURIString02 =
    tn02nrm @=? normalizeEscape tn02str
testNormalizeURIString03 =
    tn03nrm @=? normalizePathSegments tn03str
testNormalizeURIString04 =
    tn04nrm @=? (normalizeCase . normalizeEscape . normalizePathSegments) tn04str
testNormalizeURIString05 =
    tn04nrm @=? (normalizePathSegments . normalizeEscape . normalizeCase) tn04str
testNormalizeURIString06 =
    tn06nrm @=? normalizePathSegments tn06str
testNormalizeURIString07 =
    tn07nrm @=? normalizePathSegments tn07str

testNormalizeURIString = TF.testGroup "testNormalizeURIString"
  [ TF.testCase "testNormalizeURIString01" testNormalizeURIString01
  , TF.testCase "testNormalizeURIString02" testNormalizeURIString02
  , TF.testCase "testNormalizeURIString03" testNormalizeURIString03
  , TF.testCase "testNormalizeURIString04" testNormalizeURIString04
  , TF.testCase "testNormalizeURIString05" testNormalizeURIString05
  , TF.testCase "testNormalizeURIString06" testNormalizeURIString06
  , TF.testCase "testNormalizeURIString07" testNormalizeURIString07
  ]

-- Test strict vs non-strict relativeTo logic

trbase = fromJust $ parseURIReference "http://bar.org/"

testRelativeTo01 =
    "http://bar.org/foo" @=?
    (show $
      fromJust (parseURIReference "foo") `relativeTo` trbase)

testRelativeTo02 =
    "http:foo" @=?
    (show $
      fromJust (parseURIReference "http:foo") `relativeTo` trbase)

testRelativeTo03 =
    "http://bar.org/foo" @=?
    (show $
      fromJust (parseURIReference "http:foo") `nonStrictRelativeTo` trbase)

testRelativeTo = TF.testGroup "testRelativeTo"
  [ TF.testCase "testRelativeTo01" testRelativeTo01
  , TF.testCase "testRelativeTo02" testRelativeTo02
  , TF.testCase "testRelativeTo03" testRelativeTo03
  ]

-- Test alternative parsing functions
testAltFn01 = "Just http://a.b/c#f" @=? (show . parseURI $ "http://a.b/c#f")
testAltFn02 = "Just http://a.b/c#f" @=? (show . parseURIReference $ "http://a.b/c#f")
testAltFn03 = "Just c/d#f" @=? (show . parseRelativeReference $ "c/d#f")
testAltFn04 = "Nothing" @=? (show . parseRelativeReference $ "http://a.b/c#f")
testAltFn05 = "Just http://a.b/c" @=? (show . parseAbsoluteURI $ "http://a.b/c")
testAltFn06 = "Nothing" @=? (show . parseAbsoluteURI $ "http://a.b/c#f")
testAltFn07 = "Nothing" @=? (show . parseAbsoluteURI $ "c/d")
testAltFn08 = "Just http://a.b/c" @=? (show . parseAbsoluteURI $ "http://a.b/c")

testAltFn11 = True  @=? isURI "http://a.b/c#f"
testAltFn12 = True  @=? isURIReference "http://a.b/c#f"
testAltFn13 = True  @=? isRelativeReference "c/d#f"
testAltFn14 = False @=? isRelativeReference "http://a.b/c#f"
testAltFn15 = True  @=? isAbsoluteURI "http://a.b/c"
testAltFn16 = False @=? isAbsoluteURI "http://a.b/c#f"
testAltFn17 = False @=? isAbsoluteURI "c/d"

testAltFn = TF.testGroup "testAltFn"
  [ TF.testCase "testAltFn01" testAltFn01
  , TF.testCase "testAltFn02" testAltFn02
  , TF.testCase "testAltFn03" testAltFn03
  , TF.testCase "testAltFn04" testAltFn04
  , TF.testCase "testAltFn05" testAltFn05
  , TF.testCase "testAltFn06" testAltFn06
  , TF.testCase "testAltFn07" testAltFn07
  , TF.testCase "testAltFn08" testAltFn08
  , TF.testCase "testAltFn11" testAltFn11
  , TF.testCase "testAltFn12" testAltFn12
  , TF.testCase "testAltFn13" testAltFn13
  , TF.testCase "testAltFn14" testAltFn14
  , TF.testCase "testAltFn15" testAltFn15
  , TF.testCase "testAltFn16" testAltFn16
  , TF.testCase "testAltFn17" testAltFn17
  ]

testUriIsAbsolute :: String -> TF.Assertion
testUriIsAbsolute str =
    TF.assertBool str (uriIsAbsolute uri)
    where
    Just uri = parseURIReference str

testUriIsRelative :: String -> TF.Assertion
testUriIsRelative str =
    TF.assertBool str (uriIsRelative uri)
    where
    Just uri = parseURIReference str

testIsAbsolute = TF.testGroup "testIsAbsolute"
  [ TF.testCase "testIsAbsolute01" $ testUriIsAbsolute "http://google.com"
  , TF.testCase "testIsAbsolute02" $ testUriIsAbsolute "ftp://p.x.ca/woo?hai=a"
  , TF.testCase "testIsAbsolute03" $ testUriIsAbsolute "mailto:bob@example.com"
  ]

testIsRelative = TF.testGroup "testIsRelative"
  [ TF.testCase "testIsRelative01" $ testUriIsRelative "//google.com"
  , TF.testCase "testIsRelative02" $ testUriIsRelative "/hello"
  , TF.testCase "testIsRelative03" $ testUriIsRelative "this/is/a/path"
  , TF.testCase "testIsRelative04" $ testUriIsRelative "?what=that"
  ]

testPathSegmentsRoundTrip :: URI -> TF.Assertion
testPathSegmentsRoundTrip u =
  let segs = pathSegments u

      dropSuffix _suf []              = []
      dropSuffix suf [x] | suf == x   = []
                         | otherwise = [x]
      dropSuffix suf (x:xs)          = x : dropSuffix suf xs

      dropPrefix _pre []                 = []
      dropPrefix pre (x:xs) | pre == x   = xs
                            | otherwise = (x:xs)
      strippedUriPath = dropSuffix '/' $ dropPrefix '/' $ uriPath u
  in
     (Data.List.intercalate "/" segs @?= strippedUriPath)

assertJust _f Nothing = TF.assertFailure "URI failed to parse"
assertJust f  (Just x) = f x

testPathSegments = TF.testGroup "testPathSegments"
  [ TF.testCase "testPathSegments03" $
        assertJust testPathSegmentsRoundTrip $ parseURIReference ""
  , TF.testCase "testPathSegments04" $
        assertJust testPathSegmentsRoundTrip $ parseURIReference "/"
  , TF.testCase "testPathSegments05" $
        assertJust testPathSegmentsRoundTrip $ parseURIReference "//"
  , TF.testCase "testPathSegments06" $
        assertJust testPathSegmentsRoundTrip $ parseURIReference "foo//bar/"
  , TF.testCase "testPathSegments07" $
        assertJust testPathSegmentsRoundTrip $ parseURIReference "/foo//bar/"
  , TF.testCase "testPathSegments03" $
        assertJust testPathSegmentsRoundTrip $ parseURI "http://example.org"
  , TF.testCase "testPathSegments04" $
        assertJust testPathSegmentsRoundTrip $ parseURI "http://example.org/"
  , TF.testCase "testPathSegments05" $
        assertJust testPathSegmentsRoundTrip $ parseURI "http://example.org//"
  , TF.testCase "testPathSegments06" $
        assertJust testPathSegmentsRoundTrip $ parseURI "http://ex.ca/foo//bar/"
  , TF.testCase "testPathSegments07" $
        assertJust testPathSegmentsRoundTrip $ parseURI "http://ex.ca/foo//bar/"
  ]

testRectify = TF.testGroup "testRectify"
  [ TF.testCase "testRectify"
    ((show $ rectify $ URI { uriScheme = "http" ,
                            uriAuthority = Just (URIAuth "ezra" "www.google.com" "80") ,
                            uriPath = "/foo/bar" ,
                            uriQuery = "foo=bar&baz=quz" ,
                            uriFragment = "chap10" })
      @?=
    "http://ezra@www.google.com:80/foo/bar?foo=bar&baz=quz#chap10")
  , -- According to RFC2986, any URL without a // does not have an authority component.
    -- Therefore tag: URIs have all their content in the path component. This is supported
    -- by the urn: example in section 3. Note that tag: URIs have no leading slash on their
    -- path component.
    TF.testCase "testRectify"
    ("tag:timothy@hpl.hp.com,2001:web/externalHome"
    @=?
    (show $ rectify $ URI { uriScheme = "tag" ,
                            uriAuthority = Nothing,
                            uriPath = "timothy@hpl.hp.com,2001:web/externalHome",
                            uriQuery = "" ,
                            uriFragment = "" }))
  , TF.testCase "testRectifyAuth"
    ("//ezra@www.google.com:80"
    @=?
    ((uriAuthToString id . Just . rectifyAuth $ URIAuth "ezra" "www.google.com" "80") ""))
  ]

#if __GLASGOW_HASKELL__ >= 760
testReadUri = True @=? (isJust (readMaybe "http://a.b" :: Maybe URI))
testReadBadUri = True @=? (isNothing (readMaybe "baduri" :: Maybe URI))
testReadRoundtrip = "http://a.b" @=? (show (read "http://a.b" :: URI))

testRead = TF.testGroup "testRead" [
  TF.testCase "testReadUri" testReadUri
  , TF.testCase "testReadRoundtrip" testReadRoundtrip
  , TF.testCase "testReadBadUri" testReadBadUri
  ]
#endif

-- Full test suite
allTests = TF.testGroup "all"
  [ testURIRefSuite
  , testComponentSuite
  , testRelativeSuite
  , testRFC2396Suite
  , testOddballSuite
  , testNormalizeSuite
  , testShowURI
  , testEscapeURIString
  , testNormalizeURIString
  , testRelativeTo
  , testAltFn
  , testIsAbsolute
  , testIsRelative
  , testPathSegments
  , testRectify
#if __GLASGOW_HASKELL__ >= 760
  , testRead
#endif
  ]

main = TF.defaultMain allTests

-- runTestFile t = do
--     h <- openFile "a.tmp" WriteMode
--     _ <- runTestText (putTextToHandle h False) t
--     hClose h
-- tf = runTestFile

-- Miscellaneous values for hand-testing/debugging in Hugs:

uref = testURIRefSuite
tr01 = testRelative01
tr02 = testRelative02
tr03 = testRelative03
tr04 = testRelative04
rel  = testRelativeSuite
rfc  = testRFC2396Suite
oddb = testOddballSuite

(Just bu02) = parseURIReference "http://example/x/y/z"
(Just ou02) = parseURIReference "../abc"
(Just ru02) = parseURIReference "http://example/x/abc"
-- fileuri = testURIReference "file:///C:/DEV/Haskell/lib/HXmlToolbox-3.01/examples/"

cu02 = ou02 `relativeTo` bu02


--------------------------------------------------------------------------------
--
--  Copyright (c) 2004, G. KLYNE.  All rights reserved.
--  Distributed as free software under the following license.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions
--  are met:
--
--  - Redistributions of source code must retain the above copyright notice,
--  this list of conditions and the following disclaimer.
--
--  - Redistributions in binary form must reproduce the above copyright
--  notice, this list of conditions and the following disclaimer in the
--  documentation and/or other materials provided with the distribution.
--
--  - Neither name of the copyright holders nor the names of its
--  contributors may be used to endorse or promote products derived from
--  this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND THE CONTRIBUTORS
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
--  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
--  HOLDERS OR THE CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
--  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
--  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
--  OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
--  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
--  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
--  USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
--------------------------------------------------------------------------------
-- $Source: /srv/cvs/cvs.haskell.org/fptools/libraries/network/tests/URITest.hs,v $
-- $Author: gklyne $
