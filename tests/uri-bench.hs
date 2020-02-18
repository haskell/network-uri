{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE BangPatterns #-}
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
-- Performance benchmarks for the network-uri package.
--
--------------------------------------------------------------------------------

module Main where

import Network.URI
    ( parseURI
    , parseURIReference
    , pathSegments
    , relativeFrom
    , relativeTo
    , isReserved
    )

import Criterion.Main
import Control.DeepSeq

main = defaultMain [
    let Just !u = force (parseURI "http://ezample.org/foo/bar/baz//wimple/dimple/simple") in
    bgroup "pathSegments" [
       bench "head" $ nf head (pathSegments u)
     , bench "tail" $ nf tail (pathSegments u)
    ]
  , bgroup "relativeFrom" [
      let Just !u1 = force (parseURI "http://ex.it/foo/bar/baz/bop") in
      let Just !u2 = force (parseURI "http://ex.it/foo/bar/baz/bap") in
      bench "same 4" $ nf (relativeFrom u1) u2
    , let Just !u1 = force (parseURI "http://ex.it/foo/bar/biz/bop") in
      let Just !u2 = force (parseURI "http://ex.it/foo/bar/baz/bap") in
      bench "different 4" $ nf (relativeFrom u1) u2
    ]
  , bgroup "relativeTo" [
      let Just !u1 = force (parseURIReference "../../biz/../biz/./bop") in
      let Just !u2 = force (parseURI "http://ex.it/foo/bar/baz/bap") in
      bench "dots and double dots" $ nf (relativeTo u1) u2
    ]
  , -- Prompted by https://github.com/haskell/network-uri/pull/46
    bgroup "isReserved" [
      bench "isReserved a" $ nf isReserved 'a'
      , bench "isReserved :" $ nf isReserved ':'
    ]
  ,
    bench "parseURI" $
      nf parseURI "http://foo@bar.quix.gov/flip/flop?a=b&c=d"
  ]
