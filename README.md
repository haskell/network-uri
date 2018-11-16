The network-uri package
=======================

This package provides facilities for parsing and unparsing URIs, and creating
and resolving relative URI references, closely following the URI spec, IETF
RFC 3986 [1].

The main module in this package, `Network.URI`, was split off from the
network package in the network-2.6 release.

# Network.URI.Static

Network.URI.Static that allows you to declare static URIs in type-safe manner.

With the base module, when you declare a static URI, you need to either use `Maybe URI` or use `URI` and give up type safety.

```haskell
safeButWrappedInMaybeURI :: Maybe URI
safeButWrappedInMaybeURI = parseURI "http://www.google.com/"

directButUnsafeURI :: URI
directButUnsafeURI = fromJust $ parseURI "http://www.google.com/"
```

This library allows you to write static URIs in type-safe manner by checking URIs at compile time using template haskell.

Now, you can write the following.

```haskell
directAndSafeURI :: URI
directAndSafeURI = $$(staticURI "http://www.google.com")
```

You can even use a quasi quote if you'd like.

```haskell
directAndSafeURI :: URI
directAndSafeURI = [uri|"http://www.google.com"|]
```

These two expressions emit an error at compile time if a specified URI is malformed.
