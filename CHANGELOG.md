# network-uri-2.6.3.0 (2020-02-18)
* Add official support for SafeHaskell
  NOTE: This is the first version whose SafeHaskell properties have become an
  intentional part of the API contract; previous versions were merely
  accidentally safe-inferred (or not depending on various factors; in other
  words, this was a fragile property).
* Derive `Lift` instances using `DeriveLift` extension, when available.

# network-uri-2.6.2.0 (2020-01-30)
* Mark the modules as Safe for SafeHaskell.

# network-uri-2.6.2.0 (2020-01-30)
* Merge network-uri-static (Network.URI.Static) into this
  package, which offers a way to parse URI strings at compile time.
* Add `Lens`es for the `URI` types
* Add `Generic` instances for the `URI` type
* Add `Lift` instances for the `URI` type
* Optimize `isReserved` and related character-class functions.
* Start to add some benchmarks for performance analysis
* Fix a bug: Correctly parse IPv6 addresses in URIs.
* Add `rectify` which normalizes a URI if it is missing certain
  separator characters required by the module. Some users found adding
  those characters inconvenient when building a URI from parts.
* Add `nullURIAuth` and `uriAuthToString`, paralleling `nullURI` and `uriToString`.

# network-uri-2.6.0.3
* Fix a bug with IPv4 address parsing.

# network-uri-2.6.0.2
* Implement Generic and NFData.

# network-uri-2.6.0.0
* Initial release: Module split off from `network`.
