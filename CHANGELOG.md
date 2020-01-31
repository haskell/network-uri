# network-uri-2.6.2.0 (2020-01-30)
* Merge network-uri-static (Network.URI.Static) into this package
* Add `Lens`es for the `URI` types
* Optimize `isReserved` and related character-class functions
* Add `Generic` instances for the `URI` type
* Add `Lift` instances for the `URI` type

# network-uri-2.6.2.0 (2019-??-??)
* Start to add some benchmarks for performance analysis
* Correctly parse IPv6 addresses in URIs
* Add `rectify` which normalizes a URI if it is missing certain
  separator characters required by the module, which some users find
  inconvenient.
