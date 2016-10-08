# Authentication Service

## Description

Experimental authentication service written in Haskell. Registration and
authentication by name and password.

## Next

Incomplete list of things on my mind:

- handle error on inserting duplicate name,
- translate authentication errors to HTTP responses,
- swap PSS for Ed25519 signatures,
- key management,
- automatic tests,
- support for authorization schemes,
- harden against timing attacks.
