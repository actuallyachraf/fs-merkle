# fs-merkle

A pure [F#](https://fsharp.org) implementation of the merkle tree algorithm.

- [**Features**]
    - Generating and Verifying Audit Paths
    - Efficient Sparse Merkle Trees implementation
    - Follows [RFC](https://tools.ietf.org/html/rfc6962#page-4)


This implementation uses a recursive algorithm over byte arrays instead of rebuilding trees
using *types*.


* **N.B** : Uses BouncyCastle for the SHA3 digest.
