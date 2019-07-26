# fs-merkle

A pure [F#](https://fsharp.org) implementation of the merkle tree algorithm.

This implementation uses a recursive algorithm over byte arrays instead of rebuilding trees
using *types*.

* **N.B** : Uses BouncyCastle for the SHA3 digest.
