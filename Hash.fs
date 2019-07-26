module Hash

open Org.BouncyCastle.Crypto.Digests

[<Literal>]
let Length = 32

type Hash =
    | Hash of byte []

    override x.ToString() =
        let (Hash bytes) = x
        Base16.encode bytes

    member x.AsString = x.ToString()
let zero = Hash(Array.zeroCreate Length)

let compute bytes =
    let hash = Array.zeroCreate Length
    let sha3 = Sha3Digest(256)
    sha3.BlockUpdate(bytes, 0, Array.length bytes)
    sha3.DoFinal(hash, 0) |> ignore
    Hash hash

let computeOfHash (Hash bytes) = compute bytes


let computeMultiple (bytes: byte array seq) =
    let hash = Array.zeroCreate Length
    let sha3 = Sha3Digest(256)

    Seq.iter (fun bytes -> sha3.BlockUpdate(bytes,0,Array.length bytes)) bytes
    sha3.DoFinal(hash, 0) |> ignore
    Hash hash

let bytes (Hash hash) = hash

let joinHashes : Hash seq -> _ =
    Seq.map bytes
    >> computeMultiple

let fromBytes bytes =
    match Array.length bytes with
    | Length -> Some (Hash bytes)
    | _ -> None

let toString (h:Hash) =
    h.ToString()

let fromString encoded =
    match Base16.decode encoded with
    | Some decoded when Array.length decoded = Length
        -> Hash decoded |> Ok
    | _ -> Error "Could not decode hash"

let isValid (Hash hash) =
    Array.length hash = Length


