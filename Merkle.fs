module Merkle

open System
open Hash

let emptyHash = Hash.compute (Text.Encoding.ASCII.GetBytes "")
let leafPrefix = Hash [|0x00uy|]
let interiorPrefix = Hash [|0x01uy|]


type AuditHash =
    {Value : byte [];
    RightOp : bool}


let private prevPowOf2 (x:int) =
    if x &&& (x-1) = 0 then x/2
    else
        let exp = int32 (Math.Log (float x,2.) )
        1 <<< exp

let rec root (items : byte [][]) =
    match items.Length with
    | 0 -> emptyHash
    | 1 -> Hash.computeMultiple (Seq.ofList [Hash.bytes leafPrefix;items.[0]])
    | x ->
        let k = prevPowOf2 x
        let left = root items.[..(k-1)]
        let right = root items.[k..]

        let h =  Hash.computeMultiple (Seq.ofList [Hash.bytes interiorPrefix; Hash.bytes left;Hash.bytes right])
        h

