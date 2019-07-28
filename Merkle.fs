namespace Merkle

module Tree =
    open System
    open Hash

    let emptyHash = Hash.compute (Text.Encoding.ASCII.GetBytes "")
    let leafPrefix = Hash [| 0x00uy |]
    let interiorPrefix = Hash [| 0x01uy |]

    type AuditHash =
        { Value : byte []
          RightOp : bool }

    let compAuditHash x =
        { Value = (Hash.compute x) |> bytes
          RightOp = false }

    let private prevPowOf2 (x : int) =
        if x &&& (x - 1) = 0 then x / 2
        else
            let exp = int32 (Math.Log(float x, 2.))
            1 <<< exp

    let rec root (items : byte [] []) =
        match items.Length with
        | 0 -> emptyHash
        | 1 ->
            Hash.computeMultiple (Seq.ofList [ Hash.bytes leafPrefix
                                               items.[0] ])
        | n ->
            let k = prevPowOf2 n
            let left = root items.[..(k - 1)]
            let right = root items.[k..]

            let h =
                Hash.computeMultiple (Seq.ofList [ Hash.bytes interiorPrefix
                                                   Hash.bytes left
                                                   Hash.bytes right ])
            h

    let rec proof (items : byte [] []) (index : int) =
        match items.Length with
        | 0 -> None
        | 1 -> Some(List.empty<AuditHash>)
        | n ->
            match index with
            | x ->
                if x < 0 || x > n then None
                else
                    let k = prevPowOf2 n
                    if x < k then
                        let leftPath =
                            (proof items.[..(k - 1)] x).Value
                            @ [ { Value = bytes (root items.[k..])
                                  RightOp = true } ]
                        Some(leftPath)
                    else
                        let rightPath =
                            (proof items.[k..] (x - k)).Value
                            @ [ { Value = bytes (root items.[..(k - 1)])
                                  RightOp = false } ]
                        Some(rightPath)

    let rec verify (items : byte [] []) (index : int)
            (auditpath : AuditHash list) =
        // we add a leaf prefix to the item to prove and hash it
        let mutable hash =
            compute (Array.append (bytes leafPrefix) items.[index])
        for h in auditpath do
            let proof = h.Value
            let rightop = h.RightOp
            if rightop then
                let concatright = Array.append (bytes hash) proof
                hash <- compute
                            (Array.append (bytes interiorPrefix) concatright)
            else
                let concatleft = Array.append proof (bytes hash)
                hash <- compute (Array.append (bytes interiorPrefix) concatleft)
        hash = (root items)
