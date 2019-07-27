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

    let emptyAuditHash = {Value = Array.zeroCreate 32;RightOp=false}


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
        | x ->
            let k = prevPowOf2 x
            let left = root items.[..(k - 1)]
            let right = root items.[k..]

            let h =
                Hash.computeMultiple (Seq.ofList [ Hash.bytes interiorPrefix
                                                   Hash.bytes left
                                                   Hash.bytes right ])
            h

    let rec proof (items : byte [] []) (index : int) =
        match items.Length with
        | 1 -> Some([ {Value = (compute items.[index] |> bytes); RightOp = false}])
        | 0 -> None
        | _ ->
            match index with
            | x ->
                if x < 0 || x > items.Length then None
                else
                    let k = prevPowOf2 items.Length
                    let mutable left = items.[..(k - 1)]
                    let mutable right = items.[k..]
                    let mutable rightOp = true
                    let mutable i = index
                    match (i >= k) with
                    | true ->
                        i <- i - k
                        let mutable tmp = left
                        left <- right
                        right <- tmp
                        rightOp <- false
                    | false -> ()
                    let res = proof left i
                    match res with
                    | None -> None
                    | Some(x) ->
                        let v = root right

                        let app =
                            [ { Value = bytes v
                                RightOp = rightOp } ]

                        let ret = x @ app
                        Some(ret)

    let rec verify (items : byte [] []) (index : int)
            (auditpath : AuditHash list) =
        // we add a leaf prefix to the item to prove and hash it
        let mutable hash =
            compute (Array.append (bytes leafPrefix) items.[index])
        // the first element in the audit path is the hash of the item to prove we skip it
        for h in auditpath.[1..] do
                let proof = h.Value
                let rightop = h.RightOp
                if rightop then
                    let concatright = Array.append (bytes hash) proof
                    hash <- compute
                                (Array.append (bytes interiorPrefix) concatright)
                else
                    let concatleft = Array.append proof (bytes hash)
                    hash <- compute
                                (Array.append (bytes interiorPrefix) concatleft)
        hash = (root items)