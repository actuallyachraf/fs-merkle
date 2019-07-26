
module Base16

open System

let [<Literal>] Charset = "0123456789abcdef"

let internal charsetRev = [|
    255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy;
    255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy;
    255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy;
    0uy;   1uy;   2uy;   3uy;   4uy;   5uy;   6uy;   7uy;   8uy;   9uy;   255uy; 255uy; 255uy; 255uy; 255uy; 255uy;
    255uy; 10uy;  11uy;  12uy;  13uy;  14uy;  15uy;  255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy;
    255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy;
    255uy; 10uy;  11uy;  12uy;  13uy;  14uy;  15uy;  255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy;
    255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy
|]


let convertBits (data: array<byte>) (fromBits:int) (toBits:int) (maybePad:bool) : option< array<byte> > =

    let mutable value = 0ul
    let mutable bits = 0
    let mutable outputRev = []

    let maxv = ((1ul) <<< toBits) - 1ul;

    for b in data do
        value <- (value <<< fromBits) ||| (uint32 b)
        bits  <- bits + fromBits

        while bits >= toBits do
            bits <- bits - toBits;
            outputRev <- byte ((value >>> bits) &&& maxv) :: outputRev

    if maybePad then
        if bits <> 0 then
            outputRev <- byte ((value <<< (toBits - bits)) &&& maxv) :: outputRev

        List.rev outputRev |> List.toArray |> Some

    elif ((value <<< (toBits - bits)) &&& maxv) <> 0ul || bits >= fromBits then
        None
    else
        List.rev outputRev |> List.toArray |> Some

let encode (data:array<byte>) =
    match convertBits data 8 4 false with
    | None -> failwith "failed to converts bits"
    | Some data ->
        Array.map (fun b -> Charset.[int b]) data
        |> System.String

let decode: string -> option<byte[]> =
    Seq.map (fun c -> byte (charsetRev.[int c]))
    >> Array.ofSeq
    >> (fun data -> convertBits data 4 8 false)
    >> function
       | None -> failwith "failed to converts bits"
       | Some data -> Some data
