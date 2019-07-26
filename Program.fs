// Learn more about F# at http://fsharp.org

module FSMerkle

open Hash
open System
open System.Threading

let entries = [|"Valar Morghulis"B;"Satoshi Nakamoto"B;"Craig Wright is a fraud"B;""B|]


[<EntryPoint>]
let main argv =

    let hashOfEmptyString = Hash.compute (System.Text.Encoding.ASCII.GetBytes(""))
    let hashOfMax =  Base16.decode "7fffffff"
    match hashOfMax with
    | None -> printfn "Empty"
    | Some(v) -> (printfn "%s" (Hash.toString (compute v)))

    printfn "%s" (Base16.encode (bytes hashOfEmptyString))
    entries
    |> Merkle.root
    |> bytes
    |> Base16.encode
    |> printfn "%s"

    0 // return an integer exit code
