// Learn more about F# at http://fsharp.org

module FSMerkle

open Hash
open System
open System.Threading
open Merkle
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
    |> Tree.root
    |> bytes
    |> Base16.encode
    |> printfn "%s"

    let ap = Tree.proof entries 0

    let getAndPrintVal (a:Tree.AuditHash) = printfn "%s" (Base16.encode (a.Value))

    List.iter getAndPrintVal ap.Value

    if Tree.verify entries 0 ap.Value then printfn "cool" else printfn "not cool"

    0 // return an integer exit code
