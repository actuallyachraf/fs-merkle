module Tests

open System
open NUnit.Framework
open Merkle



let cxt = "tests"B
let serializer (value:int) = System.BitConverter.GetBytes value

[<Test>]
let ``verify key doest exist when tree empty``() =
    let tree = Sparse.create cxt serializer

    let key = Hash.compute "0"B

    let auditPath = Sparse.createAuditPath key tree

    Sparse.verifyEmpty tree tree.root auditPath key |> Assert.IsTrue
    Sparse.verifyValue tree tree.root auditPath key 0 |> Assert.IsFalse

[<Test>]
let ``add key and verify``() =
    let key = Hash.compute "0"B

    let tree =
        Sparse.create cxt serializer
        |> Sparse.add key 0

    let auditPath = Sparse.createAuditPath key tree

    Sparse.verifyValue tree tree.root auditPath key 0 |> Assert.IsTrue
    Sparse.verifyValue tree tree.root auditPath key 1 |> Assert.IsFalse
    Sparse.verifyEmpty tree tree.root auditPath key |> Assert.IsFalse

[<Test>]
let ``remove key and verify``() =
    let key = Hash.compute "0"B

    let tree =
        Sparse.create cxt serializer
        |> Sparse.add key 0
        |> Sparse.remove key

    let auditPath = Sparse.createAuditPath key tree

    Sparse.verifyEmpty tree tree.root auditPath key |> Assert.IsTrue
    Sparse.verifyValue tree tree.root auditPath key 1 |> Assert.IsFalse
    Sparse.verifyValue tree tree.root auditPath key 0 |> Assert.IsFalse

[<Test>]
let ``creating a lot of keys``() =
    let createKey (i:int) = (Hash.compute (BitConverter.GetBytes i)),i

    let keys =  seq {1..1000} |> Seq.map createKey

    let tree =
        keys
        |> Seq.map (fun (key,value) -> key, Sparse.Value value)
        |> Seq.toArray
        |> Sparse.updateMultiple (Sparse.create cxt serializer)

    // validating all keys are present
    keys
    |> Seq.iter (fun (key,value) ->
        let auditPath = Sparse.createAuditPath key tree

        Sparse.verifyValue tree tree.root auditPath key value |> Assert.IsTrue
        Sparse.verifyEmpty tree tree.root auditPath key |> Assert.IsFalse)

    // validating other keys are not there
    seq {1010..2000}
    |> Seq.map (fun i -> (Hash.compute (BitConverter.GetBytes i)),i)
    |> Seq.iter (fun (key,value) ->
        let auditPath = Sparse.createAuditPath key tree

        Sparse.verifyValue tree tree.root auditPath key value |> Assert.IsFalse
        Sparse.verifyEmpty tree tree.root auditPath key |> Assert.IsTrue)

    // removing some keys
    let removingKeys = seq {76..100} |> Seq.map createKey
    let leftKeys = seq {1..75} |> Seq.map createKey

    let tree' =
        removingKeys
        |> Seq.map (fun (key,_) -> key, Sparse.Empty)
        |> Seq.toArray
        |> Sparse.updateMultiple tree

    leftKeys
        |> Seq.iter (fun (key,value) ->
            let auditPath = Sparse.createAuditPath key tree'

            Sparse.verifyValue tree' tree'.root auditPath key value |> Assert.IsTrue
            Sparse.verifyEmpty tree' tree'.root auditPath key |> Assert.IsFalse)

    removingKeys
    |> Seq.iter (fun (key,value) ->
        let auditPath = Sparse.createAuditPath key tree'

        Sparse.verifyValue tree' tree'.root auditPath key value |> Assert.IsFalse
        Sparse.verifyEmpty tree' tree'.root auditPath key |> Assert.IsTrue)

[<Test>]
let ``update root with auditpath only``() =
    let tree = Sparse.create cxt serializer

    let key = Hash.compute "0"B

    let auditPath = Sparse.createAuditPath key tree

    Sparse.verifyEmpty tree tree.root auditPath key |> Assert.IsTrue

    let root = Sparse.addToRoot tree auditPath key 0 tree.root

    Sparse.verifyValue tree root auditPath key 0 |> Assert.IsTrue

    let tree' = Sparse.add key 0 tree

    Assert.AreEqual (root,tree'.root)

