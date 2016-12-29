namespace CFR.Tests

open CFR.CFRStrategy
open CFR.RockPaperScissors
open System
open Xunit
open FsUnit.Xunit
open Xunit.Abstractions    

type RockPaperScissorsTests (output:ITestOutputHelper) = 
        
    [<Fact>]
    member __.TestTrain() = 
        let s = train [| 1.; 0.; 0. |] 1000000
        let strat = getAverageStrategy s
        strat 
        |> Array.map (fun x ->  Math.Round(x,2))
        |> should equal [| 0.; 1.; 0. |] 

    [<Fact>]
    member __.TestTrain2() = 
        let s = train [| 0.; 1.; 0. |] 1000000
        let strat = getAverageStrategy s
        strat 
        |> Array.map (fun x ->  Math.Round(x,2))
        |> should equal [| 0.; 0.; 1. |] 

