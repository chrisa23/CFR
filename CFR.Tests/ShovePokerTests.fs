namespace CFR.Tests
open Xunit.Abstractions
open Xunit
open FsUnit.Xunit
open CFR.CFRStrategy
open CFR.ShovePoker
open System
open fspoker.Holes

type ShovePokerTests (output:ITestOutputHelper) =
        
    [<Fact>]
    member __.TestPayoffs1() = 
        let history = "ff"
        let stacks = { BigBlind = 200.; SmallBlind = 100.; Ante = 50.; Stacks = 2000. }
        let cards = getHoles 2
        let payoffs = calcPayoffs cards stacks history
        payoffs |> should equal [| -150.; -250. |]

    [<Fact>]
    member __.TestPayoffs2() = 
        let history = "rf"
        let stacks = { BigBlind = 200.; SmallBlind = 100.; Ante = 50.; Stacks = 2000. }
        let cards = getHoles 2
        let payoffs = calcPayoffs cards stacks history
        payoffs |> should equal [| 250.; -250. |]

    [<Fact>]
    member __.TesttRAIN1() = 
        let stacks = { BigBlind = 200.; SmallBlind = 100.; Ante = 50.; Stacks = 2000. }
        let dict = train 2 stacks 1
        for n in dict |> Seq.sortBy (fun x -> x.Key) do
    
            if not (n.Key.Contains("1|0")) then
                Console.WriteLine(sprintf "%s %A" n.Key (getPureStrategyFromSum n.Value))