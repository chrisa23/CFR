namespace CFR.Tests
open Xunit.Abstractions
open Xunit
open FsUnit.Xunit
open CFR.CFRStrategy
open CFR.KuhnPoker
open System

type KuhnPokerTests (output:ITestOutputHelper) =
        
    [<Fact>]
    member __.TestTrain() = 
        let dict, util = train 1000000
        output.WriteLine("Average game value: " + string(util / float(1000000)))
        for n in dict |> Seq.sortBy (fun x -> x.Key) do
            output.WriteLine(sprintf "%s %A" n.Key (getAverageStrategy n.Value))


