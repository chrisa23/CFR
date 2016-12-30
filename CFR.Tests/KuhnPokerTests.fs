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
        let dict, util = train 100000
        output.WriteLine("Average game value: " + string(util / float(100000)))
        output.WriteLine(sprintf "%A" dict)
        for n in dict |> Seq.sortBy (fun x -> x.Key) do
            output.WriteLine(sprintf "%s %A" n.Key (getAverageStrategy n.Value |> Array.map (fun x -> Math.Round(x,2))))


