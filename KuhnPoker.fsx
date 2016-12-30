#r "CFR/bin/Debug/CFR.dll"

open CFR.KuhnPoker
open CFR.CFRStrategy
open System

let dict, util = train 10
Console.WriteLine("Average game value: " + string(util / float(10)))
for n in dict |> Seq.sortBy (fun x -> x.Key) do
    Console.WriteLine(sprintf "%s %A" n.Key (getAverageStrategy n.Value))