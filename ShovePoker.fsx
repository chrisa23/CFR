#r "CFR/bin/Release/CFR.dll"

open CFR.ShovePoker
open CFR.CFRStrategy
open System

let stacks = { BigBlind = 200.; SmallBlind = 100.; Ante = 50.; Stacks = 2000. }
let dict = train 4 stacks 10000
for n in dict |> Seq.sortBy (fun x -> x.Key) do
   Console.WriteLine(sprintf "%s %A" n.Key (getPureStrategyFromSum n.Value))