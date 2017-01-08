#r "CFR/bin/Release/CFR.dll"
#r "lib/fspoker.dll"

open CFR
open CFR.ShovePoker
open CFR.CFRStrategy
open System
open System.Collections.Generic

fspoker.Eval.load "X:\HandRanks.dat"

let r = new Random((int)DateTime.Now.Ticks)
let ri i = r.Next(i)

let train' = train 1000000 ri 

let run players stacks = async{
    let dict = train' players stacks 
    let filename = sprintf "X:/Poker/ShoveStrats/%i-%s.strat" players (toString stacks)
    Utils.save dict filename
    return ()
}

let engine = Utils.ThrottlingAgent(6)

let save (players, stacks, dict) = async{
    let filename = sprintf "X:\\Poker\\ShoveStrats\\%i:%s.strat" players (toString stacks)
    Utils.save dict filename
}
//
//let runAndSave = async{
//    let! result = run 
//}

//let players = 2
//let stacks = { BigBlind = 200.; SmallBlind = 100.; Ante = 50.; Stacks = 2000. }
//
//let dict = train  100000 ri players stacks //|> Async.RunSynchronously
//
//for n in dict |> Seq.sortBy (fun x -> x.Key) do
//   Console.WriteLine(sprintf "%s %A" n.Key (getPureStrategyFromSum n.Value))


//let d1 = Utils.load<Dictionary<string,Strategy>> "X:\Poker\ShoveStrats\2-2000-200-100-50.strat"

let players = [2..9]
let stacks = 
    [
        { BigBlind = 200.; SmallBlind = 100.; Ante = 25.; Stacks = 2000. }
        { BigBlind = 200.; SmallBlind = 100.; Ante = 25.; Stacks = 1000. }
        { BigBlind = 200.; SmallBlind = 100.; Ante = 25.; Stacks = 500. }
        { BigBlind = 200.; SmallBlind = 100.; Ante = 50.; Stacks = 2000. }
        { BigBlind = 200.; SmallBlind = 100.; Ante = 50.; Stacks = 1000. }
        { BigBlind = 200.; SmallBlind = 100.; Ante = 50.; Stacks = 500. }
        { BigBlind = 200.; SmallBlind = 100.; Ante = 0.; Stacks = 2000. }
        { BigBlind = 200.; SmallBlind = 100.; Ante = 0.; Stacks = 1000. }
        { BigBlind = 200.; SmallBlind = 100.; Ante = 0.; Stacks = 500. }
    ]

[ for p in players do
      for s in stacks ->
          p,s ]//run p s ]
//|> List.map (fun x -> engine.DoWork(x))

|> List.map (fun (p,s) -> 
    let dict = train  100000 ri p s
    let filename = sprintf "X:/Poker/ShoveStrats/%i-%s.strat" p (toString s)
    Utils.save dict filename)


let dict = Utils.load<Dictionary<string,Strategy>> "X:\Poker\ShoveStrats\2-2000-200-100-50.strat"
for n in dict |> Seq.sortBy (fun x -> x.Key) do
   Console.WriteLine(sprintf "%s %A" n.Key (getPureStrategyFromSum n.Value))