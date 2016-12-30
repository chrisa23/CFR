namespace CFR

module KuhnPoker = 
    open System
    open System.Collections.Generic
    open CFRStrategy

    type Actions = 
        | Pass = 0
        | Bet = 1

    let NumActions = 2

    let getNodeStrategy s realizationWeight =
        let mutable normalizingSum = 0.
        for a in 0..1 do
            s.Strategy.[a] <- if s.RegretSum.[a] > 0. then s.RegretSum.[a] else 0.
            normalizingSum <- normalizingSum + s.Strategy.[a]
    
        for a in 0..s.Actions-1 do
            if (normalizingSum > 0.) then
                s.Strategy.[a] <- s.Strategy.[a] / normalizingSum
            else
                s.Strategy.[a] <- 1.0 / float(s.Actions)
            
            s.StrategySum.[a] <- s.StrategySum.[a] + realizationWeight * s.Strategy.[a]

    
    let calcUtil (cards:int[]) (history:string) =
        let plays = history.Length
        let value = if history.Substring(history.Length - 2, 2) = "bb" then 2. else 1.
        
        match history.EndsWith("bp"), plays, cards.[0] > cards.[1] with
        | true, 2, _ -> [| 1.; -1.|]
        | true, 3, _ -> [| -1.; 1.|]
        | false, _, true -> [| value ; -value |]
        | false, _, false -> [| -value ; value |]  

    let rec cfr (dict:Dictionary<string, Strategy>) (cards:int[]) (history:string) p0 p1 =
        let plays = history.Length
        let player = plays % 2
        let isTerminal = history = "pp" || history = "bp" || history = "pbp" || history = "bb" || history = "pbb"
        if isTerminal then 
            let util = (calcUtil cards history)
            //printfn "%A %A %A" cards history util
            util.[player]
        else 
            let infoSet = string(cards.[player]) + history
            if not (dict.ContainsKey infoSet) then
                dict.[infoSet] <- create NumActions
            let node = dict.[infoSet]
            
            getNodeStrategy node (if player = 0 then p0 else p1)

            //printfn "%A %A" player node

            let util = Array.zeroCreate<float> NumActions
            let mutable nodeUtil = 0.
            for a in 0..1 do
                let nextHistory = history + if a = 0 then "p" else "b"
                util.[a] <- if player = 0 then 
                                0. - cfr dict cards nextHistory (p0 * node.Strategy.[a]) p1
                            else 
                                0. - cfr dict cards nextHistory p0 (p1 * node.Strategy.[a])
            
                nodeUtil <- nodeUtil + node.Strategy.[a] * util.[a];

            for a in 0..1 do
                let regret = util.[a] - nodeUtil
                node.RegretSum.[a] <- node.RegretSum.[a] + (if player = 0 then p1 else p0) * regret

            nodeUtil
      

    let swap (a: _[]) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp


    let shuffle a =
        Array.iteri (fun i _ -> swap a i (rnd.Next(i, Array.length a))) a
        

    let train iterations = 
        let dict = new Dictionary<string, Strategy>()
        let cards = [| 1; 2; 3; |]
        let mutable util = 0.
        for i in 1..iterations do
            shuffle cards
            shuffle cards
            util <- util + cfr dict cards "" 1. 1.
        dict, util

