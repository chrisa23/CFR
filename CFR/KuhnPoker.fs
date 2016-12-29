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

            s.StrategySum.[a] <- realizationWeight + s.Strategy.[a]

    
    let rec cfr (dict:Dictionary<string, Strategy>) (cards:int[]) (history:string) p0 p1 =
        

        let calcUtil (dict:Dictionary<string, Strategy>) (cards:int[]) (history:string) p0 p1  = 
            let plays = history.Length
            let player = plays % 2
            let infoSet = string(cards.[player]) + history
            if not (dict.ContainsKey infoSet) then
                dict.[infoSet] <- create NumActions
            let node = dict.[infoSet]
        
            getNodeStrategy node (if player = 0 then p0 else p1)
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

        let plays = history.Length
        let player = plays % 2
        let opponent = 1 - player
        let secondRound = plays > 1

        if secondRound then 
            let terminalPass = history.[plays - 1] = 'p'
            let doubleBet = history.Substring(plays - 2, 2) = "bb"
            let isPlayerCardHigher = cards.[player] > cards.[opponent]
            match  terminalPass, history = "pp", isPlayerCardHigher, doubleBet with
            | true, true, true, _ -> 1.
            | true, true, false, _ -> -1.
            | true, false, _, _ -> 1.
            | false, _, true, true -> 2.
            | false, _, false, true -> -2.
            | _ -> calcUtil dict cards history p0 p1

        else calcUtil dict cards history p0 p1
                   

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
            util <- util + cfr dict cards "" 1. 1.
        dict, util
