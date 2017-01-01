namespace CFR

module ShovePoker = 
    open System
    open System.Collections.Generic
    open CFRStrategy
    open fspoker
    open fspoker.Eval
    open fspoker.Cards
    open fspoker.Holes
    open fspoker.Boards
    open fspoker.Equity


    type Action = 
    | Fold = 0
    | ShoveCall = 1

    let NumActions = 2

    //infoSet = Cards (169) | Position | NumShoveCall ahead 

    type Stakes = 
        {
            BigBlind: float
            SmallBlind: float
            Ante: float
            Stacks: float
        }

    let calcPayoffs (cards: Hole[]) (stacks:Stakes) (history:string) = 
        let hist = history.ToCharArray()
        let raises = hist |> Array.fold (fun x y -> if y = 'r' then x + 1 else x) 0

        let payoffs = Array.zeroCreate<float> cards.Length
        payoffs.[payoffs.Length - 1] <- - stacks.BigBlind
        payoffs.[payoffs.Length - 2] <- - stacks.SmallBlind

        for i in 0..history.Length-1 do payoffs.[i] <- payoffs.[i] - stacks.Ante
        let mutable pot = 0. - (payoffs |> Array.reduce (+))
        
        if raises = 0 
        then 
            payoffs
        else if raises = 1 
        then 
            let i = hist |> Array.findIndex (fun x -> x = 'r')
            payoffs.[i] <- payoffs.[i] + pot
            payoffs
        else 
            let mutable i = 0
            let indexedCards = cards |> Array.indexed
            let cards1 = 
                Array.map2 (fun x y -> x,y) hist indexedCards 
                |> Array.where (fun (x,y) -> x = 'r')
                |> Array.map (fun (x,y) -> y)
            
            let cards2 = cards1 |> Array.map (fun (x,y) -> y)
            for i,c  in cards1 do
                pot <- pot + stacks.Stacks - payoffs.[i]

            let equity = evalHands2 cards2 1000

            let mutable a = 0
            for i,c  in cards1 do
                payoffs.[i] <- (equity.[a] * pot) - ((1. - equity.[a]) * stacks.Stacks) - stacks.Stacks
                a <- a + 1

            payoffs

    let getPayoffs (cards: Hole[]) (stacks:Stakes) (history:string) (payoffDict:Dictionary<string,float[]>) = 
        payoffDict.Add (history, calcPayoffs cards stacks history)
        history
            
    let rec cfr (dict:Dictionary<string, Strategy>) (cards:Hole[]) (stacks:Stakes) (history:string) (fold:string) (payoffs:Dictionary<string,float[]>) : string  = 
        let player = history.Length
        let playersAhead = history.ToCharArray() |> Array.sumBy (fun x -> if x = 'r' then 1 else 0)
        let isTerminal = history.Length = cards.Length || history = fold 
        if isTerminal then (getPayoffs cards stacks history payoffs)
        else
            let infoSet =  (shortString cards.[player].Cards) + "|" + string(player) + "|" + string(playersAhead)
            if not (dict.ContainsKey infoSet) then
                dict.[infoSet] <- CFRStrategy.create NumActions
            let node = dict.[infoSet]
            getPureStrategy node
            let mutable util = Array.zeroCreate<float> NumActions
            let mutable nodeUtil = 0.
            let mutable foldHist = ""
            let mutable raiseHist = ""
            
            for a in 0..1 do
                let nextHistory = history + if a = 0 then "f" else "r"
                let h = cfr dict cards stacks nextHistory fold payoffs
                nodeUtil <- nodeUtil + node.Strategy.[a] * payoffs.[h].[player]
                if a = 0 then 
                    foldHist <- h
                else 
                    raiseHist <- h
            
            let regretF = payoffs.[foldHist].[player] - nodeUtil
            node.RegretSum.[0] <- node.RegretSum.[0] + regretF 

            let regretR = payoffs.[raiseHist].[player] - nodeUtil
            node.RegretSum.[1] <- node.RegretSum.[1] + regretR

            if node.Strategy.[0] = 1. then foldHist  else raiseHist
        

    let getHoles players = 
        let holes = Array.zeroCreate<Hole> players
        let mutable mask = 0UL
        for i in 0..players-1 do
            let hole = Holes.rndHole mask
            mask <- hole.Mask &&& mask
            holes.[i] <- hole
        holes

    let train players stakes iterations = 
        let dict = new Dictionary<string, Strategy>()
        let foldTerm = String.concat "" [ for i in 1..players-1 -> "f"]
        for i in 1..iterations do
            let holes = getHoles players
            let payoffs = new Dictionary<string,float[]>()
            cfr dict holes stakes "" foldTerm payoffs |> ignore
        dict

