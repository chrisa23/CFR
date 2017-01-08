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

    let toString s = sprintf "%.0f-%.0f-%.0f-%.0f" s.Stacks s.BigBlind s.SmallBlind s.Ante

    type Context =
        {
            Players: int
            Stakes: Stakes
            Strategies: Dictionary<string,Strategy>
            Payoffs: Dictionary<string,float[]>
            Cards: Hole[]
            Mask: uint64
            Fold: string
            Rnd: int -> int
        }

    let calcPayoffs (ctx:Context) (history:string) = 
        let hist = history.ToCharArray()
        let raises = hist |> Array.fold (fun x y -> if y = 'r' then x + 1 else x) 0

        let payoffs = Array.zeroCreate<float> ctx.Players
        payoffs.[payoffs.Length - 1] <- - ctx.Stakes.BigBlind
        payoffs.[payoffs.Length - 2] <- - ctx.Stakes.SmallBlind

        for i in 0..history.Length-1 do payoffs.[i] <- payoffs.[i] - ctx.Stakes.Ante
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
            let indexedCards = ctx.Cards |> Array.indexed
            let cards1 = 
                Array.map2 (fun x y -> x,y) hist indexedCards 
                |> Array.where (fun (x,y) -> x = 'r')
                |> Array.map (fun (x,y) -> y)
            
            let cards2 = cards1 |> Array.map (fun (x,y) -> y)
            for i,c  in cards1 do
                pot <- pot + ctx.Stakes.Stacks - payoffs.[i]

            let equity = evalHands2 ctx.Rnd 1000 ctx.Mask cards2 

            let mutable a = 0
            for i,c  in cards1 do
                //double check this
                payoffs.[i] <- (equity.[a] * pot) - ((1. - equity.[a]) * ctx.Stakes.Stacks) - ctx.Stakes.Stacks
                a <- a + 1

            payoffs

    let getPayoffs (ctx:Context) (history:string)  = 
        if ctx.Payoffs.ContainsKey(history) then history
        else
            ctx.Payoffs.Add (history, calcPayoffs ctx history )
            history
            
    let rec cfr (ctx:Context) (history:string) : string  = 
        let isTerminal = history.Length = ctx.Cards.Length || history = ctx.Fold 
        if isTerminal then (getPayoffs ctx history)
        else
            let player = history.Length
            //TODO:  find another way to encode players before that captures position
            //? use the history? Will be best but also increses the strat points exponentially
            let playersAhead = history.ToCharArray() |> Array.sumBy (fun x -> if x = 'r' then 1 else 0)
            let infoSet =  (shortString ctx.Cards.[player].Cards) + "|" + string(player) + "|" + string(playersAhead)
            if not (ctx.Strategies.ContainsKey infoSet) then
                ctx.Strategies.[infoSet] <- CFRStrategy.create NumActions
            let node = ctx.Strategies.[infoSet]
            getPureStrategy node
            let mutable nodeUtil = 0.
            let mutable foldHist = ""
            let mutable raiseHist = ""
            
            for a in 0..1 do
                let nextHistory = history + if a = 0 then "f" else "r"
                let h = cfr ctx nextHistory
                nodeUtil <- nodeUtil + node.Strategy.[a] * ctx.Payoffs.[h].[player]
                if a = 0 then 
                    foldHist <- h
                else 
                    raiseHist <- h
            
            let regretF = ctx.Payoffs.[foldHist].[player] - nodeUtil
            node.RegretSum.[0] <- node.RegretSum.[0] + regretF 

            let regretR = ctx.Payoffs.[raiseHist].[player] - nodeUtil
            node.RegretSum.[1] <- node.RegretSum.[1] + regretR

            if node.Strategy.[0] = 1. then foldHist  else raiseHist
        

    let getHoles (ri: int -> int) players = 
        let holes = Array.zeroCreate<Hole> players
        let mutable mask = 0UL
        for i in 0..players-1 do
            let hole = Holes.rndHole mask ri
            mask <- hole.Mask &&& mask
            holes.[i] <- hole
        mask, holes

    let train iterations (ri: int -> int) players stakes   = 
        let getHoles' = getHoles ri
        let dict = Dictionary<string, Strategy>()
        let foldTerm = String.concat "" [ for i in 1..players-1 -> "f"]
        let ctx = { Players = players; Stakes = stakes; Strategies = dict; Payoffs = null; Cards = null; Mask = 0UL; Fold = foldTerm; Rnd = ri }
        for i in 1..iterations do
            let mask, holes = getHoles' players 
            let payoffs = Dictionary<string,float[]>()
            //Repeat multiple times to allow strategy adjustment (?)
            let ctx' = { ctx with Payoffs = payoffs; Cards = holes; Mask = mask }
            for m in 1..players do  
                cfr ctx' "" |> ignore
        dict

