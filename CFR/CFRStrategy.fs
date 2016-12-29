namespace CFR

module CFRStrategy =
    open System

    let rnd = new Random()

    type Strategy = 
        {
            Actions: int
            RegretSum: float[]
            StrategySum: float[]
            Strategy: float[]
        }

    let create i =  
        {
            Actions = i
            RegretSum = Array.zeroCreate i
            StrategySum = Array.zeroCreate i
            Strategy = Array.zeroCreate i
        }
    
    let getStrategy s = 
        let mutable normalizingSum = 0.
        for a in 0..s.Actions-1 do
            s.Strategy.[a] <- if s.RegretSum.[a] > 0. then s.RegretSum.[a] else 0.
            normalizingSum <- normalizingSum + s.Strategy.[a]
    
        for a in 0..s.Actions-1 do
            if (normalizingSum > 0.) then
                s.Strategy.[a] <- s.Strategy.[a] / normalizingSum
            else
                s.Strategy.[a] <- 1.0 / float(s.Actions)

            s.StrategySum.[a] <- s.StrategySum.[a] + s.Strategy.[a]

    let getAverageStrategy  s =
        let avgStrategy = Array.zeroCreate<float> s.Actions
        let normalizingSum = s.StrategySum |> Array.reduce (+)
        for a in 0..s.Actions-1 do
            if (normalizingSum > 0.) then
                avgStrategy.[a] <- s.StrategySum.[a] / normalizingSum;
            else
                avgStrategy.[a] <- 1.0 / float(s.Actions)
        avgStrategy
    
//    let getAction (strat:float[]) =
//        let r = rnd.NextDouble()
//        let count = strat.Length
//        let mutable cumProb = 0.;
//        let mutable i = 0;
//        let mutable loop = true;
//
//        while i < count && loop do
//            cumProb <- cumProb + strat.[i]
//            if r < cumProb 
//            then loop <- false
//            else i <- i + 1
//        i

    let getAction (strat:float[]) =
        let r = rnd.NextDouble()
        let count = strat.Length

        let rec f c i (s:float[]) =
            if r < c || i = count - 1 then i
            else f (c + s.[i+1]) (i+1) s
        
        f strat.[0] 0 strat

