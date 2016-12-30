namespace CFR

module RockPaperScissors =
    open CFRStrategy

    type Actions = 
        | Rock = 0 
        | Papar = 1
        | Scissors = 2
    
    let NUM_ACTIONS = 3

    let getUtility myAction otherAction (actionUtility:float[]) = 
        actionUtility.[otherAction] <- 0.
        actionUtility.[if otherAction = 2 then 0 else otherAction + 1] <- 1.
        actionUtility.[if otherAction = 0 then 2 else otherAction - 1] <- -1.
        actionUtility

    let train oppStrategy iterations =
        
        let strat = create NUM_ACTIONS
        
        let actionUtility = Array.zeroCreate<float> NUM_ACTIONS
        
        for i in 1..iterations do
            getStrategy strat
            let myAction = getAction strat.Strategy
            let otherAction = getAction oppStrategy
            let actionUtility = getUtility myAction otherAction actionUtility
            
            for n in 0..2 do
                strat.RegretSum.[n] <- strat.RegretSum.[n] + actionUtility.[n] - actionUtility.[myAction]

        strat

