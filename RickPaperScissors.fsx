open System

type Actions = 
    | Rock = 0 
    | Papar = 1
    | Scissors = 2
    
let NUM_ACTIONS = 3

let actions = [|0..2|]

let random = new Random()

let getStrategy (regretSum:float[]) (strategySum:float[]) (strategy:float[]) = 
    let mutable normalizingSum = 0.
    for a in actions do
        strategy.[a] <- if regretSum.[a] > 0. then regretSum.[a] else 0.
        normalizingSum <- normalizingSum + strategy.[a]
    
    for a in actions do
        if (normalizingSum > 0.) then
            strategy.[a] <-  strategy.[a] / normalizingSum
        else
            strategy.[a] <- 1.0 / float(NUM_ACTIONS)

        strategySum.[a] <- strategySum.[a] + strategy.[a]
    //printfn "%A"  strategy

let getAction (strat:float[]) =
    let r = random.NextDouble()
    match r with
    | r when r < strat.[0] -> 0
    | r when r < (strat.[0] + strat.[1]) -> 1
    | _ -> 2

let getAverageStrategy  strategySum =
    let avgStrategy = Array.zeroCreate<float> NUM_ACTIONS
    let normalizingSum = strategySum |> Array.reduce (+)
    for a in actions do
        if (normalizingSum > 0.) then
            avgStrategy.[a] <- strategySum.[a] / normalizingSum;
        else
            avgStrategy.[a] <- 1.0 / float(NUM_ACTIONS)
    avgStrategy
    
let getUtility myAction otherAction (actionUtility:float[]) = 
    actionUtility.[otherAction] <- 0.
    actionUtility.[if otherAction = 2 then 0 else otherAction + 1] <- 1.
    actionUtility.[if otherAction = 0 then 2 else otherAction - 1] <- -1.
    actionUtility

let train oppStrategy iterations =
    let mutable regretSum = Array.zeroCreate<float> NUM_ACTIONS
    let strategySum = Array.zeroCreate<float> NUM_ACTIONS
    let actionUtility = Array.zeroCreate<float> NUM_ACTIONS
    let strategy = Array.zeroCreate<float> NUM_ACTIONS
    for i in 1..iterations do
        getStrategy regretSum strategySum strategy
        //regretSum <- Array.zeroCreate<float> NUM_ACTIONS
        let myAction = getAction(strategy)
        let otherAction = getAction(oppStrategy)
        let actionUtility = getUtility myAction otherAction actionUtility
        //printfn "%d %d %A" myAction otherAction actionUtility
        regretSum
        |> Array.iteri (fun n x -> regretSum.[n] <- regretSum.[n] + actionUtility.[n] - actionUtility.[myAction])
        //printfn "%A %A"  regretSum strategySum
    //printfn "%A %A"  regretSum strategySum
    regretSum, strategySum


let r, s = train [| 0.5; 0.25; 0.25 |] 10000000
let strat = getAverageStrategy s
