
namespace CFR.Tests

module CFRStrategyTests = 
    open CFR.CFRStrategy
    open System
    open Xunit
    open FsUnit.Xunit

    [<Fact>]
    let TestStrategyDistribution() = 
        let strat = [| 0.3333; 0.3333; 0.3334 |]
        let actions = Array.zeroCreate<float> 3
        for i in 1..1000000 do
            let action = getAction strat
            actions.[action] <- actions.[action] + 1.
        let sum = actions |> Array.reduce (+)
        let normalized = actions |> Array.map (fun x -> Math.Round(x / sum, 2))
        normalized |> should equal [| 0.33; 0.33; 0.33 |]

