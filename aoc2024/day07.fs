module aoc2024.day07
open System
open aoc2024.util

let isValid target (ns: 'a list) operators =
    let rec loop acc = function
        | [] -> acc = target
        | h::t -> operators |> Seq.tryFind(fun op -> (loop (op acc h) t)) |> Option.isSome
    loop ns[0] ns[1..]
    
let solve() =
    let io = aocIO
    let inp = io.getInput()
    
    let operatorsPt1 = [(+); (*);]
    let operatorsPt2 = [(+); (*); (fun a b -> Int64.Parse(a.ToString()+b.ToString()))]
    
    let calculate operators =
        inp |> Seq.map(fun line ->
            let nums = line |> String.extractAllNumsU |> Seq.map int64 |> Seq.toList
            (nums[0], nums[1..])
        )
        |> Seq.where(fun (target, nums) -> isValid target nums operators)
        |> Seq.sumBy fst
    
    let a1, a2 = (calculate operatorsPt1, calculate operatorsPt2)
        
    printfn $"%A{a1}"
    printfn $"%A{a2}"
    0
   