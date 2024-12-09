module aoc2024.day07
open System
open aoc2024.util

let isValid target (xs: 'a list) operators =
    let rec loop acc = function
        | [] -> acc = target
        | head::tail -> operators |> Seq.tryFind(fun op -> (loop (op acc head) tail)) |> Option.isSome
    loop xs[0] xs[1..]
    
let solve() =
    let io = aocIO
    let inp = io.getInput()
    
    let operatorsPt1 = [(+); (*);]
    let operatorsPt2 = [(+); (*); (fun a b -> Int64.Parse(a.ToString()+b.ToString()))]
    
    let calculate operators =
        inp |> Seq.map(fun line ->
            let nums = line |> String.extractAllNumsBig |> Seq.map int64 |> Seq.toList
            (nums[0], nums[1..])
        )
        |> Seq.where(fun (target, nums) -> isValid target nums operators)
        |> Seq.sumBy fst
    
    let a1, a2 = (calculate operatorsPt1, calculate operatorsPt2)
    
    let mutable l = list.Empty
    l <- 5 :: l
    l <- 10 :: l
    l <- l @ l
    l <- l @ [1;2;3]
    printfn $"%A{l}"
    
    printfn $"%A{a1}"
    printfn $"%A{a2}"
    0
   
   
