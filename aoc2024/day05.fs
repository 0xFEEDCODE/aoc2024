module aoc2024.day05

open System.Collections.Generic
open aoc2024.util

let compareByOrder (orderTable: Dictionary<'a, List<'a>>) a b =
    if (orderTable.ContainsKey a && orderTable[a].Contains(b)) then -1 else 1

let solve() =
    let io = aocIO
    let inp = io.getInput()
    
    let mutable orderTable = Dictionary<int, List<int>>()
    
    let orderComparer = compareByOrder orderTable
    
    let pages = List<int array>()
    
    inp |> Seq.iter(fun line ->
        if line.Contains "|" then
            let nums = line |> String.extractAllNums
            let n1, n2 = (nums[0], nums[1])
            nums |> Seq.iter(fun n -> if not(orderTable.ContainsKey(n)) then orderTable[n] <- List<int>())
            orderTable[n1].Add n2
        else if line.Contains "," then
            pages.Add (line |> String.extractAllNums))
                
    let ans1, ans2 =
            pages |> Seq.fold(fun (ans1, ans2) page ->
                let isSorted = page |> Seq.pairwise |> Seq.forall(fun (n1, n2) -> (orderComparer n1 n2) = -1)
                if isSorted then
                    (ans1 + page[(page |> Seq.length)/2], ans2)
                else
                    let sorted = page |> Seq.sortWith(orderComparer) |> Seq.toArray
                    (ans1, ans2 + sorted[(sorted |> Seq.length)/2]))
                (0, 0)
    
    printfn $"%A{ans1}"
    printfn $"%A{ans2}"
    0
