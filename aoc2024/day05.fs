module aoc2024.day05

open System.Collections.Generic
open aoc2024.util

let solve() =
    let io = aocIO
    let inp = io.getInput()
    
    let mutable nums = Dictionary<int, List<int>>()
    
    (*
    let rec isLower n1 n2 =
        if nums[n1].Contains n2 then
            true
        else
            isLower nums[]
            *)
    
    let pages = List<string>()
    for l in inp do
        if l.Contains "|" then
            let spl = l.Split "|"
            let n1 = spl[0] |> int
            let n2 = spl[1] |> int
            if not(nums.ContainsKey(n1)) then
                nums[n1] <- List<int>()
            if not(nums.ContainsKey(n2)) then
                nums[n2] <- List<int>()
            nums[n1].Add n2
        else if l.Contains "," then
            pages.Add l
                
                
    (*
    for num in nums do
        for otherNums in nums do
            if num.Key <> otherNums.Key then
                if otherNums.Value.Contains num.Key then
                    num.Value.AddRange num.Value
                    *)
    
    (*
    let mutable s = 0
    for p in pages do
        let ns = p.Split "," |> Seq.map int |> Seq.toArray
        let mutable v = true
        for i in 0..(ns |> Seq.length)-2 do
            let n1 = nums[ns[i]]
            let n2 = ns[i+1]
            if not (n1.Contains(n2)) then
                v <- false
        if v then
            let mv = ns[(ns.Length/2)]
            &s += mv
            printfn $"%A{(v, p, mv)}"
            *)
    let mutable s = 0
    for p in pages do
        let ns = p.Split "," |> Seq.map int |> Seq.toArray
        let mutable v = true
        for i in 0..(ns |> Seq.length)-2 do
            let n1 = nums[ns[i]]
            let n2 = ns[i+1]
            if not (n1.Contains(n2)) then
                v <- false
        if v then
            let mv = ns[(ns.Length/2)]
            //&s += mv
            printfn $""
            //printfn $"%A{(v, p, mv)}"
        else
            let nl = List<int>()
            for i in 0..(ns |> Seq.length)-1 do
                let f = ns[i]
                if not (nl.Contains(ns[i])) then
                    if(nl |> Seq.length |> (=) 0) then
                        nl.Insert (0, f)
                    else
                        let idxf = nl.FindIndex(fun x -> nums[x].Contains f)
                        nl.Insert ((if idxf <> -1 then idxf else (nl.Count)), f)
            &s += nl[(ns.Length/2)]
                
    
    
    printfn $"%A{s}"
    //io.submitAnswer 1 ans1
    //io.submitAnswer 2 s
    0
