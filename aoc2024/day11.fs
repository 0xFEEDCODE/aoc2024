module aoc2024.day11

open System
open System.Text
open Microsoft.FSharp.Collections
open aoc2024.util

(*
let solve () =
    let io = aocIO
    let mutable stones = io.getInput() |> Seq.head |> String.extractAllNumsUint64 |> Seq.map string
    
    let p s =
        if s = "0" then
            seq {"1"}
        else if s.Length % 2 = 0 then
            let l = s[..(s.Length / 2) - 1]
            let h =
                let mutable v = s[(s.Length/2)..]
                if (v |> Seq.head = '0' && v |> Seq.last = '0' && v |> Seq.forall (fun x -> x = '0')) then
                    v <- "0"
                else
                    v <- (v.TrimStart '0')
                v
            seq {l; h}
        else
            let v = (UInt64.Parse(s) * 2024UL).ToString()
            seq {v}
    
    let proc () =
        stones <- stones |> PSeq.collect p
        
        
    let mutable a1 = 0
    let mutable a2 = 0
    
    let pr() = 
        for s in stones do
            printf $" %A{s}"
        printfn ""
    
    for i in 0..(50)-1 do

        proc()

    let st = stones |> Seq.length
    printfn $"%A{st}"*)


let solve () =
    let io = aocIO
    let mutable sb = io.getInput() |> Seq.head |> String.extractAllNumsUint64 |> Seq.map string |> String.concat " "
    
    let mutable nnsb = StringBuilder()
    let p() =
        let mutable i = 0
        nnsb <- nnsb.Clear()
        while(i <= sb.Length-1) do
            if i < sb.Length-2 && sb[i] = '0' && sb[i+1] = ' ' then
                nnsb <- nnsb.Append("1")
                i <- i + 2
            else if sb[i] <> ' ' then
                let mutable nsb = StringBuilder()
                let mutable y = 0
                while((i+y) <= sb.Length-1 && sb[i+y] <> ' ') do
                    nsb <- nsb.Append(sb[i+y])
                    y <- y+1
                    
                let ns = nsb.ToString()
                
                if(ns.Length%2=0) then
                    let l = ns[..(ns.Length / 2) - 1]
                    let h =
                        let mutable v = ns[(ns.Length/2)..]
                        if (v |> Seq.head = '0' && v |> Seq.last = '0' && v |> Seq.forall (fun x -> x = '0')) then
                            v <- "0"
                        else
                            v <- (v.TrimStart '0')
                        v
                    nnsb <- nnsb.Append(l)
                    nnsb <- nnsb.Append(h)
                else
                    nnsb <- nnsb.Append(((UInt64.Parse ns)*2024UL).ToString())
                    
                i <- i + y
            else
                nnsb <- nnsb.Append " "
                i <- i + 1
        sb <- nnsb.ToString()
        
    let mutable a1 = 0
    let mutable a2 = 0
    
    for i in 0..(25)-1 do
        p()

    let st = sb.ToString() |> Seq.length
    printfn $"%A{st}"
        
    
    //a1 <- stones.Count
    //a2 <- stones.Count
    //io.submitAnswer 1 a1
    //io.submitAnswer 2 a2
    0