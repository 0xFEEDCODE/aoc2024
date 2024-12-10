module aoc2024.day09

open System
open System.Collections.Generic
open System.Text
open Microsoft.FSharp.Core
open aoc2024.util

let solve()=
    let io = aocIO
    let mutable inp = io.getInput() |> Seq.head
    inp <- inp + "0"
    
    let idMap = Dictionary<char, int>()
    let w =
        inp
        |> Seq.pairwise
        |> smapi
        |> Seq.filter (fun (i, _) -> i % 2 = 0)
        |> Seq.map snd
        |> smapi
        |> Seq.map (fun (i, x) -> (i, (Int16.Parse ((fst x).ToString()) |> int, Int16.Parse((snd x).ToString()) |> int)))
    
    let str (x: int * (int * int)) =
        let i, (f,s) = x
        let mutable id =  char 1 + char i
        if (id = '.') then
            id <- Char.MaxValue
        idMap[id] <- i
        
        new string(id, f)+new string('.', s)
        
    let toStr (s : (int * (int * int)) seq) =
        s |> Seq.map(str) |> Seq.fold(fun acc x -> acc + x) ""
        
    (*
    let mutable s = toStr w
    
    let mutable l = List<char>()
    s |> Seq.iter(fun x -> l.Add x)
    
    let len = (s |> Seq.length)-1
    let mutable i = len
    let mutable lastJ = None
    while (i >= 0 && (lastJ.IsNone || (lastJ.IsSome && i > lastJ.Value))) do
        if(l[i] <> '.') then
            let mutable j = if lastJ.IsNone then -1 else lastJ.Value
            let mutable found = false
            while (not found && j < i) do
                j <- j+1
                if(i <> j && l[j] = '.') then
                    found <- true
                    
            if found then
                l[j] <- l[i]
                l[i] <- '.'
                lastJ <- Some(j)
            else
                failwith "w"
            (*
            let s = l |> Seq.fold(fun acc x -> acc+x.ToString() ) ""
            printfn $"%A{s}"
            *)
            
        i <- i - 1
    
    let s = l |> Seq.fold(fun acc x -> acc+x.ToString() ) ""
    
    
    let mutable a1 = bigint 0
    let mutable pos = 0
    let firstFree = s.IndexOf('.')
    for i in 0..firstFree-1 do
        let id = idMap[s[i]]
        a1 <- a1 + (bigint (id * i))
        pos <- pos + 1
        ()
            
    
    //printfn $"%A{s}"
    printfn $"%A{a1}"
    *)
    
    let mutable s = StringBuilder(toStr w)
    
    let len = (s.Length)-1
    let mutable i = len
    while (i >= 0) do
        if(s[i] <> '.') then
            
            let ch = s[i]
            let mutable len = 0
            while(i >= 0 && s[i] = ch) do
                len <- len + 1
                i <- (i-1)
                
            let mutable j = -1
            let mutable found = false
            while (not found && j < i) do
                j <- j+1
                if(i <> j && s[j] = '.') then
                    let mutable isEnough = len
                    let mutable k = j
                    while(isEnough >= 0 && s[k] = '.') do
                        k <- k+1
                        isEnough <- isEnough - 1
                    if (isEnough <= 0) then
                        found <- true
                    
            if found then
                for xl in 0..len-1 do
                    s[j+xl] <- ch
                    s[i+1+xl] <- '.'
            
        else
            i <- (i - 1)
    
    let s = s.ToString() |> Seq.fold(fun acc x -> acc+x.ToString() ) ""
    
    let mutable a1 = bigint 0
    
    for i in 0..(s|>Seq.length)-1 do
        if(s[i] <> '.') then
            let id = idMap[s[i]]
            a1 <- a1 + (bigint (id * i))
        ()
            
    
    //printfn $"%A{s}"
    printfn $"%A{a1}"
        
    
    
    //io.submitAnswer 1 a1
    //io.submitAnswer 2 a2
    
    0
    

