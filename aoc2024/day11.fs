module aoc2024.day11

open System
open System.Collections.Generic
open Microsoft.FSharp.Collections
open aoc2024.day06
open aoc2024.util

type Spl<'T> = { L: 'T; R: 'T }
type Entry =
    | Split of Spl<string>
    | Splits of Spl<string> list
    | Zero
    | Odd of uint64
    | Odds of uint64 list
    

(*
let solve () =
    let io = aocIO
    
    let splitStr (s: string) = 
        let l = s[..(s.Length / 2) - 1]
        let r =
            let mutable v = s[(s.Length/2)..]
            if (v = "" || (v |> Seq.head = '0' && v |> Seq.last = '0' && v |> Seq.forall (fun x -> x = '0'))) then
                v <- ""
            else
                v <- (v.TrimStart '0')
            v
        {L = l; R = r}
        
    let mutable stones = io.getInput() |> Seq.head |> String.extractAllNumsUint64 |> Seq.map (fun x ->
        if x = 1UL then
            Odd (1UL)
        else if (x.ToString().Length%2=0) then
            Split (splitStr (x.ToString()))
        else if x = 0UL then
            Zero
        else
            Odd(x)
        )
    
    let processOdd v =
        let nv = v*2024UL
        let s = nv.ToString()
        if (s.Length >= 2 && s.Length % 2 = 0) then
            Split (splitStr s)
        else
            Odd nv
            
    let p s =
        match s with
        | Split spl ->
            let mutable e = []
            if(spl.L <> "") then
                if (spl.L.Length%2 = 0) then
                    e <- e @ [Split(splitStr spl.L)]
                else
                    e <- e @ [Odd(uint64 spl.L)]
            else
                e <- e @ [Zero]
            if (spl.R <> "") then
                if (spl.R.Length%2 = 0) then
                    e <- e @ [Split(splitStr spl.R)]
                else
                    e <- e @ [Odd(uint64 spl.R)]
            else
                e <- e @ [Zero]
            e
        | Splits spls ->
            let nsplits = spls |> List.map(fun x -> [x.L; x.R] |> List.where(fun y -> y.Length > 0) |> List.map(fun x -> Split(splitStr x))) |> List.collect id
            let nodds = spls |> List.map(fun x -> [x.L; x.R] |> List.where(fun y -> y.Length = 1) |> List.map(fun x -> Odd(uint64 x))) |> List.collect id
            nsplits @ nodds
        | Zero ->
            [Odd 1UL ]
        | Odd v ->
            [processOdd v ]
        | Odds v ->
            v |> List.map(processOdd)
            
    
    let proc () =
        stones <- stones |> PSeq.collect p
        
        
    let mutable a1 = 0
    let mutable a2 = 0
    
    let pr() = 
        for s in stones do
            printf $" %A{s}"
        printfn ""
    
    for i in 0..(40)-1 do
        proc()

    let st = stones |> Seq.sumBy(fun x ->
            match x with
            | Splits spl ->
                spl.Length
            | Odds o ->
                o.Length
            | _ -> 1)
    printfn $"%A{st}"
    *)
    
(*let solve () =
    let io = aocIO
    
    let mutable stones = io.getInput() |> Seq.head |> String.extractAllNumsUint64 |> Seq.map string
   
    let mutable evens = List(stones |> Seq.where(fun x -> x.Length%2=0) |> Seq.toList)
    let mutable odds = List(stones |> Seq.where(fun x -> x.Length%2<>0) |> Seq.map uint64 |> Seq.toList)
    
    let splitStr (s: string) = 
        let l = s[..(s.Length / 2) - 1]
        let r =
            let mutable v = s[(s.Length/2)..]
            if (v |> Seq.head = '0' && v |> Seq.last = '0' && v |> Seq.forall (fun x -> x = '0')) then
                v <- "0"
            else
                v <- (v.TrimStart '0')
            v
        {L = l; R = r}
        
    let processOdd v =
        let nv = v*2024UL
        let s = nv.ToString()
        if (s.Length >= 2 && s.Length % 2 = 0) then
            Split (splitStr s)
        else
            Odd nv
    
    let proc () =
        let nevens = List<string>()
        let nodds = List<uint64>()
        
        for e in evens do
            let spl = splitStr e
            if(spl.L.Length > 1 && spl.L.Length%2 = 0) then
                nevens.Add(spl.L)
            else if(spl.L.Length = 1) then
                nodds.Add(spl.L |> uint64)
                
            if(spl.R.Length > 1 && spl.R.Length%2 = 0) then
                nevens.Add(spl.R)
            else if(spl.R.Length = 1) then
                nodds.Add(spl.R |> uint64)
                
        for o in odds do
            let po = processOdd o
            match po with
            | Odd o ->
                nodds.Add(o)
            | Split spl ->
                if(spl.L.Length > 1 && spl.L.Length%2 = 0) then
                    nevens.Add(spl.L)
                else
                    nodds.Add(spl.L |> uint64)
                if(spl.R.Length > 1 && spl.R.Length%2 = 0) then
                    nevens.Add(spl.R)
                else
                    nodds.Add(spl.R |> uint64)
        evens <- nevens
        odds <- nodds
            
        
    let mutable a1 = 0
    let mutable a2 = 0
    
    let pr() = 
        for s in stones do
            printf $" %A{s}"
        printfn ""
    
    for i in 0..50-1 do
        proc()
        ()

    printfn $"%A{((evens.Count/2) + odds.Count)}"*)
    
let nDigits n =
    (int (log10(n |> double)) + 1)
let split n ndigits =
    let x = Math.Pow(10, (ndigits/2.)) |> uint64
    (n / x, n % x)

let solve () =
    let io = aocIO
    let mutable stones = io.getInput() |> Seq.head |> String.extractAllNumsUint64 |> Array.toSeq |> Seq.map(fun x -> seq{ x })
    let mutable d = Dictionary<uint64, uint64 * Option<uint64>>()
    
    let proc () =
        stones <- stones |> PSeq.map(fun s ->
            let en = s.GetEnumerator()
            seq {
            while en.MoveNext() do
                let n = en.Current
                
                if n = 0UL then
                     yield 1UL
                 else
                    if d.ContainsKey(n) then
                        let f,s = d[n]
                        if s.IsSome then
                            yield s.Value
                        yield f
                    else
                         let nd = nDigits n
                         if nd % 2 = 0 then
                             let f,s = split n nd
                             d[n] <- (f, Some(s))
                             yield f
                             yield s
                         else
                             let v = (n * 2024UL)
                             yield v
                             d[n] <- (v, None)
            }
        )
        
    let mutable a1 = 0
    let mutable a2 = 0
    
    let pr() = 
        for s in stones do
            printf $" %A{s}"
        printfn ""
    
    for i in 0..(75)-1 do
        (*
        let l1 = stones.Count
        *)
        proc()
        (*
        let l2 = stones.Count
        printfn $"%A{((i, l1, l2, l2-l1))}"
        *)
        printfn $"%A{i}"
        ()

    let st = (stones |> Seq.fold(fun acc s -> acc + bigint(s |> Seq.length)) bigint.Zero)
    printfn $"%A{st}"