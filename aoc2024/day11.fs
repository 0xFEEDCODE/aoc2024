module aoc2024.day11


open System
open System.Collections.Generic
open System.Linq
open aoc2024.util

type Spl<'T> = { L: 'T; R: 'T }
type Entry =
    | Split of Spl<string>
    | Splits of Spl<string> list
    | Zero
    | Odd of uint64
    | Odds of uint64 list
    
    
let nDigits n =
    (int (log10(n |> double)) + 1)
let split n ndigits =
    let x = Math.Pow(10, (ndigits/2.)) |> uint64
    (n / x, n % x)
    
let splitl n ndigits =
    let x = Math.Pow(10, (ndigits/2.)) |> uint64
    [n / x; n % x]
    
let isEven n =
     let nd = nDigits n
     nd % 2 = 0
        
type Lifetime(t: int, v: List<uint64>) =
    let mutable v = v
    let mutable t = t
    member this.V with get() = v and set value = v <- value
    member this.T with get() = t and set value = t <- value
    member this.IsAlive() = t = 0
    member this.Tick() = t <- t - 1
     

    (*
    let rec gs lst r =
        let rec loop l acc cr =
            if r = cr then
                acc
            else
                let mutable acc = acc
                for n in lst do
                    if not (d.ContainsKey(n)) || not (d[n].ContainsKey(cr)) then
                        if not (d.ContainsKey(n)) then
                            d[n] <- Dictionary<int, uint64>()
                        if not (d[n].ContainsKey(cr)) then
                            d[n][cr] <- 0UL
                            
                        if n = 0UL then
                            let res = gs [1UL] cr
                            d[n][cr] <- res
                        if n >= 10UL && isEven n then
                            let nd = nDigits n
                            let l,r = split n nd
                            let res = (gs [l] cr) + (gs [r] cr)
                            d[n][cr] <-  res
                        else
                            let res = gs [n*2024UL] cr
                            d[n][cr] <- res
                    acc <- acc + d[n][cr]
                loop l acc (cr+1)
                *)
let solve () =
    let io = aocIO
    let mutable stones = io.getInput() |> Seq.head |> String.extractAllNumsUint64 |> Seq.toList
    
    let d = Dictionary<uint64, Dictionary<int, uint64>>()
    
        
    let rec gs on r =
        if not (d.ContainsKey(on)) then
            d[on] <- Dictionary<int, uint64>()
            
        let rec loop (l: List<uint64>) cr acc =
            d[on][cr] <- acc
            if r = cr then
                d[on][r] <- acc
                d[on][r]
            else
                let cr = cr + 1
                let mutable nl = List()
                let mutable acc = 0UL
                for n in l do
                    if n <> on && (not (d.ContainsKey n) || not (d[n].ContainsKey(r))) then
                        acc <- acc + (gs n r)
                    else if d.ContainsKey n && d[n].ContainsKey(r) then
                        acc <- acc  + d[n][r]
                    else
                        if n = 0UL then
                            acc <- acc + 1UL
                            nl.Add(1UL)
                        if n >= 10UL && isEven n then
                            let nd = nDigits n
                            let l,r = split n nd
                            acc <- acc + 2UL
                            nl.Add(l)
                            nl.Add(r)
                        else
                            acc <- acc + 1UL
                            nl.Add(n*2024UL)
                loop nl cr acc
                
        loop (List([on])) 0 1UL
        
    let t = 40
    stones |> Seq.iter(fun s -> gs s t |> ignore)
    for i in 0..(t-1) do
        let s = d.Where(fun x -> x.Value.ContainsKey(i) && stones.Contains(x.Key)).Select(fun x -> x.Value[i]) |> Seq.sum
        printfn $"%A{s}"
    ()
    
    (*let rec proc (lst: List<uint64>) targetN  =
        let map = Dictionary<int, List<uint64>>()
        map[0] <- lst
        map[1] <- List<uint64>()
        
        let mutable i = 0
        
        while(i < targetN) do
            if not (map.ContainsKey(i+1)) then
                map[i+1] <- List<uint64>()
                
            let target = map[i+1]
            for n in map[i] do
                if n = 0UL then
                    target.Add(1UL)
                if n >= 10UL && isEven n then
                    let nd = nDigits n
                    let l,r = split n nd
                    target.Add(l)
                    target.Add(r)
                else
                    if i <> targetN && (d.ContainsKey n) then
                        let en = d[n]
                        let enIdx = en.Keys.Max()
                        let offset = if (i+enIdx) > targetN then (targetN - (i+enIdx)) else 0
                        let offset = if offset < 0 then offset else 0
                        
                        let enIdx = enIdx + offset;
                        let idx = i + enIdx
                        
                        if not (map.ContainsKey idx) then
                            map[idx] <- List<uint64>()
                        map[idx].AddRange(en[enIdx])
                    else
                        target.Add(n*2024UL)
            i <- i + 1
        map*)
       
    (*let rec procUntilSingles (lst: List<uint64>) targetId lim pall ss =
        let mutable lst = List(lst)
        
        let res = proc lst lim ss pall
        if not (d.ContainsKey targetId) then
            d[targetId] <- Dictionary<int, List<uint64>>()
            
        let mutable prev = -1
        res.OrderBy(fun x -> x.Key) |> Seq.takeWhile(fun x ->
            let r = x.Value.Count > 0 && prev = -1 || prev+1 = x.Key
            prev <- x.Key
            r) |> Seq.iter(fun x -> if not (d[targetId].ContainsKey(x.Key)) then d[targetId][x.Key] <- x.Value)
        
    for i in 0..9 do
        if( i <> 8 ) then
            let i = i |> uint64
            if not (isEven i) then
                let ls = List<UInt64>()
                ls.Add(i)
                procUntilSingles ls i 25 false true*)

    (*
    printfn $"Hi"
    let r = proc stones 50 
    printfn $"%A{r[50].Count}"
    *)
        
