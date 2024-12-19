module aoc2024.day11

open System
open System.Collections.Generic
open aoc2024.util


let getNDigits n = int ((log10 (double n)) + one)

let split n (ndigits: int) =
    let x = Math.Pow(10, (float ndigits / 2.))
    (float n / x, float n % x)

let splitl n (ndigits: int) =
    let x = Math.Pow(10, (float ndigits / 2.))
    [ float n / x; n % x ]

let isEven n =
    let nd = getNDigits n
    nd % 2 = zero

let solve () =
    let io = aocIO
    
    let mutable stones =
        List(
            io.getInput ()
            |> Seq.head
            |> String.extractAllNumsBig
            |> Seq.map uint64
            |> Seq.toList
        )

    let nxt = Dictionary<uint64, uint64 * uint64 option>()

    let fnd n r =
        let q = Queue()
        q.Enqueue(n)

        let nStElements = q.Count

        for _ in 0..r do
            for _ in 0..nStElements do
                match q.Dequeue() with
                | n when nxt.ContainsKey n ->
                    let f, s = nxt[n]
                    q.Enqueue(f)

                    if s.IsSome then
                        q.Enqueue(s.Value)
                | n when n = zero ->
                    nxt.Add(zero, (one, None))
                    q.Enqueue(one)
                | n when isEven n ->
                    let nDigits = getNDigits n
                    let l, r = split n nDigits
                    let l, r = uint64 l, uint64 r
                    nxt.Add(n, (l, Some(r)))
                    q.Enqueue(l)
                    q.Enqueue(r)
                | n ->
                    let newValue = n * 2024UL
                    nxt.Add(n, (newValue, None))
                    q.Enqueue(newValue)

    let cached = Dictionary<uint64, Dictionary<int, uint64>>()

    let nr n targetR =
        let rec loop n rl sr acc =
            if (rl = 0) then
                acc
            else
                if not (nxt.ContainsKey(n)) then
                    fnd n rl

                let f, s = nxt[n]
                let nsr = (rl - 1)

                if not (cached.ContainsKey f) then
                    cached[f] <- Dictionary<int, uint64>()

                if s.IsSome && not (cached.ContainsKey s.Value) then
                    cached[s.Value] <- Dictionary<int, uint64>()

                let lv =
                    if cached[f].ContainsKey(rl) then
                        cached[f][rl]
                    else
                        let r = (loop f nsr sr one)
                        cached[f][rl] <- r
                        r

                let rv =
                    if s.IsSome then
                        if cached[s.Value].ContainsKey(rl) then
                            cached[s.Value][rl]
                        else
                            let r = (loop s.Value nsr sr one)
                            cached[s.Value][rl] <- r
                            r
                    else
                        zero

                lv + rv

        loop n targetR targetR zero

    let res = stones |> Seq.fold (fun acc x -> acc + (nr x 75)) zero
    printfn $"%A{res}"
