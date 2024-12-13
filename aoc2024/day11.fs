module aoc2024.day11

open System
open System.Collections.Generic
open aoc2024.util


let nDigits n = (int (log10 (n |> double)) + 1)

let split n ndigits =
    let x = Math.Pow(10, (ndigits / 2.)) |> uint64
    (n / x, n % x)

let splitl n ndigits =
    let x = Math.Pow(10, (ndigits / 2.)) |> uint64
    [ n / x; n % x ]

let isEven n =
    let nd = nDigits n
    nd % 2 = 0

let solve () =
    let io = aocIO

    let mutable stones =
        List(io.getInput () |> Seq.head |> String.extractAllNumsUint64 |> Seq.toList)

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
                | 0UL ->
                    nxt.Add(0UL, (1UL, None))
                    q.Enqueue(1UL)
                | n when isEven n ->
                    let nd = nDigits n
                    let l, r = split n nd
                    nxt.Add(n, (l, Some(r)))
                    q.Enqueue(l)
                    q.Enqueue(r)
                | n ->
                    let nv = n * 2024UL
                    nxt.Add(n, (nv, None))
                    q.Enqueue(nv)

    let m = Dictionary<uint64, Dictionary<int, uint64>>()

    let nr n targetR =

        let rec loop n rl sr acc =
            if (rl = 0) then
                acc
            else
                if not (nxt.ContainsKey(n)) then
                    fnd n rl

                let f, s = nxt[n]
                let nsr = (rl - 1)

                if not (m.ContainsKey f) then
                    m[f] <- Dictionary<int, uint64>()

                if s.IsSome && not (m.ContainsKey s.Value) then
                    m[s.Value] <- Dictionary<int, uint64>()

                let lv =
                    if m[f].ContainsKey(rl) then
                        m[f][rl]
                    else
                        let r = (loop f nsr sr 1UL)
                        m[f][rl] <- r
                        r

                let rv =
                    if s.IsSome then
                        if m[s.Value].ContainsKey(rl) then
                            m[s.Value][rl]
                        else
                            let r = (loop s.Value nsr sr 1UL)
                            m[s.Value][rl] <- r
                            r
                    else
                        0UL

                lv + rv


        loop n targetR targetR 0UL

    let res = stones |> Seq.fold (fun acc x -> acc + (nr x 75)) 0UL
    printfn $"%A{res}"
