module aoc2024.day11

open System
open System.Collections.Generic
open aoc2024.util

type Spl<'T> = { L: 'T; R: 'T }

type Link =
    { V: uint64
      mutable Left: Option<Link>
      mutable Right: Option<Link> }


type Link with
    static member Create(v: uint64) = { V = v; Left = None; Right = None }
    member this.Assignleft(n: Link) = this.Left <- Some(n)
    member this.AssignRight(n: Link) = this.Right <- Some(n)

type Entry =
    | Split of Spl<string>
    | Splits of Spl<string> list
    | Zero
    | Odd of uint64
    | Odds of uint64 list

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

    let mutable lnk = new Dictionary<uint64, Link>()
    lnk[0UL] <- Link.Create(0UL)
    let nxt = Dictionary<uint64, uint64 * uint64 option>()

    let st = Stack()
    stones |> Seq.iter st.Push
    st.Push(336509UL)
    st.Push(138944UL)

    let fnd (st: Stack<uint64>) =
        let mutable nl = List<List<uint64>>()

        for i in 0..30 do
            let mutable ll = List<uint64>()

            while (st.Count > 0) do
                match st.Pop() with
                | n when nxt.ContainsKey n ->
                    let (f, s) = nxt[n]
                    ll.Add(f)

                    if s.IsSome then
                        ll.Add s.Value
                | 0UL ->
                    nxt.Add(0UL, (1UL, None))
                    ll.Add(1UL)
                | n when isEven n ->
                    let nd = nDigits n
                    let l, r = split n nd
                    nxt.Add(n, (l, Some(r)))
                    ll.Add(l)
                    ll.Add(r)
                | n ->
                    let nv = n * 2024UL
                    nxt.Add(n, (nv, None))
                    ll.Add(nv)

            nl.Add(ll)

            for x in ll do
                st.Push x

    fnd st

    let m = Dictionary<uint64, Dictionary<int, uint64>>()

    let nr n targetR =
        let targetR = targetR + 1

        let rec loop n rl sr acc =
            if (rl = 0) then
                acc
            else
                if not (nxt.ContainsKey(n)) then
                    let st = Stack()
                    st.Push(n)
                    fnd st

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

    let res = stones |> Seq.fold (fun acc x -> acc + (nr x 74)) 0UL
    printfn $"%A{res}"

    (*
    for i in 0..75 do
        let r = nr 0UL i
        printfn $"%A{(i, r)}"
        *)
    ()
