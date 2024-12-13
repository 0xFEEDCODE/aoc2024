module aoc2024.day12

open System.Collections.Generic
open Microsoft.FSharp.Core
open aoc2024.util
open aoc2024.util.Grid


type Plane =
    | X of int
    | Y of int

let solve () =
    let io = aocIO
    let inp = io.getInput (false)

    let gr = Grid.initializeFromStringSeq inp

    let wb (p: Point2D) =
        let (x, y) = (p.x, p.y)
        (x >= 0 && x <= (gr[0].Length - 1) && y >= 0 && y <= (gr.Length - 1))

    let d = Dictionary<int * int, List<char>>()

    let opD (p: Point2D) = Point2D(p.x * -1, p.y * -1)


    let getBP yx ch =
        let y, x = yx
        let sp = Point2D(x, y)

        let explore = Queue<Point2D>()
        let mutable region = List<Point2D>()
        let visited = List<Point2D>()

        let dirs = [ (-1, 0); (1, 0); (0, -1); (0, 1) ] |> Seq.map Point2D

        let addExplore p =
            visited.Add(p)

            for np in Grid.getAdjacentNeighbours p do
                if not (visited.Contains(np)) && wb np then
                    explore.Enqueue(np)

        if not (d.ContainsKey(y, x)) then
            d[(y, x)] <- List<char>()

        addExplore sp
        region.Add sp

        while (explore.Count > 0) do
            let c = explore.Dequeue()

            if (not (visited.Contains(c)) && gr[c.y][c.x] = ch) then
                addExplore c
                region.Add(c)

        region.ForEach(fun p ->
            if not (d.ContainsKey((p.y, p.x))) then
                d[(p.y, p.x)] <- List<char>()

            d[(p.y, p.x)].Add(ch))

        let isCh (p: Point2D) = region.Contains p
        //(wb p) && gr[p.y][p.x] = ch

        let isOp (p: Point2D) = not (region.Contains p)
        //not (wb p) || gr[p.y][p.x] <> ch

        let edgeCheck (n: Point2D) neighbors =
            let l = Point2D(n.x - 1, n.y)
            let r = Point2D(n.x + 1, n.y)
            let u = Point2D(n.x, n.y - 1)
            let d = Point2D(n.x, n.y + 1)

            if
                neighbors |> List.contains l
                && neighbors |> List.contains u
                && not (neighbors |> List.contains r)
                && not (neighbors |> List.contains d)
            then
                true
            else if
                neighbors |> List.contains r
                && neighbors |> List.contains d
                && not (neighbors |> List.contains l)
                && not (neighbors |> List.contains u)
            then
                true
            else if
                neighbors |> List.contains r
                && neighbors |> List.contains u
                && not (neighbors |> List.contains l)
                && not (neighbors |> List.contains d)
            then
                true
            else
                neighbors |> List.contains l
                && (neighbors |> List.contains d)
                && not (neighbors |> List.contains r)
                && not (neighbors |> List.contains u)

        let gl () =
            let neighbors =
                region
                |> Seq.map (fun r -> r |> getAllNeighbours)
                |> Seq.collect id
                |> Seq.where isOp
                |> Seq.toList

            let mutable nn = []

            for n in neighbors do
                let l = Point2D(n.x - 1, n.y)
                let r = Point2D(n.x + 1, n.y)
                let u = Point2D(n.x, n.y - 1)
                let d = Point2D(n.x, n.y + 1)
                let all = [ l; r; u; d ]

                let nOccurences =
                    all |> Seq.where (fun x -> neighbors |> List.contains x) |> Seq.length

                (*
                if ((nOccurences = 3) || (nOccurences = 1)) then
                    nn <- nn @ [ n ]
                    *)

                if edgeCheck n neighbors || ((nOccurences = 3) || (nOccurences = 1)) then
                    nn <- nn @ [ n ]

            let neighbors = Set(nn) |> Seq.toList


            let mutable vertices = 0
            let mutable cons = []

            let ec (p: Point2D) =
                if not (isCh p) then
                    0
                else
                    let u = -1
                    let d = 1
                    let l = -1
                    let r = 1

                    let br = p + Point2D(r, d)
                    let bl = p + Point2D(l, d)
                    let tl = p + Point2D(l, u)
                    let tr = p + Point2D(r, u)

                    let d = Point2D(p.x, p.y + 1)
                    let u = Point2D(p.x, p.y - 1)
                    let l = Point2D(p.x - 1, p.y)
                    let r = Point2D(p.x + 1, p.y)

                    let mutable s = 0

                    if (p = Point2D(0, 2)) then
                        ()

                    // ..
                    // .x
                    if (isOp u && isOp l && isOp tl) then
                        s <- s + 1
                    // ..
                    // x.
                    if (isOp u && isOp r && isOp tr) then
                        s <- s + 1
                    // x.
                    // ..
                    if (isOp d && isOp r && isOp br) then
                        s <- s + 1
                    // .x
                    // ..
                    if (isOp d && isOp l && isOp bl) then
                        s <- s + 1

                    // .x
                    // xx
                    if (isCh d && isCh r && isOp br) then
                        s <- s + 1
                    // x.
                    // xx
                    if (isCh u && isCh r && isOp tr) then
                        s <- s + 1
                    // xx
                    // .x
                    if (isCh l && isCh u && isOp tl) then
                        s <- s + 1
                    // xx
                    // x.
                    if (isCh d && isCh l && isOp bl) then
                        s <- s + 1


                    if (isOp d && isOp r && isCh br) then
                        s <- s + 1

                    if (isOp l && isOp d && isCh bl) then
                        s <- s + 1

                    if (isOp u && isOp l && isCh tl) then
                        s <- s + 1

                    if (isOp u && isOp r && isCh tr) then
                        s <- s + 1






                    (*
                    if (s > 0) then
                        printfn $"%A{(p, s)}"
                        *)

                    s





            (*
                let complexCase =
                    let nei = p |> getAllNeighbours
                    if edgeCheck
                    *)


            if ch = 'X' then
                ()


            for x in region do
                let s = ec x
                vertices <- vertices + s

            vertices


        let sc = gl ()
        //printfn $"%A{(ch, sc)}"
        (region.Count, sc)


    let mutable a1 = 0
    let mutable a2 = 0

    for y in 0 .. (gr.Length - 1) do
        for x in 0 .. (gr[0].Length - 1) do
            let ch = gr[y][x]
            let idx = (y, x)

            if not (d.ContainsKey((y, x))) || not (d[y, x].Contains(ch)) then
                let (a, p) = getBP (y, x) ch
                a1 <- a1 + (a * p)


    printfn $"%A{a1}"
    //io.submitAnswer 1 a1
    //io.submitAnswer 2 a2
    0
