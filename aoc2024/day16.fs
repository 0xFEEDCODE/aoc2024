module aoc2024.day16

open System.Collections.Generic
open Microsoft.FSharp.Core
open aoc2024.util

(*type R =
    { X: int
      Y: int }

    static member C(x: int, y: int) = { X = x; Y = y }
    
type C(x: int, y: int) =
    let mutable X = x
    let mutable Y = y
    
    *)


type Dir =
    | L
    | R
    | U
    | D

let solve () =
    let mutable a1 = bigint.Zero
    let mutable a2 = bigint.Zero

    let canMove d1 d2 =
        match d1 with
        | L -> d2 = L || d2 = U || d2 = D
        | R -> d2 = R || d2 = U || d2 = D
        | U -> d2 = U || d2 = L || d2 = R
        | D -> d2 = D || d2 = L || d2 = R

    let d2 =
        Map.empty
            .Add(Point2D(0, 1), Dir.D)
            .Add(Point2D(0, -1), Dir.U)
            .Add(Point2D(-1, 0), Dir.L)
            .Add(Point2D(1, 0), Dir.R)

    let dirs =
        Map.empty
            .Add(Dir.D, Point2D(0, 1))
            .Add(Dir.U, Point2D(0, -1))
            .Add(Dir.L, Point2D(-1, 0))
            .Add(Dir.R, Point2D(1, 0))

    let io = aocIO
    let inp = io.getInput ()

    let gr = inp |> Grid.initializeFromStringSeq

    let rows, cols = (gr.Length - 1, gr[0].Length - 1)

    let mutable S = Point2D()
    let mutable E = Point2D()

    for y in 0..rows do
        for x in 0..cols do
            if (gr[y][x] = 'S') then
                S <- Point2D(x, y)

            if (gr[y][x] = 'E') then
                E <- Point2D(x, y)

            ()

    let noffs = [ Point2D(-1, 0); Point2D(1, 0); Point2D(0, -1); Point2D(0, 1) ]

    let mutable visited = Dictionary<Point2D, int>()

    let pr () =
        for y in 0..rows do
            for x in 0..cols do
                if (visited.ContainsKey(Point2D(x, y))) then
                    printf "O"
                else
                    printf $"%c{gr[y][x]}"

            printfn ""

    let mutable mc = None

    let fp (start: Point2D) (target: Point2D) (sp: int) (sdir: Dir) (mv: Option<int>) =

        let mutable cameFrom = Dictionary<Point2D * Dir, Option<Point2D * Dir>>()

        cameFrom.Add((start, sdir), None)

        let mutable q = PriorityQueue<(Point2D * Dir) * ((Point2D * int) * Dir) list, int>()
        q.Enqueue(((start, sdir), []), sp)

        let mutable costSoFar = Dictionary<Point2D * Dir, int>()
        costSoFar[(start, sdir)] <- sp

        let mutable found = false

        let mutable result = (0, [])

        while not found && q.Count > 0 do
            let (cp, cd), junctions = q.Dequeue()

            if cp = target then
                let mutable c = Some((cp, cd))
                let mutable count = 0

                visited[start] <- 1
                visited[target] <- 1

                result <- (costSoFar[(cp, cd)], junctions)

                while c.IsSome do
                    count <- count + 1
                    let cp, cd = c.Value
                    c <- cameFrom[(cp, cd)]
                    visited[cp] <- 1

                found <- true

                //pr ()

            let neighbours =
                noffs
                |> Seq.map (fun offs -> (offs + cp, d2[offs]))
                |> Seq.where (fun (_, d) -> canMove cd d)

            let mutable choices = []

            for np, nd in neighbours do
                let newCost = costSoFar[(cp, cd)] + (if nd = cd then 1 else 1001)

                if
                    ((mv.IsNone || newCost <= mv.Value)
                     && (not (costSoFar.ContainsKey(np, nd)) || newCost < costSoFar[(np, nd)])
                     && np.y >= 0
                     && np.y < rows
                     && np.x >= 0
                     && np.x < cols
                     && (gr[np.y][np.x] <> '#'))
                then
                    choices <- choices @ [ (np, nd, newCost) ]

            let lj =
                if (choices |> Seq.length > 1) then
                    let ch = choices |> List.map (fun (np, nd, nc) -> ((np, nc), nd))
                    junctions @ ch
                else
                    junctions

            for np, nd, nc in choices do
                costSoFar[(np, nd)] <- nc
                let priority = nc
                q.Enqueue(((np, nd), lj), priority)
                cameFrom[(np, nd)] <- Some(cp, cd)


        if (fst result) <> 0 then Some(result) else None

    let r = (fp S E 0 R None)
    let junctions = snd r.Value
    let mv = fst r.Value



    for (j, sp), d in junctions |> Seq.skip 1 do
        fp j E sp d (Some(mv)) |> ignore


    (*
    printfn $"%A{visited.Count}"
    printfn $"%A{junctions}"
    *)

    printfn $"%A{visited.Count}"
    0


//io.submitAnswer 1 a1
//io.submitAnswer 2 a2
