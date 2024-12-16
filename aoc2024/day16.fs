module aoc2024.day16

open System.Collections.Generic
open Microsoft.FSharp.Core
open aoc2024.util

type Dir =
    | L
    | R
    | U
    | D

type Record =
    { cp: Point2D
      cd: Dir
      score: int
      path: Point2D list }

    member this.Unpack = (this.cp, this.cd, this.score, this.path)

let solve () =
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

    let neighbourOffsets =
        [ Point2D(-1, 0); Point2D(1, 0); Point2D(0, -1); Point2D(0, 1) ]

    let mutable visitedBestPath = Dictionary<Point2D, int>()

    let fp (start: Point2D) (target: Point2D) (sp: int) =

        let mutable visited = Dictionary<Point2D * Dir, int>()

        let mutable q = PriorityQueue()

        q.Enqueue(
            { cp = start
              cd = R
              score = sp
              path = [ start ] },
            0
        )

        let mutable found = false
        let mutable maxScore = None

        while q.Count > 0 do
            let c = q.Dequeue()
            let cp, cd, score, path = c.Unpack

            if (maxScore.IsSome && score > maxScore.Value) then
                ()
            else
                if cp = target then
                    maxScore <- Some(score)

                    for p in path do
                        visitedBestPath[p] <- 1

                    found <- true

                if (not (visited.ContainsKey(cp, cd)) || (score <= visited[(cp, cd)])) then
                    visited[(cp, cd)] <- score

                    let neighbours =
                        neighbourOffsets
                        |> List.map (fun offs -> (offs + cp, d2[offs]))
                        |> List.where (fun (_, d) -> canMove cd d)

                    let mutable choices =
                        neighbours
                        |> List.where (fun (np, _) -> (np.y >= 0 && np.y < rows && np.x >= 0 && np.x < cols && (gr[np.y][np.x] <> '#')))
                        |> List.map (fun (np, nd) ->
                            let newScore = score + (if nd = cd then 1 else 1001)
                            (np, nd, newScore))

                    choices
                    |> List.iter (fun (np, nd, ns) ->
                        q.Enqueue(
                            { cp = np
                              cd = nd
                              score = ns
                              path = path @ [ np ] },
                            score
                        ))

        maxScore

    let bestPathScore = fp S E 0
    printfn $"%A{(bestPathScore, visitedBestPath.Count)}"
    0
