module aoc2024.day20

open System.Collections.Generic
open aoc2024.util

let solve () =
    let io = aocIO
    let inp = io.getInput ()

    let gr = inp |> Grid.initializeFromStringSeq
    let rows = gr.Length - 1
    let cols = gr[0].Length - 1

    let mutable start = Point2D()
    let mutable target = Point2D()

    for y in 0..rows do
        for x in 0..cols do
            if gr[y][x] = 'S' then
                start <- Point2D(x, y)

            if gr[y][x] = 'E' then
                target <- Point2D(x, y)

    let neighbourOffsets = [ (-1, 0); (1, 0); (0, -1); (0, 1) ] |> List.map Point2D

    let fp (start: Point2D) (target: Point2D) (useCheatTime: bool) (shortestAllowedTime: int option) =
        let mutable visited = Dictionary<Point2D * int * uint64, int>()

        let mutable cts = List<Point2D * Point2D>()

        let mutable q = PriorityQueue()

        let MAX_CTIME = 2
        q.Enqueue((start, MAX_CTIME, 0, ZERO, None, None, false), 0)

        let mutable results = []

        let mutable lastId = ZERO

        while (q.Count > 0) do
            let cp, ct, time, id, cheatStart, cheatEnd, cheatActive = q.Dequeue()

            if cp = target then
                results <- results @ [ time ]
            else if
                ((if (ct = 0 && cheatActive) then
                      not (cts.Contains((cheatStart.Value, cheatEnd.Value)))
                  else
                      true)
                 && (ct > 0 || ct = 0 && (gr[cp.y][cp.x] <> '#'))
                 && (shortestAllowedTime.IsNone || time < shortestAllowedTime.Value - 1)
                 && (not (visited.ContainsKey(cp, ct, id)) || (time < visited[(cp, ct, id)])))
            then
                if (ct = 0 && cheatActive) then
                    cts.Add((cheatStart.Value, cheatEnd.Value))

                visited[(cp, ct, id)] <- time

                let newTime = time + 1

                if not useCheatTime || not (ct > 0 && ct <> MAX_CTIME) then
                    let choices =
                        neighbourOffsets
                        |> List.map (fun offs -> (offs + cp))
                        |> List.where (fun np ->
                            (np.y >= 0
                             && np.y <= rows
                             && np.x >= 0
                             && np.x <= cols
                             && (gr[np.y][np.x] <> '#')
                             && (shortestAllowedTime.IsNone
                                 || (np.GetManhattanDistance(target) + time) < shortestAllowedTime.Value)))

                    choices
                    |> List.iter (fun np -> q.Enqueue((np, ct, newTime, id, cheatStart, cheatEnd, false), newTime + np.GetManhattanDistance(target)))

                if useCheatTime && ct > 0 then
                    neighbourOffsets
                    |> List.map (fun offs -> (offs + cp))
                    |> List.where (fun np ->
                        (np.y >= 0 && np.y <= rows && np.x >= 0 && np.x <= cols)
                        && if ct = MAX_CTIME then gr[np.y][np.x] = '#' else true
                        && (np.GetManhattanDistance(target) + time) < shortestAllowedTime.Value)
                    |> List.iter (fun np ->
                        let cheatStart = if (ct = MAX_CTIME) then Some(cp) else cheatStart
                        let cheatEnd = if (ct - 1 = ZERO) then Some(cp) else cheatEnd

                        lastId <- lastId + ONE
                        q.Enqueue((np, ct - 1, newTime, (if ct = MAX_CTIME then lastId else id), cheatStart, cheatEnd, true), newTime + np.GetManhattanDistance(target)))

        results

    let shortestPathWithoutCheats = fp start target false None |> List.head
    printfn $"%A{shortestPathWithoutCheats}"

    let allPathsWithCheats =
        fp start target true (Some(shortestPathWithoutCheats))
        |> List.map ((-) shortestPathWithoutCheats)
        |> List.groupBy id
        |> List.rev


    let mutable a = ZERO

    for p in allPathsWithCheats do
        printfn $"%A{(fst p, snd p |> Seq.length)}"

        if ((fst p) >= 100) then
            a <- a + ((snd p |> Seq.length) |> double)

    printfn $"%A{a}"

    (*
    printfn $"%A{(start, target)}"
    printfn $"%A{r}"
    *)

    (*
    for y in 0..rows do
        for x in 0..cols do
            if (v |> List.contains (Point2D(x, y))) then
                printf $"O"
            else
                printf $"%c{(gr[y][x])}"
        printfn ""
        *)



    0
