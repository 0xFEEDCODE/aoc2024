module aoc2024.day18

open System.Collections.Generic
open aoc2024.util


(*
frontier = Queue()
frontier.put(start )
came_from = dict()
came_from[start] = None

while not frontier.empty():
   current = frontier.get()

   if current == goal: 
      break           

   for next in graph.neighbors(current):
      if next not in came_from:
         frontier.put(next)
         came_from[next] = current
         *)

let solve () =
    let io = aocIO
    let inp = io.getInput ()

    let bs =
        inp
        |> Seq.map (fun x ->
            let n = x |> String.extractAllNums
            Point2D(n[0], n[1]))
        |> Seq.toList

    let R = (bs |> List.maxBy (_.y)).y
    let C = (bs |> List.maxBy (_.x)).x

    let fp t =
        let bs = bs |> List.take t
        let st = Point2D(0, 0)
        let e = Point2D(C, R)

        let q = PriorityQueue()
        let cameFrom = Dictionary<Point2D, Option<Point2D>>()
        cameFrom[st] <- None
        q.Enqueue((st, 0), 0)

        let mutable found = false

        let mutable res = None

        while not found && q.Count > 0 do
            let c, st = q.Dequeue()

            if c = e then
                res <- Some(st)

                found <- true

            let neigh =
                [ Point2D(-1, 0); Point2D(1, 0); Point2D(0, -1); Point2D(0, 1) ]
                |> List.map (fun np -> c + np)
                |> List.where (fun np ->
                    (np.y >= 0 && np.y <= R && np.x >= 0 && np.x <= C)
                    && not (cameFrom.ContainsKey(np))
                    && not (bs |> List.contains np))

            for n in neigh do
                let dist = n.GetManhattanDistance(e)
                q.Enqueue((n, st + 1), dist)
                cameFrom[n] <- Some(c)

        res


    let so = 0

    let r =
        Seq.initInfinite (fun x -> x + 1 + so)
        |> Seq.takeWhile (fun i ->
            printfn $"%A{i}"
            (fp i).IsSome)
        |> Seq.last

    printfn $"%A{r}"
    printfn $"%A{(bs[r - so])}"


    //io.submitAnswer 1 a1
    //io.submitAnswer 2 a2
    0
