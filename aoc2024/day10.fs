module aoc2024.day10

open System
open System.Collections.Generic
open aoc2024.day06
open aoc2024.util


    
    (*
    frontier = PriorityQueue()
frontier.put(start, 0)
came_from = dict()
cost_so_far = dict()
came_from[start] = None
cost_so_far[start] = 0

while not frontier.empty():
   current = frontier.get()

   if current == goal:
      break
   
   for next in graph.neighbors(current):
      new_cost = cost_so_far[current] + graph.cost(current, next)
      if next not in cost_so_far or new_cost < cost_so_far[next]:
         cost_so_far[next] = new_cost
         priority = new_cost
         frontier.put(next, priority)
         came_from[next] = current
         *)

let solve () =
    let io = aocIO
    let inp = io.getInput()
    
    let gr = inp |> Grid.initializeFromStringSeq |> Seq.map (fun x -> x |> Seq.map (fun x -> Int16.Parse(x.ToString())) |> Seq.map int |> Seq.toArray) |> Seq.toArray
    
    let findPaths (start: int * int) (target: int * int) =
        let (sy,sx) = start
        let mutable f = PriorityQueue()
        
        let ey, ex = target
        let start = Point2D(sx, sy)
        let target = Point2D(ex, ey)
        
        let mutable cameFrom = Dictionary<Point2D, Option<Point2D>>()
        let mutable costSoFar = Dictionary<Point2D, int>()
        
        cameFrom.Add(start, None)
        costSoFar.Add(start, 0)
        f.Enqueue(start, 0)
        
        let mutable nTrails = 0
        
        let mutable found = false
        while not found && f.Count > 0 do
            let current = f.Dequeue()
            
            if current = target then
                &nTrails += 1
                
            for n in Grid.getAdjacentNeighbours(current) do
                if((n.y >= 0 && n.y < gr.Length && n.x >= 0 && n.x < (gr[0] |> Seq.length)) &&
                    (gr[n.y][n.x] - gr[current.y][current.x]) = 1) then
                    
                    let newCost = costSoFar[current] + 1
                    if not (costSoFar.ContainsKey n) || newCost < costSoFar[n] then
                        costSoFar[n] <- newCost
                        f.Enqueue(n, newCost)
                        cameFrom[n] <- Some(current)
                else
                    ()
                    
        if nTrails > 0 then
            Some(nTrails)
        else
            None
            
    let findPaths2 (start: int * int) (target: int * int) =
        let (sy,sx) = start
        let mutable f = Queue()
        
        let ey, ex = target
        let start = Point2D(sx, sy)
        let target = Point2D(ex, ey)
        
        let mutable cameFrom = Dictionary<Point2D, Option<Point2D>>()
        
        cameFrom.Add(start, None)
        f.Enqueue(start)
        
        let mutable reached = List<Point2D>()
        
        let mutable nTrails = 0
        
        let mutable found = false
        while not found && f.Count > 0 do
            let current = f.Dequeue()
            
            if current = target then
                &nTrails += 1
                
            for n in Grid.getAdjacentNeighbours(current) do
                if(
                    (n.y >= 0 && n.y < gr.Length && n.x >= 0 && n.x < (gr[0] |> Seq.length)) &&
                    (gr[n.y][n.x] - gr[current.y][current.x]) = 1) then
                    
                    reached.Add(n)
                    f.Enqueue(n)
                    
        if nTrails > 0 then
            Some(nTrails)
        else
            None
        
    let trailheads = List<int * int>()
    let trailtails = List<int * int>()
    for y in 0..gr.Length-1 do
        for x in 0..gr[0].Length-1 do
            if(gr[y][x] = 0) then
                trailheads.Add((y,x))
            if(gr[y][x] = 9) then
                trailtails.Add((y,x))
                
    let mutable a1 = 0
    let mutable a2 = 0
    for h in trailheads do
        for t in trailtails do
            let sc = findPaths h t
            if (sc.IsSome) then
                &a1 += 1
            let sc = findPaths2 h t
            if (sc.IsSome) then
                &a2 += sc.Value
        
    printfn $"%A{a1}"
    printfn $"%A{a2}"
        
    //io.submitAnswer 1 a1
    //io.submitAnswer 2 a2
    
    0
