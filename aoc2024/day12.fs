module aoc2024.day12

open System
open System.Collections.Generic
open System.Drawing
open Microsoft.FSharp.Core
open aoc2024.util


type Plane =
    | X of int
    | Y  of int

let solve() =
    let io = aocIO
    let inp = io.getInput(false)
    
    let gr = Grid.initializeFromStringSeq inp
    
    let wb (p: Point2D) =
        let (x,y) = (p.x, p.y)
        (x >= 0 && x <= (gr[0].Length-1) && y >= 0 && y <= (gr.Length-1))
    
    let d = Dictionary<int * int, List<char>>()
    
    let opD (p: Point2D) =
        Point2D(p.x * -1, p.y * -1)
        
        
    let getBP yx ch =
        let y,x = yx
        let sp = Point2D(x,y)
        
        let explore = Queue<Point2D>()
        let mutable region = List<Point2D>()
        let visited = List<Point2D>()
        
        let dirs = [ (-1, 0); (1, 0); (0, -1); (0, 1) ] |> Seq.map Point2D
        
        let getLines (src: Point2D list) =
            let mutable r = []
            let mutable cons = []
            
            for x in src do
                let mutable points = [x]
                for y in src do
                    if (x <> y) then
                        if not (cons |> List.contains y) then
                            if points |> List.tryFind(fun p -> (p |> Grid.getAdjacentNeighbours) |> Seq.contains y) |> Option.isSome then
                                points <- points @ [y]
                                
                if points.Length = 1 then
                    if not (cons |> List.contains (points[0])) then
                        r <- r @ [points]
                else
                    cons <- cons @ points
                    r <- r @ [points]
            r
        
        let conv (d: Point2D) =
            match (d.x, d.y) with
            | -1,0 -> Point2D(0, -1)
            | 0, -1 -> Point2D(1, 0)
            | 1, 0 -> Point2D(0, 1)
            | 0, 1 -> Point2D(-1, 0)
        
        let gl (neighs : Point2D list) =
            let mutable lines = []
            let mutable cons = []
            let mutable sides = 0
            if(ch='A') then
                ()
            
            let mutable s = 0
            for neigh in neighs do
                let ds = dirs |> Seq.where(fun d ->
                    let np = Point2D(neigh.x+d.x, neigh.y+d.y)
                    wb np && region.Contains(np)) |> Seq.map conv
                
                let dsq = ds |> Seq.tryFind(fun d -> not (cons |> List.contains (neigh, d)))
                
                if dsq.IsSome then
                    let dsq = dsq.Value
                    let mutable curr = neigh
                    
                    let mutable any = region.Contains (Point2D(curr.x+dsq.x, curr.y+dsq.y))
                    if any then
                        s <- s + 1
                        
                    while(any && region.Contains (Point2D(curr.x+dsq.x, curr.y+dsq.y))) do
                        cons <- cons @ [(curr, dsq)]
                        let adjNeigh = curr |> Grid.getAdjacentNeighbours |> Seq.where(fun x -> (neighs |> List.contains x))
                        let r = adjNeigh |> Seq.tryFind(fun x -> not (cons |> List.contains (x, dsq)) && region.Contains(Point2D(x.x+dsq.x, x.y+dsq.y)))
                        if r.IsSome then
                            curr <- r.Value
                        else
                            any <- false
            s
            
        let addExplore p =
            visited.Add(p)
            for np in Grid.getAdjacentNeighbours p do
                if not (visited.Contains(np)) && wb np then
                    explore.Enqueue(np)
        
        if not(d.ContainsKey(y,x)) then
            d[(y,x)] <- List<char>()
            
        addExplore sp
        region.Add sp
        
        while(explore.Count > 0) do
            let c = explore.Dequeue()
            if(not (visited.Contains(c)) &&
               gr[c.y][c.x] = ch) then
                addExplore c
                region.Add(c)
                    
        region.ForEach(fun p ->
            if not (d.ContainsKey((p.y, p.x))) then
                d[(p.y,p.x)] <- List<char>()
            d[(p.y, p.x)].Add(ch))
        
        let mutable allNeigh = List.empty
        for p in region do
            for np in Grid.getAdjacentNeighbours p do
                if not (allNeigh |> List.contains(np)) && not (region.Contains(np)) then
                    allNeigh <- allNeigh @ [np]
                    
        let p = gl allNeigh
        printfn $"%A{(ch, p)}"
        (region.Count, p) 
            
        
    let mutable a1 = 0
    let mutable a2 = 0
    
    for y in 0..(gr.Length-1) do
        for x in 0..(gr[0].Length-1) do
            let ch = gr[y][x]
            let idx = (y,x)
            if not (d.ContainsKey((y,x))) || not (d[y,x].Contains(ch)) then
                let (a,p) = getBP (y,x) ch
                a1 <- a1 + (a*p)
            
    
    printfn $"%A{a1}"
    //io.submitAnswer 1 a1
    //io.submitAnswer 2 a2
    0