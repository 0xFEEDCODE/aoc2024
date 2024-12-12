module aoc2024.day12

open System.Collections.Generic
open aoc2024.util

let solve() =
    let io = aocIO
    let inp = io.getInput(false)
    
    let gr = Grid.initializeFromStringSeq inp
    
    let wb (p: Point2D) =
        let (x,y) = (p.x, p.y)
        (x >= 0 && x <= (gr[0].Length-1) && y >= 0 && y <= (gr.Length-1))
    
    let d = Dictionary<int * int, List<char>>()

    
    let getAP (yx) ch =
        let (y,x) = yx
        let sp = Point2D(x,y)
        
        let explore = Queue<Point2D>()
        let visited = List<Point2D>()
        let mutable region = List<Point2D>()
        
        let calcP() =
            let mutable s = 0
            for p in region do
                for np in Grid.getAdjacentNeighbours p do
                    if (wb np) then
                        if gr[np.y][np.x] <> ch then
                            s <- s+1
                    else
                        s <- s+1
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
                d[(p.y,p.x)] <- new List<char>()
            d[(p.y, p.x)].Add(ch))
        let p = calcP()
        //printfn $"%A{(region.Count, p, ch, region.Count*p)}"
        (region.Count, p) 
            
            
   
    let getBP (yx) ch =
        let (y,x) = yx
        let sp = Point2D(x,y)
        
        let explore = Queue<Point2D>()
        let visited = List<Point2D>()
        let mutable region = List<Point2D>()
        
        let side = List<Point2D>()
        
                                (*
                                let mutable nn = 0
                                for np in Grid.getAdjacentNeighbours c do
                                    if not (visited.Contains(np)) && wb np then
                                        nn <- nn + 1
                                let isOuter = nn > 0
                                        
                                printfn $"%A{(c, isOuter)}"
                                *)
                                
                                
        let getE (region: List<Point2D>) =
            let allNeigh = List<Point2D>()
            for p in region do
                for np in Grid.getAdjacentNeighbours p do
                    if not (allNeigh.Contains(np)) && not (region.Contains(np)) then
                        allNeigh.Add(np)
                        
            let dir = [ (-1, 0); (1, 0); (0, -1); (0, 1) ] |> Seq.map Point2D
            
            let considered = List<Point2D>()
            let mutable s = 0
            for n in allNeigh do
                if not (considered.Contains(n)) then
                    for d in dir do
                        let mutable np = d + n
                        let mutable m = 1
                        while not (considered.Contains(np)) && allNeigh.Contains(np) do
                            m <- m + 1
                            considered.Add(np)
                            np <- Point2D(d.x*m + n.x, d.y*m + n.y)
                    s <- s + 1
                    (*
                    printfn $"%A{(n,s)}"
            printfn $"X"
            *)
            for n in allNeigh do
                let mutable ls = 0
                for np in Grid.getAdjacentNeighbours n do
                    if wb(np) && region.Contains(np) && not (considered.Contains(n)) then
                        ls <- ls + 1
                if (ls > 1) then
                    s <- s + (if ls = 1 then 1 else 2)
                    printfn $"%A{(n, ls)}"
            printfn $"%A{(ch, s)}"
            s
                    

        
        let calcP() =
            let edges = Queue<Point2D>()
            let mutable s = 0
            edges.Enqueue(sp)
            let exp = Queue<Point2D * Point2D>()
            let procEdges = List<Point2D>()
            visited.Clear()
            
            let mutable s = 0
     
                            
                //printfn $"%A{s}"
                    
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
                d[(p.y,p.x)] <- new List<char>()
            d[(p.y, p.x)].Add(ch))
        let p = calcP()
        //printfn $"%A{(region.Count, p, ch, region.Count*p)}"
        let p = getE (region)
        (region.Count, p) 
            
        
    
    let mutable a1 = 0
    let mutable a2 = 0
    
    (*
    for y in 0..(gr.Length-1) do
        for x in 0..(gr[0].Length-1) do
            let ch = gr[y][x]
            let idx = (y,x)
            if not (d.ContainsKey((y,x))) || not (d[y,x].Contains(ch)) then
                let (a,p) = getAP (y,x) ch
                a1 <- a1 + (a*p)
                *)
            
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