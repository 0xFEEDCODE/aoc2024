module aoc2024.day06

open System.Collections.Generic
open aoc2024.util

type Dir =
    | U
    | D
    | L
    | R
    
let ndir d =
    match d with
    | U -> R
    | R -> D
    | D -> L
    | L -> U

let solve() =
    let io = aocIO 
    let inp = io.getInput ()
    
    let points = List<Point2D>()
    
    let mutable cd : Dir = U
    let mutable cp : Point2D = Point2D(0,0)
    
    let nRows = inp |> Seq.length
    let nCols = inp |> Seq.head |> Seq.length
    
    let mutable visited = Dictionary<string, int>()
    
    let mutable y = 0
    for l in inp do
        let mutable x = 0
        for ch in l do
            if(ch = '^') then
                cp <- Point2D(x,y)
            else if(ch = '#') then
                points.Add (Point2D(x,y))
            &x += 1
        &y += 1
    
    (*
    let mutable nSteps = 0
    while(cp.x >= 0 && cp.x < nCols && cp.y >= 0 && cp.y < nRows) do
        let np =
            match cd with
            | U -> Point2D(cp.x, cp.y-1)
            | D -> Point2D(cp.x, cp.y+1)
            | L -> Point2D(cp.x-1, cp.y)
            | R -> Point2D(cp.x+1, cp.y)
        if(points |> Seq.tryFind(fun x -> x = np) |> Option.isSome) then
            cd <- ndir cd
        else
            &nSteps += 1
            visited[cp] <- 1
            cp <- np
    
    printfn $"%A{visited.Count}"
    *)
    
    let alg sp npp =
        visited.Clear()
        let mutable cd : Dir = U
        let mutable cp : Point2D = sp
        
        let nRows = inp |> Seq.length
        let nCols = inp |> Seq.head |> Seq.length
        
        let mutable nSteps = 0
        let mutable detectedL = false
        while(cp.x >= 0 && cp.x < nCols && cp.y >= 0 && cp.y < nRows && not detectedL) do
            let np =
                match cd with
                | U -> Point2D(cp.x, cp.y-1)
                | D -> Point2D(cp.x, cp.y+1)
                | L -> Point2D(cp.x-1, cp.y)
                | R -> Point2D(cp.x+1, cp.y)
            if(np = npp || points |> Seq.tryFind(fun x -> x = np) |> Option.isSome) then
                let prevDir = cd
                cd <- ndir cd
                let key = np.ToString()+prevDir.ToString()+cd.ToString()
                if not (visited.ContainsKey key) then
                    visited[key] <- 0
                visited[key] <- visited[key] + 1
                if visited[key] > 1 then
                    detectedL <- true
            else
                &nSteps += 1
                cp <- np
        detectedL
    
    let mutable loops = 0
    let sp = cp
    for y in 0..nRows do
        for x in 0..nCols do
            let p = Point2D(x,y)
            if not (points.Contains(p)) && p <> sp then
                &loops += if (alg cp p) then 1 else 0
            printfn $"%A{(x,y, loops)}"
        
    //io.submitAnswer 2 loops
    printfn $"%A{loops}"
