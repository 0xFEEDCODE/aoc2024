module aoc2024.day08

open System.Collections.Generic
open aoc2024.util


//let p = [for x in xs do for y in ys -> x, y]

let solve() =
    let io = aocIO
    let inp = io.getInput()
    
    let gr = inp |> Grid.initializeFromStringSeq
    
    let mutable (a1, a2) = (0, 0)
    
    let an = HashSet()
    
    let pr() = 
        for y in 0..gr.Length-1 do
            for x in 0..(gr[0].Length-1) do
                if (an.Contains (y,x)) then
                    printf $"#"
                else
                    printf $"{gr[y][x]}"
            printfn $""
            
    let addN (y3: double) y1 y2 x1 x2 b m = 
        let x3 = ((y3-b)/m)
        
        if(y3 <> y2 && y3 <> y1 && y3 >= 0 && y3 <= double(gr.Length-1)) then
            let ny = y3 |> int
            let nx = (round x3) |> int
            an.Add(ny, nx) |> ignore
    
    let withinBounds y x =
        let y = y |> int
        let x = x |> int
        x >= 0 && x <= (gr.Length-1) && y >= 0 && y <= (gr[0].Length-1)
    
    let addPoint sx sy (x: int) (y: int) (dist: int) = 
        let x1, y1 = ((double sx), (double sy))
        let x2, y2 = ((double x), (double y))
        let dy = y2 - y1
        let dx = x2 - x1
        printfn $"%A{(dx,dy)}"
        
        let m = (double dy)/(double dx)
        let b = y1 - (m*x1)
        
        (*
        an.Add((int y1, int x1))
        an.Add((int y2, int x2))
        let mutable mul = 1.
        while (withinBounds (int (round(mul*dy+y2))) (int (round(mul*dx+x2)))) do
            an.Add(int (round(mul*dy+y2)), int (round(mul*dx+x2))) |> ignore
            &mul+=1.
                
        let mutable mul = 1.
        while (withinBounds (int (round(y1 - mul*dy))) (int (round(x1 - mul*dx)))) do
            an.Add(int (round(y1 - mul*dy)), int (round(x1 - mul*dx))) |> ignore
            &mul+=1.
            *)
        ()
        
        
        let y3 = double(y + dist)
        addN y3 y1 y2 x1 x2 b m
        let y3 = double(sy - dist)
        addN y3 y1 y2 x1 x2 b m 
        
    let sp sy sx freq =
        let mutable y = sy
        let mutable dist = 0
        
        for y in 0..gr.Length-1 do
            for x in 0..(gr[0].Length-1) do
                if (gr[y][x] = freq && sx <> x && sy <> y) then
                    addPoint sx sy x y (abs (sy-y))
        (*
        while (y > 0) do
            &y -= 1
            for x in 0..(gr[0].Length-1) do
                if(gr[y][x] = freq) then
                    addPoint sx sy x y (abs (sy-y))
                    
        dist <- 0
        for y in (sy+1)..(gr.Length-1) do
            for x in 0..(gr[0].Length-1) do
                if(gr[y][x] = freq) then
                    addPoint sx sy x y (abs (sy-y))
                    *)
                        
    
    for y in 0..gr.Length-1 do
        for x in 0..(gr[0].Length-1) do
            if (gr[y][x] <> '.') then
                let freq = gr[y][x]
                sp y x freq
                
    (*
    for (y,x) in an do
        printfn $"%A{(gr[y][x])}"
    *)
    pr()
    printfn $"%A{an.Count}"
    
    //io.submitAnswer 1 an.Count
    //io.submitAnswer 2 a2
    0
