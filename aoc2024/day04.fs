module aoc2024.day04

open System.Collections.Generic
open aoc2024.util


let solve() =
    let io = aocIO
    let inp = io.getInput(true)
    
    let nCols = (inp |> Seq.head |> Seq.length)
    let nRows = (inp |> Seq.length)
    
    let gr = Grid.createGrid nRows nCols
    
    let mutable r = 0
    for l in inp do
        let mutable c = 0
        for ch in l do
            gr[c][r] <- ch
            &c += 1
        &r += 1
        
    (*
    let dirsF = [ (0, 1); (1, 0); (1, 1); (-1, 1);  ]
    let dirsB = [ (-1, 0); (0, -1); (-1, -1); (1, -1);]
    *)
    let dirs = [ (0, 1); (1, 0); (1, 1); (-1, 1); (-1, 0); (0, -1); (-1, -1); (1, -1) ]

    let xmas = "XMAS"
    let samx = "SAMX"
    let variations = [xmas; samx]
    
    let mutable markedPos = List<string>()
    
    (*
    let mutable ans1 = 0
    
    for y in 0..(nRows-1) do
        for x in 0..(nCols-1) do
            if (gr[x][y] = 'X' || gr[x][y] = 'S') then
                let v = variations |> Seq.find(fun v -> v[0] = gr[x][y])
                let mutable found = false
                for offY,offX in dirs do
                    let mutable isValid = true
                    for i in 0..3 do
                        let nxtX = (x + (offX*i))
                        let nxtY = (y + (offY*i))
                        if not ((nxtX >= 0 && nxtX < nCols && nxtY >= 0 && nxtY < nRows) &&
                                gr[nxtX][nxtY] = v[i]) then
                            isValid <- false
                    if isValid then
                        let nY = (y + (offY*3))
                        let nX = (x + (offX*3))
                        let k = (x.ToString()+"_"+y.ToString()+"||"+(nX).ToString()+"_"+(nY).ToString())
                        if (not (markedPos.Contains(k))) then
                            &ans1 += 1
                            markedPos.Add(k)
                            
                            let k2 = (nX.ToString()+"_"+nY.ToString()+"||"+(x).ToString()+"_"+(y).ToString())
                            markedPos.Add(k2)
                            found <- true
        
    printfn $"%A{ans1}"
    *)
    
    let mutable ans2 = 0
    
    let mutable markedPos2 = List<string>()
    
    for y in 0..(nRows-1) do
        for x in 0..(nCols-1) do
            if (gr[x][y] = 'A') then
                if(x-1 >= 0 && x+1 < nCols && y-1 >= 0 && y+1 < nRows) then
                    if(gr[x-1][y-1] = 'M' && gr[x-1][y+1] = 'M' &&
                      gr[x+1][y-1] = 'S' && gr[x+1][y+1] = 'S') then
                        &ans2 += 1
                    else if(gr[x-1][y-1] = 'S' && gr[x-1][y+1] = 'S' &&
                      gr[x+1][y-1] = 'M' && gr[x+1][y+1] = 'M') then
                        &ans2 += 1
                    else if(gr[x-1][y-1] = 'M' && gr[x-1][y+1] = 'S' &&
                      gr[x+1][y-1] = 'M' && gr[x+1][y+1] = 'S') then
                        &ans2 += 1
                    else if(gr[x-1][y-1] = 'S' && gr[x-1][y+1] = 'M' &&
                      gr[x+1][y-1] = 'S' && gr[x+1][y+1] = 'M') then
                        &ans2 += 1
    
    //io.submitAnswer 1 ans1
    io.submitAnswer 2 ans2
    printfn $"%A{ans2}"
    0