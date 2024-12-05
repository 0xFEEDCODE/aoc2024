module aoc2024.day04
open aoc2024.util

let solve() =
    let io = aocIO
    let inp = io.getInput()
    
    let nCols = (inp |> Seq.head |> Seq.length)
    let nRows = (inp |> Seq.length)
    let gr = inp |> Seq.map(fun l -> l |> Seq.toArray) |> Seq.toArray
        
    let directionOffsets = [ (0, 1); (1, 0); (1, 1); (-1, 1); (-1, 0); (0, -1); (-1, -1); (1, -1) ]
    let xmas = "XMAS"
        
    let getNXmasOccurrences x y =
        (directionOffsets |> Seq.where(fun (offY, offX) ->
                    {0..(xmas.Length-1)} |> Seq.forall(fun i ->
                        let nX, nY = ((x + (offX*i)), (y + (offY*i)))
                        ((nX >= 0 && nX < nCols && nY >= 0 && nY < nRows) && gr[nX][nY] = xmas[i]))
            ) |> Seq.length)
        
    let ans1 = seq {0..(nRows-1)} |> Seq.sumBy(fun y -> seq {0..(nCols-1)} |> Seq.sumBy(fun x -> getNXmasOccurrences x y))
        
    printfn $"%A{ans1}"
    
    let isXmas2 x y =
        gr[x][y] = 'A' &&
        (x-1 >= 0 && x+1 < nCols && y-1 >= 0 && y+1 < nRows) &&
        ((gr[x-1][y-1] = 'M' && gr[x-1][y+1] = 'M' && gr[x+1][y-1] = 'S' && gr[x+1][y+1] = 'S') || 
        (gr[x-1][y-1] = 'S' && gr[x-1][y+1] = 'S' && gr[x+1][y-1] = 'M' && gr[x+1][y+1] = 'M') ||
        (gr[x-1][y-1] = 'M' && gr[x-1][y+1] = 'S' && gr[x+1][y-1] = 'M' && gr[x+1][y+1] = 'S') ||
        (gr[x-1][y-1] = 'S' && gr[x-1][y+1] = 'M' && gr[x+1][y-1] = 'S' && gr[x+1][y+1] = 'M')) 
            
    let ans2 = seq {0..(nRows-1)} |> Seq.sumBy(fun y -> seq {0..(nCols-1)} |> Seq.where(fun x -> isXmas2 x y) |> Seq.length)
    
    printfn $"%A{ans2}"
    0