module aoc2024.day02
open aoc2024.util

let isSafe nums =
    let ascending = (nums |> Seq.head) < (nums |> Seq.last)
    let mutable prev = None
    
    nums 
    |> Seq.takeWhile
        (fun num ->
            if prev.IsNone then
                prev <- Some(num)
                true
            else
                let curr = num
                let diff = curr - prev.Value
                prev <- Some(curr)
                (abs diff > 0 && abs diff < 4 && ((diff > 0) = ascending)))
    |> Seq.length
    |> (=) (nums |> Seq.length)
    
let solve() =
    
    let inp = aocIO.getInput()
    let ans1 =
        inp
        |> Seq.map String.extractAllNums
        |> Seq.where isSafe
        |> Seq.length
        
    let ans2 =
        inp
        |> Seq.map String.extractAllNums
        |> Seq.where (fun nums ->
            isSafe nums || ({-1..(nums.Length-1)} |> Seq.exists (fun i -> isSafe (Seq.append nums[..i] nums[(i+2)..] ))))
        |> Seq.length
        
    printfn $"%A{ans1}"
    printfn $"%A{ans2}"
