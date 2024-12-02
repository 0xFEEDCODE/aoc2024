module aoc2024.day02
open aoc2024.util

let isSafe nums =
    let ascending = (nums |> Seq.head) < (nums |> Seq.last)
    
    nums
    |> Seq.scan
        (fun (prev: Option<int>) (curr: int) ->
            let diffOption = if prev.IsNone then None else Some(curr-prev.Value)
            
            match diffOption with
            | Some(diff) when abs diff > 0 && abs diff < 4 && ((diff > 0) = ascending) ->
                Some(curr)
            | None when prev.IsNone ->
                Some(curr)
            | _ ->
                None
        ) None
    |> Seq.skip 1
    |> Seq.takeWhile(_.IsSome)
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
