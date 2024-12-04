module aoc2024.day03

open aoc2024.util
open System

let processMul i (data: string) =
        let mutable i = i
        &i += 4
        let mutable content = ""
        while(data[i] <> ')') do
            content <- content + (data[i] |> string)
            &i += 1
        if content.Contains ',' then
            let spl = content.Split ','
            // check if spl[0] and spl[1] can be parsed to int
            if (spl.Length = 2 && spl[0] |> Seq.forall Char.IsDigit && spl[1] |>Seq.forall Char.IsDigit) then
                Some((spl[0] |> int) * (spl[1] |> int))
            else
                None
        else
            None
    

let solve() =
    let inp = aocIO.getInput() |> Seq.toArray |> String.Concat
    
    let mutable s = 0I
    let mutable i = 0
    
    let mutable isDo = true
    
    while i < (inp |> Seq.length) do
        if isDo then
            if inp[i..i+3] = "mul(" then
                let r = processMul i inp
                if r.IsSome then
                    s <- s + (r.Value |> bigint)
        if (inp[i..i+3] = "do()") then
            isDo <- true
        
        if (inp[i..i+6] = "don't()") then
            isDo <- false
            
        &i += 1
            
        
    printfn $"%A{s}"
    
    //aocIO.submitAnswer 2 s
        
    0
