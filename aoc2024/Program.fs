open System.Diagnostics
open aoc2024

let sw = Stopwatch()

sw.Start()
//day01.solve()
day02.solve()
sw.Stop()

printfn $"Time taken - %A{sw.Elapsed}"
