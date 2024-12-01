open System.Diagnostics

let sw = Stopwatch()

sw.Start()
//day01.solve()
sw.Stop()

printfn $"Time taken - %A{sw.Elapsed}"
