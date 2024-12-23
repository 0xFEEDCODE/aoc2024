module aoc2024.day23

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Threading.Tasks
open aoc2024.util

let solve () =
    let io = aocIO
    let inp = io.getInput ()

    let directConnections = Dictionary<char, Set<char>>()

    let mutable all = Set.empty
    let mutable connPairs = []

    let mutable idMapping = Dictionary<string, char>()
    let mutable idToName = Dictionary<char, string>()
    let mutable nextId = char 'a'

    for line in inp do
        let spl = line.Split '-'

        connPairs <- connPairs @ [ spl[0], spl[1] ]

        for c1 in spl do

            if not (idMapping.ContainsKey(c1)) then
                idMapping[c1] <- nextId
                idToName[nextId] <- c1
                nextId <- nextId + char 1

            let c1Id = idMapping[c1]

            all <- all.Add(c1Id)

            if not (directConnections.ContainsKey(c1Id)) then
                directConnections[c1Id] <- Set.empty

            for c2 in spl do
                if not (idMapping.ContainsKey(c2)) then
                    idMapping[c2] <- nextId
                    idToName[nextId] <- c2
                    nextId <- nextId + char 1

                let c2Id = idMapping[c2]

                if c1 <> c2 then
                    directConnections[c1Id] <- directConnections[c1Id].Add(c2Id)

                    if not (directConnections.ContainsKey(c2Id)) then
                        directConnections[c2Id] <- Set.empty

                    directConnections[c2Id] <- directConnections[c2Id].Add(c1Id)

    let mutable sets = []

    all <- Set(all |> Seq.sort)

    (*
    let a1 =
        let mutable a1 = 0

        for c1 in all do
            for c2 in all do
                for c3 in all do
                    if (c1 <> c2 && c2 <> c3) then
                        if
                            directConnections[c1].Contains c2
                            && directConnections[c1].Contains c3
                            && directConnections[c2].Contains c1
                            && directConnections[c2].Contains c3
                            && directConnections[c3].Contains c1
                            && directConnections[c3].Contains c2
                        then
                            let ordered = [ c1; c2; c3 ] |> List.sortDescending


                            if not (sets |> List.contains ordered) then
                                if (ordered |> Seq.tryFind (fun x -> x.StartsWith "t") |> Option.isSome) then
                                    a1 <- a1 + 1

                                sets <- sets @ [ ordered ]

        a1*)



    let translateSeq ps =
        let r =
            ps
            |> Seq.map (fun x -> idToName[x])
            |> Seq.sortDescending
            |> Seq.rev
            |> Seq.fold (fun acc x -> if acc |> Seq.length > 0 then acc + "," + x else x) ""

        r

    let mutable memo = ConcurrentDictionary<char seq, bool>()

    let rec areInterconnected pcs =
        let originalPcs = pcs

        let rec loop (pcseq: char list) (processed: char list) =
            let key = pcseq

            if processed.Length > 0 then
                memo[processed] <- true

            if memo.ContainsKey(key) then
                memo[key]
            else
                match pcseq with
                | [] ->
                    memo[originalPcs] <- true
                    memo[key] <- true
                    true
                | [ _ ] ->
                    memo[originalPcs] <- true
                    memo[key] <- true
                    true
                | h :: tail ->
                    let allInterconnected =
                        tail
                        |> Seq.forall (fun pc ->
                            (directConnections[h] |> Set.contains pc)
                            || (directConnections[pc] |> Set.contains h))

                    if allInterconnected then
                        loop tail (processed @ [ h ])
                    else
                        memo[originalPcs] <- false
                        memo[key] <- false
                        memo[processed] <- false
                        false

        loop pcs []


    let mutable bestScore = Int32.MinValue
    let mutable bestValue = []

    let rec sortedInsert (originalList: 'a list) newItem =
        match originalList with
        | [] -> [ newItem ]
        | head :: tail when newItem > head -> head :: sortedInsert tail newItem
        | head :: tail -> newItem :: sortedInsert tail head

    let findInterconnected pc =
        let cons = ConcurrentDictionary<char seq, byte>()

        let pq = PriorityQueue()
        pq.Enqueue([ pc ], 0)

        while (pq.Count > 0) do
            let pcs = pq.Dequeue()

            let remainingPCS = all |> Seq.where (fun p -> not (pcs |> List.contains p))
            let mutable nRemaining = remainingPCS |> Seq.length

            if not (pcs.Length + nRemaining < bestScore) then
                for remainingPc in remainingPCS do
                    let newPcs = sortedInsert pcs remainingPc

                    let key = newPcs

                    if not (cons.ContainsKey(key)) then
                        cons[key] <- byte 0

                        if areInterconnected newPcs then

                            if (newPcs.Length > bestScore) then
                                bestScore <- newPcs.Length
                                bestValue <- newPcs
                                let translated = translateSeq bestValue
                                printfn $"%A{(translated, translated.Length)}"

                            pq.Enqueue(newPcs, newPcs.Length * -1)

                    nRemaining <- nRemaining - 1

    let mutable i = 0

    let skip = 90
    let ac = all.Count - skip

    Parallel.ForEach(
        all,
        findInterconnected
    )
    |> ignore

    (*
    for pc in all |> Seq.skip skip do
        i <- i + 1
        printfn $"%A{(i, ac-i)}"
        
        findInterconnected pc
        let translated = translateSeq bestValue
        printfn $"%A{(translated, translated.Length)}"
        ()
        *)

    0
