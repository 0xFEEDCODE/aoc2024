module aoc2024.helper

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open Microsoft.Extensions.Configuration
open System.Net.Http

open System.Threading


type Helper() = 
    let httpClient = new HttpClient()

    let sessionToken = 
        let secrets = ConfigurationBuilder().AddUserSecrets(Assembly.GetExecutingAssembly()).Build()
        (secrets.AsEnumerable() |> Seq.find(fun s -> s.Key = "tokenAOC")).Value

    let getCallerModuleName() = 
        let st = StackTrace();
        let mutable i = 0;
        while(st.GetFrame(i).GetMethod().ReflectedType.Name = "Helper") do
            i <- i + 1
        st.GetFrame(i).GetMethod().ReflectedType.Name
        
    let getPathToInputFile moduleName =
        Path.Combine(Directory.GetParent(Environment.CurrentDirectory).Parent.Parent.FullName, moduleName, "inp.txt")

        
    member this.readInput() =
        let moduleName = getCallerModuleName()
        File.ReadLines(getPathToInputFile moduleName)
        
    member this.getInput(?overrideIfExists: bool) =
        let overrideIfExists = defaultArg overrideIfExists false
        
        let moduleName = getCallerModuleName()
        let pathToInputFile = getPathToInputFile moduleName
        
        let mutable shouldDownloadInput = true
        if(not overrideIfExists) then
            if(File.Exists pathToInputFile) then
                shouldDownloadInput <- false
            
        if shouldDownloadInput then
            printfn "Downloading"
            let urlDayPart = if moduleName.StartsWith "day0" then moduleName[4..] else moduleName[3..]
            let url = $"https://adventofcode.com/2024/day/{urlDayPart}/input"
            let request = new HttpRequestMessage(HttpMethod.Get, url)
            request.Headers.Add("Cookie", $"session={sessionToken}")
            let cts = new CancellationTokenSource()
            let response = httpClient.Send request
            if(not response.IsSuccessStatusCode) then
                failwith $"Could not get input, http error - {response.StatusCode}" 
            let content = response.Content.ReadAsStringAsync(cts.Token)
            content |> Async.AwaitTask |> ignore
            File.WriteAllText(pathToInputFile, content.Result)
            
        this.readInput()
        
    member this.submitAnswer part answer =
        let moduleName = getCallerModuleName()
        
        let urlDayPart = if moduleName.StartsWith "day0" then moduleName[4..] else moduleName[3..]
        let url = $"https://adventofcode.com/2024/day/{urlDayPart}/answer"
        let request = new HttpRequestMessage(HttpMethod.Post, url)
        request.Headers.Add("Cookie", $"session={sessionToken}")
        
        let payload = List<KeyValuePair<string, string>>()
        payload.Add(KeyValuePair<string, string>("level", part.ToString()));
        payload.Add(KeyValuePair<string, string>("answer", answer.ToString()));
        request.Content <- new FormUrlEncodedContent(payload);
        
        let cts = new CancellationTokenSource()
        let response = httpClient.Send request
        if(not response.IsSuccessStatusCode) then
            failwith $"Could submit answer, http error - {response.StatusCode}" 
        let content = response.Content.ReadAsStringAsync(cts.Token)
        content |> Async.AwaitTask |> ignore
        
        let html = content.Result
        let pattern = "(?i)(?s)<article><p>(?<content>.*?)</p></article>"
        let m = Regex.Match(html, pattern)
        if m.Success then
            let content = m.Groups.["content"].Value
            printfn $"%A{content}"
            content
        else
            failwith "wtf"
        
        
let helper = Helper()
