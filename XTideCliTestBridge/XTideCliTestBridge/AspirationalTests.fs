namespace XTideCliTestBridge
open System
open NUnit.Framework
open TideMonkey

(*
[<TestFixture>]
[<Ignore("Use-case: Will break until infrastructure is complete")>]
type Test() = 


    [<Test>]
    member x.TideMonkeyWorking() =
        let harmonicsPath = "/Users/larryobrien/Documents/src/3rd party/MX-Tides-iOS/resources/harmonics-dwf-20121224-free.tcd" 

        //Implicitly asserting that this is Some
        let stationDB = Station.LoadStations(harmonicsPath).Value
        Assert.IsTrue(fun _ -> stationDB.Length > 100)
        
        for _ in 1..100 do
            let randomStationName = stationDB |> Seq.map (fun s -> Station.Named s stationDb)  |> List.ofSeq |> RandomElement
            let aStation = stationDB.[randomStationName]

            for _ in 1..100 do
                let randomTime = rand.NextInt()
                let prediction = aStation.Predict randomTime
                //validate against XTide cli
                Assert.IsTrue(true)
*)