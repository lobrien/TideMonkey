namespace XTideCliTestBridge
open System
open NUnit.Framework
open TideMonkey

[<TestFixture>]
type TideMonkeyLibTests() = 

    let fromUnixTimestamp secs = 
        let epoch = new DateTime(1970,1,1,0,0,0,0,System.DateTimeKind.Utc)
        epoch.AddSeconds( secs ).ToLocalTime()
    

    [<Test>]
    member x.CanLoadMockHarmonics() =
        match Station.LoadStations "AnyPath" with
        | None -> Assert.Fail("Did not load stations")
        | Some ss ->
            Assert.AreEqual(3, ss.Length)

    [<Test>]
    member x.CanFindKona() = 
        let ss = Station.LoadStations("pathToHarmonics") 
        let s = ss.Value |> Station.Named "Kailua Kona" 
        match s with 
        | None -> Assert.Fail("Could not locate Kona");
        | Some _ -> ignore() //pass

    [<Test>]
    member x.KonaCanPredict() = 
        let ss = Station.LoadStations "AnyPath"
        let s = ss.Value |> Station.Named "Kailua Kona" |> fun x -> x.Value
        let kkPredictor = Station.Predict s 
        let prediction = fromUnixTimestamp 1445121500.0 |> kkPredictor 
        Assert.AreEqual(Falling, prediction.Direction)
        Assert.AreEqual((float) 0.6523, (float) prediction.Magnitude, 0.0001)