namespace XTideCliTestBridge
open System
open NUnit.Framework
open TideMonkey
open TideMonkey.Station

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
        let kkPredictor = s.Predict
        let prediction = fromUnixTimestamp 1445121500.0 |> kkPredictor 
        Assert.AreEqual(Falling, prediction.Direction)
        Assert.AreEqual((float) 0.6523, (float) prediction.Magnitude, 0.0001)

    [<Test>]
    member test.CanRunStationAddSunMoonEvents() = 
        let s = Station.Named("Kailua Kona") <| (Station.LoadStations("Any") |> Option.get)

        match s with 
        | Some st -> 
            let start = new DateTime(2016, 01, 06)
            let endTime = new DateTime(2016, 01, 07)
            let teo = st.AddSunMoonEvents start endTime
            Assert.AreEqual(59., float teo.Sunrises.Head.Minute, 2.0)
            Assert.AreEqual(59., float teo.Sunsets.Head.Minute, 2.0)
            Assert.AreEqual(57., float teo.Moonrises.Head.Minute, 2.0)
            Assert.AreEqual(44., float teo.Moonsets.Head.Minute, 2.0)
            Assert.AreEqual(SkyCal.NewMoon, snd teo.MoonPhases.Head)

        | None -> Assert.Fail()
