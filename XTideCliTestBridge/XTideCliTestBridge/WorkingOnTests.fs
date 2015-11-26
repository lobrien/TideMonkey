namespace XTideCliTestBridge
open System
open NUnit.Framework
open TideMonkey
open TideMonkey.Station

[<TestFixture>]
type WorkingOnTests() = 

    [<Test>]
    member test.CanRunStationAddSunMoonEvents() = 
        let s = Station.Named("Kailua Kona") <| (Station.LoadStations("Any") |> Option.get)

        match s with 
        | Some st -> 
            let start = new DateTime(2015, 12, 01)
            let endTime = new DateTime(2015, 12, 02)
            let teo = st.AddSunMoonEvents start endTime
            Assert.Fail()

        | None -> Assert.Fail()


