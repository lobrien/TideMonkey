namespace XTideCliTestBridge
open System
open NUnit.Framework
open System.Diagnostics

type XTideResult = { time : DateTime; magnitude : float; unitOfMeasure : string; direction : string }

[<TestFixture>]
type CLISmokeTest() = 

    let path = "/Users/larryobrien/Documents/src/xamarin_mine/TideMonkey/xtide_cli"
    let harmonicsPath = "/Users/larryobrien/Documents/src/3rd party/MX-Tides-iOS/resources/harmonics-dwf-20121224-free.tcd" 

    let cli exe cmd = 
        let procStartInfo = new ProcessStartInfo(exe, cmd) 

        // Redirect to the Process.StandardOutput StreamReader.
        procStartInfo.RedirectStandardOutput <- true
        procStartInfo.UseShellExecute <- false;

        // Do not create the black window.
        procStartInfo.CreateNoWindow <- true;

        // Create a process, assign its ProcessStartInfo and start it
        let proc = new Process();
        proc.StartInfo <- procStartInfo;
        proc.Start() |> ignore

        proc.WaitForExit()

        proc.StandardOutput.ReadToEnd()

    let xtide_cli station time = 
       let cmd = String.Format("\"{0}\" \"{1}\" \"{2}\"", harmonicsPath, station, time.ToString())
       let ary = cli path cmd |> fun s -> s.Split('"') |> Seq.filter (fun s -> s.Length > 1) |> Seq.toArray

       //Yes, this is extremely fragile. I don't care, as I control the XTide cli
       ary |> fun a -> { time = DateTime.Parse(a.[0].Substring(0, a.[0].IndexOf("M")+1)); magnitude = Double.Parse(a.[1]); unitOfMeasure = a.[2]; direction = a.[3] }

    [<Test>]
    member x.CanTestAnchorage() =
        let stationName = "Anchorage"
        let timeStamp = 1445121500

        let result = xtide_cli stationName timeStamp
        Assert.AreEqual("falling", result.direction)
        Assert.AreEqual(13.1049, result.magnitude, 0.0001)
      

    [<Test>]
    member x.CanTestKona() = 
        let stationName = "Kailua Kona"
        let timeStamp = 1445121500

        let result = xtide_cli stationName timeStamp
        Assert.AreEqual("falling", result.direction)
        Assert.AreEqual(0.6523, result.magnitude, 0.0001)

        let t2 = 1445121500 + 7200
        let r2 = xtide_cli stationName t2
        Assert.AreEqual("rising", r2.direction);
