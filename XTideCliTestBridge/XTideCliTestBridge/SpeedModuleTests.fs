namespace XTideCliTestBridge

open System
open NUnit.Framework
open TideMonkey

[<TestFixture>]
type SpeedModuleTests() =
    
   [<Test>]
   member x.CanConvertDPHtoRPS() = 
      let oncePerHour = 360.0<Degrees / Hours>
      let radians = Speed.Convert oncePerHour
      let radiansPerHour = float radians
      let radiansPerMinute = radiansPerHour / 60.
      let radiansPerSecond = radiansPerMinute / 60.
      Assert.AreEqual(radiansPerSecond, float radians, 0.01)