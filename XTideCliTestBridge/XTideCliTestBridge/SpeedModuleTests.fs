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

    [<Test>]
    member x.CanConvertSpeed() =
        let degreesPerHour = 3600.0<Degrees/Hours>
        let radiansPerSec = Speed.Convert degreesPerHour
        let degreesPerRadian = 360.0 / (Math.PI * 2.0);
        //Cast away the unit of measure
        let degreesPerSec = (float) radiansPerSec * degreesPerRadian
        Assert.AreEqual(1.0, degreesPerSec, 0.0001)

    [<Test>]
    member x.CanMultiplyIntervalsBySpeeds() = 
        let interval = 10<Seconds>
        let speed = 5<Radians/Seconds>
        let angle = interval * speed
        Assert.AreEqual(50, angle)

    [<Test>]
    [<Ignore("Units of measure type safety")>]
    member x.CanMultiplyIntervalsBySpeedWithUnitOfMeasureSafety() =
        let s2 = 3600.0<Degrees/Hours>
        let i2 = 60.0<Seconds>
        let angle = i2 * s2
        Assert.AreEqual((Math.PI * 2.0), float angle, 0.0001)
       
