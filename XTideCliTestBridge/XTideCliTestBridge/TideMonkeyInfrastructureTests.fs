namespace XTideCliTestBridge
open System
open NUnit.Framework
open TideMonkey

[<TestFixture>]
type TideMonkeyInfrastructureTests() = 

    [<Test>]
    member x.CanConvertSpeed() =
        let degreesPerHour = 3600.0<DegreesPerHour>
        let radiansPerSec = Speed.Convert degreesPerHour
        let degreesPerRadian = 360.0 / (Math.PI * 2.0);
        //Cast away the unit of measure
        let degreesPerSec = (float) radiansPerSec * degreesPerRadian
        Assert.AreEqual(1.0, degreesPerSec, 0.0001)

    [<Test>]
    member x.CanMultiplyIntervalsBySpeeds() = 
        let interval = 10<Seconds>
        let speed = 5<RadiansPerSecond>
        let angle = interval * speed
        Assert.AreEqual(50, angle)

    [<Test>]
    [<Ignore("Units of measure type safety")>]
    member x.CanMultiplyIntervalsBySpeedWithUnitOfMeasureSafety() =
        let s2 = 3600.0<DegreesPerHour>
        let i2 = 60.0<Seconds>
        let angle = i2 * s2
        Assert.AreEqual((Math.PI * 2.0), float angle, 0.0001)
       