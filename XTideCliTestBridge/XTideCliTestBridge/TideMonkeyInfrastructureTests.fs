namespace XTideCliTestBridge
open System
open NUnit.Framework
open TideMonkey
open Geometry

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
       

    [<Test>]
    member x.CanConvertDegreesIntoHMS() = 
        let ds = 1.5<Degrees>
        let hms = deg2dms ds
        Assert.AreEqual(1, int hms.Degrees)
        Assert.AreEqual(30, int hms.Minutes)
        Assert.AreEqual(0., float hms.Seconds, 0.00001)

    [<Test>]
    member x.CanConvertDecimalDegreesIntoDMS() = 
        let dd = 197.693195<Degrees>
        let ddmmss = Geometry.deg2dms(dd)
        Assert.AreEqual(197, ddmmss.Degrees)
        Assert.AreEqual(41, ddmmss.Minutes)
        Assert.AreEqual(35.5020, float ddmmss.Seconds, 0.0001)

    [<Test>]
    member x.CanConvertDMSIntoDegs() = 
        let hms = { Degrees = 13<Degrees>; Minutes = 41<ArcMinutes>; Seconds = 35.5020<ArcSeconds> }
        let dd = Geometry.dms2deg hms
        Assert.AreEqual(13.693195, float dd, 0.0000001)

