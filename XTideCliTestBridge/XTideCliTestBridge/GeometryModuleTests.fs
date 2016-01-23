namespace XTideCliTestBridge
open System
open NUnit.Framework
open TideMonkey
open Geometry

[<TestFixture>]
type GeometryModuleTests() = 


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

