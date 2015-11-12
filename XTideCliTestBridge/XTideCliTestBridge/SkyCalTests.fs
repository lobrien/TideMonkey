namespace XTideCliTestBridge
open System
open NUnit.Framework
open SkyCal
open TideMonkey

[<TestFixture>]
type SkyCalTests() = 
      
    [<Test>]
    member x.CanConvertJulianDates() = 
        let t = new DateTime(2015, 01, 01)
        let d = Calendar.ToJulianDate t
        //Julian dates begin at noon, not midnight...
        Assert.AreEqual(2457023.5, d)
        let c = new DateTime(1899, 12, 30, 0, 0, 0)
        let d2 = Calendar.ToJulianDate c
        Assert.AreEqual(2415018.5, d2)


    [<Test>]
    member x.CanFindMeeusT() = 
        let t = new DateTime(2000, 1, 1, 12, 0, 0)
        let d = Calendar.ToJulianDate t
        Assert.AreEqual(2451545.0, d)
        let meeusT = Meeus.T t
        Assert.AreEqual(0.0, meeusT)
        let t2 = t.AddYears(50)
        let meeusHalfCentury = Meeus.T t2
        Assert.AreEqual(0.5, meeusHalfCentury, 0.0001)

    
    [<Test>]
    member x.CanFindMeeusE() = 
        //Value from Example 47.a
        let t = new DateTime(1992, 04, 12)
        let e = Meeus.E(Meeus.T(t))
        Assert.AreEqual(1.000194, e, 0.000001)

    [<Test>]
    member x.WorksMeeusExample49a() = 
        let t =  new DateTime(1977, 2, 16, 12, 0, 0)
        let kApprox = Meeus.kApprox t
        Assert.AreEqual(-283., kApprox, 0.5)
        let k = -283.
        let T = Meeus.TApprox t
        Assert.AreEqual(-0.22881, T, 0.001)
        let JDE = Meeus.JDE k T
        Assert.AreEqual(2443192.94102, JDE, 0.00001)

        let E = Meeus.E T
        Assert.AreEqual(1.0005753, E, 0.00001)

        let M = Meeus.M T k
        Assert.AreEqual(-8234.2625, float M, 0.0001)

        let M' = Meeus.M' T k
        Assert.AreEqual(-108984.6278, float M', 0.0001)

        let F = Meeus.F T k
        Assert.AreEqual(-110399.0416, float F, 0.0001)

        let omega = Meeus.Omega T k
        Assert.AreEqual(567.3176, float omega, 0.0001)

    [<Test>]
    member test.CanSumNonPlanetaryPeriodicTerms() = 
        let normalize ds = 
            let unitCircle = ds % 360.<Degrees>
            match unitCircle < 0.<Degrees> with
            | true -> unitCircle + 360.0<Degrees>
            | false -> unitCircle

                //Example 49.a
        let E = 1.0005753
        let M = normalize -8234.2625<Degrees>
        let M' = normalize -108984.6278<Degrees>
        let F = normalize -110399.0416<Degrees>
        let Omega = normalize 567.3176<Degrees>
        let sumNonPlanetaryPeriodicTerms = Meeus.ApparentPhaseNonPlanetaryCorrections NewMoon E M M' F Omega
        let (r, cs) = sumNonPlanetaryPeriodicTerms
        Assert.AreEqual(-0.28916, r, 0.00001)


    [<Test>]
    member x.CanFindMeeusApproxK() = 
        //Example 49.a
        let t2 = new DateTime(1977, 2, 16, 12, 0, 0)
        let k2 = Meeus.kApprox t2
        //Example uses 1977.13, which is ~ noon on the 16th
        Assert.AreEqual(-282.87, k2, 0.01)


    [<Test>]
    member x.CanFindMeeusApproxT() = 
        let t = new DateTime(1987, 3, 1)
        let t = Meeus.TApprox t
        Assert.AreEqual(-0.128377, t, 0.00001)

    [<Test>]
    member x.CanFindMeeusJDE() = 
        //Example 49.a
        let k = -283.
        let T = -0.22881
        let jde = Meeus.JDE k T
        Assert.AreEqual(2443192.94102, jde, 0.00001)

    [<Test>]
    member x.CanFindNextMoonPhase() =
        let time = DateTime.UtcNow
        let nextPhase = Moon.NextMoonPhase time

        Assert.Fail()
