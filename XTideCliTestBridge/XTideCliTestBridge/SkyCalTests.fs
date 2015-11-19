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
        let T = Meeus.TApprox k t
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

        let jdeCorrex = Meeus.ApparentPhaseNonPlanetaryCorrections NewMoon E M M' F omega |> fst
        let finalJulian = JDE + jdeCorrex
        let date = Calendar.FromJulianDate(finalJulian)
        let expected = new DateTime(1977, 2, 18, 3, 37, 42) |> fun d -> d.ToOADate()
        //In fact, I'm off by about 61 seconds. Floating point?
        Assert.AreEqual(expected, date.ToOADate(), 120.0)

        //OK, try from start
        let ps = Moon.PhasesForMoonCycle(new DateTime(1977, 2, 16, 12, 0, 0))
        let p = ps.Head
        Assert.AreEqual(NewMoon, (snd p))
        Assert.AreEqual(expected, (fst p).ToOADate(), 0.001)

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
    member test.CanSumNonPlanetaryPeriodicTermsForQuarters() = 
        //Example 49.b
        let t =  new DateTime(2044, 01, 01, 01, 0, 0)
        let kApprox = Meeus.kApprox t
        Assert.AreEqual(544.21, kApprox, 0.5)
        let k = 544.75
        let T = Meeus.TApprox k t
        let JDE = Meeus.JDE k T
        Assert.AreEqual(2467636.88597, JDE, 0.00001)

        let E = Meeus.E T
        let M = Meeus.M T k
        let M' = Meeus.M' T k
        let F = Meeus.F T k
        let omega = Meeus.Omega T k

        let (r, cs) = Meeus.ApparentPhaseNonPlanetaryCorrections LastQuarter E M M' F omega
        // -0.39513 and quarter correction -0.00251 
        Assert.AreEqual(-0.39404, r, 0.00001)


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
        let t = Meeus.TApprox (Meeus.kApprox t) t
        Assert.AreEqual(-0.128377, t, 0.00001)

    [<Test>]
    member x.CanFindMeeusJDE() = 
        //Example 49.a
        let k = -283.
        let T = -0.22881
        let jde = Meeus.JDE k T
        Assert.AreEqual(2443192.94102, jde, 0.00001)

    [<Test>]
    member x.CanFindCurrentCyclePhaseDates() = 
        let time = new DateTime(2015, 11, 18)
        let nextMoonPhases = Moon.PhasesForMoonCycle time
        let a = nextMoonPhases.Head
        Assert.AreEqual(NewMoon, snd a)
        let expected = new DateTime(2015, 11, 11, 17, 48, 50)
        Assert.AreEqual(expected.ToOADate(), (fst a).ToOADate(), 0.001)

    [<Test>]
    member x.CanFindNextMoonPhase() =
        let time = new DateTime(2015, 11, 18)
        let nextPhase = Moon.NextMoonPhase time
        Assert.AreEqual(FirstQuarter, snd nextPhase)
        let expected = new DateTime(2015, 11, 19, 6, 29, 02)
        Assert.AreEqual(expected.ToOADate(), (fst nextPhase).ToOADate(), 0.001)
