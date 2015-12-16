namespace SkyCal

open System
open TideMonkey
open Geometry

type MoonPhaseT = 
    | NewMoon
    | FirstQuarter
    | FullMoon
    | LastQuarter

// DeltaPhi = Nutation in Longitude, DeltaEpsilon = nutation in obliquity
type NutationT = { DeltaPsi : float<ArcSeconds>; DeltaEpsilon : float<ArcSeconds> }

type SiderealTimeT = { MeanSiderealTime : HMS; ApparentSiderealTime : HMS }

type EphemerisEntryT = { Date: DateTime; Alpha : float<Degrees>; Delta : float<Degrees>; ApparentSiderealTime : float<DecimalHours> }

type EphemerisT = { Prior : EphemerisEntryT; Day : EphemerisEntryT; Following : EphemerisEntryT }

type RisingTransitSettingT = { Rising : HMS; Transit : HMS; Setting : HMS }

//type GreenwichLocalSiderealTimeT = { Date : DateTime; Time : float<DecimalHours> }

module Calendar = 

    let ToJulianDate (dateTime : DateTime) = 
        dateTime.ToOADate() + 2415018.5

    let FromJulianDate jde = 
        let d = jde - 2415018.5
        DateTime.FromOADate(d)

module Ephemeris = 
    let private EphemerisData solarOrLunar =
        let ls = System.IO.File.ReadAllLines("Resources/Greenwich" + solarOrLunar + "Ephemeris.txt")
        let tail = ls |> Seq.skipWhile(fun l -> l <> "$$SOE") |> Seq.skip 1
        let data = tail |> Seq.takeWhile(fun l -> l <> "$$EOE")
        let rs = 
            data 
            |> Seq.map (fun d -> (d.Substring(0, 17), d.Substring(22, 9), d.Substring(32, 9), d.Substring(43,12)))
            |> Seq.map (fun (julianDate, ra, dec, decimalHours) -> (float(julianDate)), float(ra), float(dec), float(decimalHours))
            |> Seq.map (fun (jd, ra, dec, localSiderealTime) -> (jd, ra * 1.0<Degrees>, dec * 1.0<Degrees>, localSiderealTime * 1.0<DecimalHours>))
            |> Seq.map (fun (jd, ra, dec, localSiderealTime) -> { Date = Calendar.FromJulianDate(jd); Alpha = ra; Delta = dec; ApparentSiderealTime = localSiderealTime })
        rs


    let GreenwichEphemeris (date : DateTime) solarOrLunar = 
        let ephs = 
            [| date.AddDays(-1.); date; date.AddDays(1.) |]
            |> Seq.map (fun d ->
                let jd = Calendar.ToJulianDate d
                EphemerisData solarOrLunar |> Seq.find(fun r -> r.Date = d)
                )
            |> Array.ofSeq
        { Prior = ephs.[0]; Day = ephs.[1]; Following = ephs.[2] } 

(* Algorithms from Meeus' "Astronomical Algorithms, 2nd Ed." Willman-Bell, 2009 *)
module Meeus = 

    //Julian centuries from Epoch J2000.0 (22.1)
    let T dateTime =
        let jd = Calendar.ToJulianDate dateTime
        (jd - 2451545.0) / 36525.0

    
    //Eccentricity of Earth's orbit (47.6)
    let E t = 1.0 - 0.002516 * t - 0.0000074 * t * t

    //Julian Ephemeris Days (49.1)
    let JDE k T = 
        2451550.09766 
        + 29.530588861 * k
        + 0.00015437 * T * T
        + 0.000000150 * T * T * T
        + 0.00000000073 * T * T * T * T

    //Approximation of k (49.2)
    let kApprox (time : DateTime) = 
        let fractionalYears = time.Subtract(new DateTime(2000, 1, 1)).TotalDays / 365.25
        fractionalYears * 12.3685 

    //Approximation of T (49.3)
    let TApprox k time = k / 1236.85

    //Sun's mean anomaly (49.4)
    let M T k = 
        2.5534
        + 29.10535670 * k
        - 0.0000014 * T * T
        - 0.00000011 * T * T * T 
        |> (*) 1.<Degrees>

    //Moon's mean anomaly (49.5)
    let M' T k =
        201.5643
        + 385.81693528 * k
        + 0.0107582 * T * T
        + 0.00001238 * T * T * T
        - 0.000000058 * T * T * T * T
        |> (*) 1.<Degrees>

    //Moon's argument of latitude (49.6)
    let F T k = 
        160.7108 
        + 390.67050284 * k
        - 0.0016118 * T * T
        - 0.00000227 * T * T * T
        + 0.000000011 * T * T * T * T
        |> (*) 1.<Degrees>

    //Longitude of the ascending node of the lunar orbit (49.7)
    let Omega T k = 
        124.7746 
        - 1.56375588 * k
        + 0.0020672 * T * T
        + 0.00000215 * T * T * T
        |> (*) 1.0<Degrees>

    let normalize ds = 
        let unitCircle = ds % 360.<Degrees>
        let degs = 
            match unitCircle < 0.<Degrees> with
            | true -> unitCircle + 360.0<Degrees>
            | false -> unitCircle
        degs

    //True (apparent) phase corrections (constants pp. 351-352)
    let ApparentPhaseNonPlanetaryCorrections moonPhase (e : float) (m : float<Degrees>) (m': float<Degrees>)  (f : float<Degrees>) (omega : float<Degrees>)  =
        let E = e
        let M = (normalize m)
        let M' = (normalize m') 
        let F = (normalize f) 
        let Omega = (normalize omega)

        let newMoonConstants = 
            [-0.40720; 0.17241 * E; 0.01608; 0.01039; 0.00739 * E; -0.00514 * E;
            0.00208 * (E * E);  -0.00111; -0.00057; 0.00056 * E; -0.00042; 0.00042 * E;
            0.00038 * E; -0.00024 * E; -0.00017; -0.00007; 0.00004; 0.00004; 0.00003; 
            0.00003; -0.00003; 0.00003; -0.00002; -0.00002; 0.00002]

        let fullMoonConstants =
            [-0.40614; 0.17302 * E; 0.01614; 0.01043; 0.00734 * E; -0.00515 * E;
            0.00209 * (E * E);  -0.00111; -0.00057; -0.00056 * E; -0.00042; 0.00042 * E;
            0.00038 * E; -0.00024 * E; -0.00017; -0.00007; 0.00004; 0.00004; 0.00003; 
            0.00003; -0.00003; 0.00003; -0.00002; -0.00002; 0.00002]


        //pg. 352
        let quarterConstants = 
            [-0.62801; 0.17172 * E; -0.01183 * E; 0.00862; 0.00804;
            0.00454 * E; 0.00204 * E * E; -0.00180; -0.00070; -0.00040;
            -0.00034 * E; 0.00032 * E; 0.00032 * E; -0.00028 * (E * E);
            0.00027 * E; -0.00017; -0.00005; 0.00004; -0.00004; 0.00004;
            0.00003; 0.00003; 0.00002; 0.00002; -0.00002]
                            
        let cs = match moonPhase with
                        | NewMoon -> newMoonConstants
                        | FullMoon -> fullMoonConstants
                        | FirstQuarter -> quarterConstants
                        | LastQuarter -> quarterConstants


        let deg2rad (degs : float<Degrees>) = float degs * (Math.PI * 2.0 / 360.0)

        let rad2deg rads = 1.<Degrees> * rads * 180. / Math.PI

        let newAndFullOps = [
            M';M ;(2. * M') ;(2. * F) ;(M' - M) ;
            (M' + M) ;(2. * M) ;(M' - 2. * F) ;(M' + 2. * F) ;(2. * M' + M) ;
            (3. * M') ;(M + 2. * F) ;(M - 2. * F) ;(2. * M' - M) ;Omega ;
            (M' + 2. * M) ;(2. * M' - 2. * F) ;(3. * M) ;(M' + M - 2. * F) ;
            (2. * M' + 2. * F) ;(M' + M + 2. * F) ;(M' - M + 2. * F) ;
            (M' - M - 2. * F) ;(3. * M' + M) ;(4. * M') ]

        let quarterOps = [
            M';
            M ;
            M' + M;
            2. * M';
            2. * F;
            M' - M;
            2. * M;
            M' - 2. * F;
            M' + 2. * F;
            3. * M';
            2. * M' - M;
            M + 2. * F;
            M - 2. * F;
            M' + 2. * M;
            2. * M' + M;
            Omega;
            M' - M - 2.* F;
            2. * M' + 2. * F;
            M' + M + 2. * F;
            M' - 2. * M;
            M' + M - 2. * F;
            3. * M;
            2. * M' - 2. * F;
            M' - M + 2. * F;
            3. * M' + M;
            ]

        let constantsAndOps = 
            match moonPhase with
            | NewMoon -> List.zip newMoonConstants newAndFullOps 
            | FullMoon -> List.zip fullMoonConstants newAndFullOps
            | FirstQuarter 
            | LastQuarter -> List.zip quarterConstants quarterOps

        let pieceWise = constantsAndOps |> List.map (fun (fst,snd) -> fst * sin(deg2rad(snd)))

          
        let r = pieceWise |> List.sum

        //Calculate correction for last and first quarter
        let w = 
            0.00306 
            - 0.00038 * E * cos(deg2rad(M)) 
            + 0.00026 * cos(deg2rad(M'))
            - 0.00002 * cos(deg2rad(M' - M))
            + 0.00002 * cos(deg2rad(M' + M))
            + 0.00002 * cos(deg2rad(2. * F))

        let correctedForQuarter = 
            match moonPhase with
            | FirstQuarter -> w
            | LastQuarter -> -w
            | NewMoon
            | FullMoon -> 0.0

        let final = r + correctedForQuarter

        (final, pieceWise)

    let NutationApprox date = 
        //Meeus Chapter 22
        let T = T date

        //Longitude of ascending node of Moon's mean orbit
        let omega = 125.04452 - (1934.136261 * T) + (0.0020708 * T * T) + (T * T * T) / 450000.
       
        // "If an accuracy of 0".5 in Δψ and of 0".1 in Δε are sufficient... "
        let L = 280.4665<Degrees> + 36000.7698<Degrees> * T
        let L' = 218.3165<Degrees> + 481267.8813<Degrees> * T

        let omegaRad = float (deg2rad (omega * 1.0<Degrees>))
        let LRad = float (deg2rad (L))
        let L'Rad = float (deg2rad (L'))

        let secondsToRads s = deg2rad (1.0<Degrees>/ 3600. * s)
        //These are in seconds
        let deltaPsiCoefficients = [-17.2; -1.32; -0.23; 0.21 ] |> List.map secondsToRads
        let deltaEpsilonCoefficients = [9.20; 0.57; 0.10; -0.09 ] |> List.map secondsToRads

        let opsPsi = [ sin (omegaRad); sin (2. * LRad); sin (2. * L'Rad); sin (2. * omegaRad) ]

        let deltaPsi = 
            let components = 
                List.zip deltaPsiCoefficients opsPsi 
                |> List.map (fun (a,b) -> a*b)
            components 
            |> List.sum
            |> normalizeRadians
            |> rad2arcsec

        let opsEps = [ cos (omegaRad); cos (2. * LRad); cos (2. * L'Rad); cos (2. * omegaRad) ]

        let deltaEpsilon = 
            let components =
                List.zip deltaEpsilonCoefficients opsEps
                |> List.map (fun (a,b) -> a*b)
            components 
            |> List.sum
            |> normalizeRadians
            |> rad2arcsec

        { DeltaPsi = deltaPsi; DeltaEpsilon = deltaEpsilon } 


    let TrueObliquityOfEcliptic date = 
        //Meeus 22.2 -- Do not use for >>hundreds of years from 2000CE
        let T = T date
        let coeffs = 
            [ 
            { Degrees = 23<Degrees>; Minutes = 26<ArcMinutes>; Seconds = 21.448<ArcSeconds> };
            { Degrees = 0<Degrees>; Minutes = 0<ArcMinutes>; Seconds = -46.8150<ArcSeconds>};
            { Degrees = 0<Degrees>; Minutes = 0<ArcMinutes>; Seconds = -0.00059<ArcSeconds>};
            { Degrees = 0<Degrees>; Minutes = 0<ArcMinutes>; Seconds = 0.001813<ArcSeconds>}
            ] |> List.map dms2deg
        let ops = [1.0; T; T * T; T * T * T ]
        let meanObliquity = 
            List.zip coeffs ops
            |> List.map (fun (a,b) -> a * b)
            |> List.sum

        let nutation = NutationApprox date
        let deltaEpsInDegrees = arcsec2deg nutation.DeltaEpsilon
        let trueObliquity = meanObliquity + deltaEpsInDegrees
        trueObliquity

    let SiderealTimeGreenwich date = 
        //Meeus 12.3
        let MeanSiderealTimeGreenwich date = 
            let T = T date
            let cs = [
                100.46061837;
                36000.770053608 * T;
                0.000387933 * T * T;
                - (T * T * T) / 38710000.;
                ]
            cs 
            |> List.sum 
            |> fun d -> d * 1.0<Degrees>
            |> fun d -> normalize d

        let mstInDegrees = MeanSiderealTimeGreenwich date
        let siderealTime = mstInDegrees |> deg2hms

        let nutation = NutationApprox date
        let trueObliquityOfEcliptic = TrueObliquityOfEcliptic date
        let cosEpsilon = cos (float( deg2rad (trueObliquityOfEcliptic) ) )

        let correction = (nutation.DeltaPsi * cosEpsilon) / 15.0

        let apparentSiderealTime = mstInDegrees + (arcsec2deg correction) |> deg2hms
            
        { MeanSiderealTime  = siderealTime; ApparentSiderealTime = apparentSiderealTime }
       

    let RisingAndSettingApprox coordinates ephemeris h0 = 
        let latitude = coordinates.Latitude
        let longitude = coordinates.Longitude
        let delta2 = ephemeris.Day.Delta
        let theta0 = ephemeris.Day.ApparentSiderealTime
        //Meeus 15.1

        //Note that this is an approximation that does not interpolate. Relies on granularity of theta0 (apparent sidereal time)

        //Circumpolar test
        let element = sin (float (deg2rad latitude)) * sin (float (deg2rad delta2))
        if element < -1.0 || element > 1.0 then
            raise (Exception("NotImplementedException -- circumpolar body"))

        //Obliquity 
        let H0 = 
            let a = sin (float (deg2rad h0))
            let b = sin (float (deg2rad latitude))
            let c = sin (float (deg2rad delta2))
            let num = a - b * c
            let den = cos (float (deg2rad latitude)) * cos (float (deg2rad delta2))
            num / den
            |> acos
            |> fun angleInRadians -> rad2deg (angleInRadians * 1.0<Radians>)
            |> normalizePositive

        let DayNormalize pct = 
            match pct with 
            | v when v < 0.0 -> pct + 1.0
            | v when v > 1.0 -> pct - 1.0
            | _ -> pct

        let theta0Degrees = theta0 * 360.0<Degrees> / 24.0<DecimalHours>

        let transit = (ephemeris.Day.Alpha + longitude - theta0Degrees) / 360.0<Degrees> |> DayNormalize
        let rising = transit - (H0 / 360.0<Degrees>) |> DayNormalize
        let setting = transit + (H0 / 360.0<Degrees>) |> DayNormalize

        let fractionalDayToHMS pct = 
            let hours = pct * 24.0
            let hour = int (Math.Floor(hours))
            let hourRemainder = hours - (float hour)
            let minuteF = hourRemainder * 60.0
            let minute = int (Math.Floor minuteF)
            let seconds = (minuteF - float minute) * 60.0
             
            let (minutesPos, secondsPos) = 
                match seconds < 0. with 
                | true -> ((minute + 1), (seconds + 60.0))
                | false -> (minute, seconds)

            { Hours = hour; Minutes = minutesPos; Seconds = secondsPos } 

        let transitTime = fractionalDayToHMS transit
        let risingTime = fractionalDayToHMS rising
        let settingTime = fractionalDayToHMS setting

        { Rising = risingTime; Transit = transitTime; Setting = settingTime } 

module Moon = 

    let PhasesForMoonCycle (t : DateTime) = 
        let ka = Meeus.kApprox t

        let ks = [ (Math.Floor(ka), NewMoon); (Math.Floor(ka) + 0.25, FirstQuarter); (Math.Floor(ka) + 0.5, FullMoon); (Math.Floor(ka) + 0.75, LastQuarter)]

        ks
        |> List.map (fun (k, phase) -> 
            let T = Meeus.TApprox k t
            let JDE = Meeus.JDE k T
            let E = Meeus.E T
            let M = Meeus.M T k
            let M' = Meeus.M' T k
            let F = Meeus.F T k
            let omega = Meeus.Omega T k
            let (correx, _) = Meeus.ApparentPhaseNonPlanetaryCorrections phase E M M' F omega
            (JDE + correx, phase)
            )
        |> List.sort
        |> List.map (fun (jde, phase) -> (Calendar.FromJulianDate(jde), phase))

    
    let rec NextMoonPhase (time : DateTime) = 
        (*
        LOB: This is not a port of Skycal's NextMoonPhase function

        It is a direct implementation in F# of algorithm 49.1 in 
        Meeus' "Astronomical Algorithms, 2nd Ed." and other algorithms

        *)
        let phases = PhasesForMoonCycle time

        let nextInCurrentCycle = phases |> List.tryFind (fun (d : DateTime, _) -> d > time)
        match nextInCurrentCycle with
        | Some phase -> phase
        | None -> 
            let lastPhaseOfCycle = phases |> List.max 
            NextMoonPhase ((fst lastPhaseOfCycle).AddDays(1.))


