namespace SkyCal

open System
open TideMonkey

type MoonPhaseT = 
    | NewMoon
    | FirstQuarter
    | FullMoon
    | LastQuarter


module Calendar = 

    let ToJulianDate (dateTime : DateTime) = 
        dateTime.ToOADate() + 2415018.5


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
    let TApprox time =
        kApprox time / 1236.85

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

    //True (apparent) phase corrections (constants pp. 351-352)
    let ApparentPhaseNonPlanetaryCorrections moonPhase (e : float) (m : float<Degrees>) (m': float<Degrees>)  (f : float<Degrees>) (omega : float<Degrees>)  =
        let normalize ds = 
            let unitCircle = ds % 360.<Degrees>
            let degs = 
                match unitCircle < 0.<Degrees> with
                | true -> unitCircle + 360.0<Degrees>
                | false -> unitCircle
            degs

        let E = e
        let M = (normalize m)
        let M' = (normalize m') 
        let F = (normalize f) 
        let Omega = (normalize omega)

        let newMoonConstants = 
            [-0.40720; 0.17241 * E; 0.01608; 0.01039; 0.00739 * E; -0.00514 * E;
            0.00208 * (E * E);  -0.00111; -0.00057; -0.00056 * E; -0.00042; 0.000042 * E;
            0.00038 * E; -0.00024 * E; -0.00017; -0.00007; 0.00004; 0.00004; 0.00003; 
            0.00003; -0.00003; 0.00003; -0.00002; -0.00002; 0.00002]

        let fullMoonConstants =
            [-0.40614; 0.17302 * E; 0.01614; 0.01043; 0.00734 * E; -0.00515 * E;
            0.00209 * (E * E);  -0.00111; -0.00057; -0.00056 * E; -0.00042; 0.000042 * E;
            0.00038 * E; -0.00024 * E; -0.00017; -0.00007; 0.00004; 0.00004; 0.00003; 
            0.00003; -0.00003; 0.00003; -0.00002; -0.00002; 0.00002]


        //pg. 352
        let quarterConstants = 
            [-0.62801; 0.17172 * E; -0.01183 * E; 0.00862; 0.00804;
            0.00454 * E; 0.00204 * E; -0.00180; -0.00070; -0.00040;
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

        

        let pieceWise  = 
            List.zip newMoonConstants newAndFullOps 
            |> List.map (fun (fst,snd) -> (fst * snd))
            |> List.map normalize 
            |> List.map deg2rad 
            |> List.map sin

          
        let r = pieceWise |> List.sum
        (r, pieceWise)
       (*  
        let correction =
            match moonPhase with
            | NewMoon 
            | FullMoon ->  
                //pg. 351
                cs.[0]    * Math.Sin(deg2rad (M'))
                + cs.[1]  * M
                + cs.[2]  * (2. * M')
                + cs.[3]  * (2. * F)
                + cs.[4]  * (M' - M)
                + cs.[5]  * (M' + M)
                + cs.[6]  * (2. * M)
                + cs.[7]  * (M' - 2. * F)
                + cs.[8]  * (M' + 2. * F)
                + cs.[9]  * (2. * M' + M)
                + cs.[10] * (3. * M')
                + cs.[11] * (M + 2. * F)
                + cs.[12] * (M - 2. * F)
                + cs.[13] * (2. * M' - M)
                + cs.[14] * Omega
                + cs.[15] * (M' + 2. * M)
                + cs.[16] * (2. * M' - 2. * F)
                + cs.[17] * (3. * M)
                + cs.[18] * (M' + M - 2. * F)
                + cs.[19] * (2. * M' + 2. * F)
                + cs.[20] * (M' + M + 2. * F)
                + cs.[21] * (M' - M + 2. * F)
                + cs.[22] * (M' - M - 2. * F)
                + cs.[23] * (3. * M' + M)
                + cs.[24] * (4. * M')
            | FirstQuarter
            | LastQuarter ->
                //pg. 352
                cs.[0]    * Math.Sin(deg2rad(M'))
                + cs.[1]  * M
                + cs.[2]  * (M' + M)
                + cs.[3]  * (2. * M')
                + cs.[4]  * (2. * F)
                + cs.[5]  * (M' - M)
                + cs.[6]  * (2. * M)
                + cs.[7]  * (M' - 2. * F)
                + cs.[8]  * (M' + 2. * F)
                + cs.[9]  * (3. * M')
                + cs.[10] * (2. * M' - M)
                + cs.[11] * (M + 2. * F)
                + cs.[12] * (M - 2. * F)
                + cs.[13] * (M' + 2. * M)
                + cs.[14] * (2. * M' + M)
                + cs.[15] * Omega
                + cs.[16] * (M' - M - 2. * F)
                + cs.[17] * (2. * M' + 2.* F)
                + cs.[18] * (M' + M + 2. * F)
                + cs.[19] * (M' - 2. * M)
                + cs.[20] * (M' + M - 2. * F)
                + cs.[21] * (3. * M)
                + cs.[22] * (2. * M' - 2. * F)
                + cs.[23] * (M' - M + 2. * F)
                + cs.[24] * (3. * M' + M)

        correction
        *)

module Moon = 

    
    
    let NextMoonPhase (time : DateTime) = 
        (*
        LOB: This is not a port of Skycal's NextMoonPhase function

        It is a direct implementation in F# of algorithm 49.1 in 
        Meeus' "Astronomical Algorithms, 2nd Ed." and other algorithms

        *)





        NewMoon
