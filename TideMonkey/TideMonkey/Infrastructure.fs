namespace TideMonkey

open System

type DirectionT = 
   | Rising 
   | Falling 
   with
   override this.ToString() = 
       match this with
       | Rising -> "Rising"
       | Falling -> "Falling"

[<Measure>] type Meters
[<Measure>] type Feet
[<Measure>] type Knots
[<Measure>] type Zulu
   
type PredictionUnitsT = 
    | Meters
    | Feet
    | Knots
    | KnotsSquared
    | Zulu


[<Measure>] type Radians
[<Measure>] type Degrees
[<Measure>] type ArcSeconds
[<Measure>] type ArcMinutes
[<Measure>] type DecimalHours
type DMS = { Degrees : int<Degrees>; Minutes : int<ArcMinutes>; Seconds : float<ArcSeconds> }
type HMS = { Hours : int; Minutes : int; Seconds : float }

[<Measure>] type Seconds 
[<Measure>] type Hours
type Year = int


(*
TODO: Convert these into composite units of measure with angles / timeT  
e.g., DegreesPerHour = <Degrees>/<Hour> but is also a measure of angular velocity 
*)
[<Measure>] type DegreesPerHour 
[<Measure>] type RadiansPerSecond

// Speed:  angular units over time units.
type SpeedT<[<Measure>] 'u> = float<'u>

//TODO: Figure out a way to validate / replace / runtime-test the match of Value with the Units
type AmplitudeT = { Value : float; Units : PredictionUnitsT }


type ConstituentT<[<Measure>] 'speedT, [<Measure>] 'amplitudeT> = { Name : string; Speed : SpeedT<'speedT>; FirstValidYear : Year; LastValidYear : Year; Amplitude : AmplitudeT; Phase : float<Radians> ; Args: (Year * float<Radians>) list; Nodes : (Year * float) list}

type IntervalT = { Duration : float<Seconds> }

type CoordinatesT = { Latitude : float<Degrees>; Longitude : float<Degrees> }

type CurrentBearingT = { Degrees : float<Degrees>; IsDegreesTrue : bool }

module Assert = 
    let IsTrue (fn : unit -> bool) = 
        match fn() with 
        | true -> ignore()
        | false -> raise (new Exception("AssertionFailed"))

    let LogException x = 
        printf "%A" x
        raise x

module Geometry = 
    let radiansPerDegree = (Math.PI* 2.0<Radians>)/360.0<Degrees>
    let degreesPerRadian = 360.0<Degrees>/(Math.PI * 2.0<Radians>)
    let degreesPerArcSecond = 1.0<Degrees>/3600.0<ArcSeconds>
    let arcSecondsPerDegree = 3600.0<ArcSeconds>/1.0<Degrees>
   

    let normalize (degs : float<Degrees>) = degs % 360.0<Degrees>
    let normalizePositive (degs : float<Degrees>) = 
        let unitCircle = normalize degs
        let degs = 
            match unitCircle < 0.<Degrees> with
            | true -> unitCircle + 360.0<Degrees>
            | false -> unitCircle
        degs


    let normalizeRadians (rads : float<Radians>) = rads % (Math.PI * 2.<Radians>)
    let normalizeRadiansPositive (rads : float<Radians>) = 
        let unitRadians = normalizeRadians rads
        match unitRadians < 0.<Radians> with
        | true -> unitRadians + (2.<Radians> * Math.PI)
        | false -> unitRadians

    let rad2deg (rads : float<Radians>) = rads * degreesPerRadian
    let deg2rad (degs : float<Degrees>) = degs * radiansPerDegree
    let deg2dms (degs : float<Degrees>) = 
        let wholeDegrees = System.Math.Floor(float degs) 
        let minutesRemaining = (float degs) - wholeDegrees
        let minutesDecimal = minutesRemaining * 60.0
        let minutes = System.Math.Floor(minutesDecimal)
        let secondsRemaining = minutesDecimal - minutes
        let secondsDecimal = secondsRemaining * 60.0<ArcSeconds>
        { Degrees = 1<Degrees> * int wholeDegrees; Minutes = 1<ArcMinutes> * int minutes; Seconds = secondsDecimal }
    
    let dms2deg (dms : DMS) = 
        let ds = float dms.Degrees
        let m = (float dms.Minutes / 60.0)
        let s = (float dms.Seconds / 3600.0)
        let d = (ds + m + s) * 1.0<Degrees>
        d
   
    let deg2arcsec (degs : float<Degrees>) = degs * arcSecondsPerDegree
    let rad2arcsec (rads : float<Radians>) = rad2deg rads |> fun degs -> degs * arcSecondsPerDegree
    let arcsec2deg (secs : float<ArcSeconds>) = secs / arcSecondsPerDegree
    let arcsec2rad (secs : float<ArcSeconds>) = secs |> arcsec2deg |> deg2rad

    let deg2hms (degs : float<Degrees>) = 
        let pctCircle = degs / 360.0<Degrees>
        let pctDay = pctCircle * 24.0
        let hours = int (Math.Floor(pctDay))
        let minutesRemainder = (pctDay - float hours) * 60.0
        let minutes = int (Math.Floor(minutesRemainder))
        let secsRemainder = (minutesRemainder - float minutes) * 60.0
        { Hours = hours; Minutes = minutes; Seconds = secsRemainder }


    let dms2hms (dms : DMS) = dms2deg dms |> deg2hms 

    let dph2rps (dph : float<DegreesPerHour>) = (dph / 360.0 * Math.PI * 2.0 / 3600.0) * 1.0<RadiansPerSecond>/1.0<DegreesPerHour>
            

module Speed = 
    let Convert (degreesPerHour : float<DegreesPerHour>) : float<RadiansPerSecond> = (degreesPerHour * Math.PI / 648000.0) * 1.0<RadiansPerSecond/DegreesPerHour>

module Constituent =    
    open Geometry
   
    let CreateFromUntypedInputs name speedDegreesPerSecond startYear numberOfYears argsDegrees (nodes : float list) amplitude phaseDegrees = 
        let typedSpeed = speedDegreesPerSecond * 1.0<DegreesPerHour>
        let speed = Speed.Convert(typedSpeed)
        let lastValidYear = startYear + numberOfYears - 1
        Assert.IsTrue(fun () -> lastValidYear >= startYear)

        let phaseRadians = deg2rad phaseDegrees

        Assert.IsTrue(fun () -> numberOfYears = (argsDegrees |> List.ofSeq |> List.length) )
        let years : Year seq = seq { for yr in startYear .. (startYear + numberOfYears) -> yr } 
        let ra = argsDegrees |> Seq.map Geometry.deg2rad
        let args = Seq.zip years ra |> List.ofSeq

        Assert.IsTrue(fun () -> numberOfYears = (nodes |> List.ofSeq |> List.length) )
        let nodes = Seq.zip years nodes |> List.ofSeq

        { Name = name; Speed = speed; FirstValidYear = startYear ; LastValidYear = lastValidYear; Amplitude = amplitude; Phase = phaseRadians ; Args = args; Nodes = nodes; }

    let ValidateYear constituent year =
        Assert.IsTrue ( fun() -> constituent.FirstValidYear >= year && constituent.LastValidYear >= year)

    [<Obsolete("Use EquilibriumArgument")>]
    let arg constituent year = 
        ValidateYear constituent year
        constituent.Args |> List.find (fun t -> fst(t) = year)

        
    let EquilibriumArgument constituent year = arg constituent year
    
    [<Obsolete("Use NodeFactor")>]
    let nod constituent year = 
        ValidateYear constituent year
        constituent.Nodes |> List.find (fun t -> fst(t) = year)

    let NodeFactor constituent year = 
        nod constituent year


module Interval = 
   //TODO: Figure out how to type-check / convert units of measure from speed to match interval
   let (*) (interval : IntervalT) (speed : SpeedT<'u>) = interval.Duration * speed
