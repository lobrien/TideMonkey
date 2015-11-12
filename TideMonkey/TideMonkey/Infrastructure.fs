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
type AmplitudeT<[<Measure>] 'u> = { Value : float<'u>; Units : PredictionUnitsT }


type ConstituentT<[<Measure>] 'speedT, [<Measure>] 'amplitudeT> = { Speed : SpeedT<'speedT>; FirstValidYear : Year; LastValidYear : Year; Amplitude : AmplitudeT<'amplitudeT>; Phase : float<Radians> ; Args: Map<Year, float<Degrees>>; Nodes : Map<Year,float>}

type IntervalT = { Duration : float<Seconds> }

module Assert = 
    let IsTrue (fn : unit -> bool) = 
        match fn() with 
        | true -> ignore()
        | false -> raise (new Exception("AssertionFailed"))

module Geometry = 
    let radiansPerDegree = (Math.PI* 2.0<Radians>)/360.0<Degrees>
    let degreesPerRadian = 360.0<Degrees>/(Math.PI * 2.0<Radians>)

    let rad2deg (rads : float<Radians>) = rads * degreesPerRadian
    let deg2rad (degs : float<Degrees>) = degs * radiansPerDegree

    let normalize (degs : float<Degrees>) = degs % 360.0<Degrees>
    let normalizePositive (degs : float<Degrees>) = 
        let unitCircle = normalize degs
        let degs = 
            match unitCircle < 0.<Degrees> with
            | true -> unitCircle + 360.0<Degrees>
            | false -> unitCircle
        degs


module Speed = 
    let Convert (degreesPerHour : float<DegreesPerHour>) : float<RadiansPerSecond> = (degreesPerHour * Math.PI / 648000.0) * 1.0<RadiansPerSecond/DegreesPerHour>

module Constituent =    
    open Geometry
   
    let CreateFromUntypedInputs speedDegreesPerSecond startYear numberOfYears argsDegrees nodes amplitude phaseDegrees = 
        let typedSpeed = speedDegreesPerSecond * 1.0<DegreesPerHour>
        let speed = Speed.Convert(typedSpeed)
        let lastValidYear = startYear + numberOfYears - 1
        Assert.IsTrue(fun () -> lastValidYear >= startYear)

        let phaseRadians = deg2rad phaseDegrees

        Assert.IsTrue(fun () -> numberOfYears = (argsDegrees |> List.ofSeq |> List.length) )
        let years : Year seq = seq { for yr in startYear .. (startYear + numberOfYears) -> yr } 
        let args = Seq.zip years (argsDegrees |> Seq.map (fun d -> d * 1.0<Degrees> )) |> Map.ofSeq

        Assert.IsTrue(fun () -> numberOfYears = (nodes |> List.ofSeq |> List.length) )
        let nodes = Seq.zip years nodes |> Map.ofSeq

        { Speed = speed; FirstValidYear = startYear ; LastValidYear = lastValidYear; Amplitude = amplitude; Phase = phaseRadians ; Args = args; Nodes = nodes; }

    let ValidateYear constituent year =
        Assert.IsTrue ( fun() -> constituent.FirstValidYear >= year && constituent.LastValidYear >= year)

    [<Obsolete("Use EquilibriumArgument")>]
    let arg constituent year = 
        ValidateYear constituent year
        constituent.Args |> Map.find year

        
    let EquilibriumArgument constituent year = arg constituent year
    
    [<Obsolete("Use NodeFactor")>]
    let nod constituent year = 
        ValidateYear constituent year
        constituent.Nodes |> Map.find year

    let NodeFactor constituent year = 
        nod constituent year

    //TODO: Use member ConstituentT with member to add methods directly to type. I mean, come on. What are we, farmers?

module Interval = 
   //TODO: Figure out how to type-check / convert units of measure from speed to match interval
   let (*) (interval : IntervalT) (speed : SpeedT<'u>) = interval.Duration * speed
