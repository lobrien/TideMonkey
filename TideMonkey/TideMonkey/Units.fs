namespace TideMonkey

open System

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
type SpeedT = float<RadiansPerSecond>

//TODO: Figure out a way to validate / replace / runtime-test the match of Value with the Units
type AmplitudeT = { Value : float; Units : PredictionUnitsT }

type IntervalT = { Duration : float<Seconds> }

module Speed = 
    let Convert (degreesPerHour : float<DegreesPerHour>) : float<RadiansPerSecond> = (degreesPerHour * Math.PI / 648000.0) * 1.0<RadiansPerSecond/DegreesPerHour>

    let dph2rps (dph : float<DegreesPerHour>) = (dph / 360.0 * Math.PI * 2.0 / 3600.0) * 1.0<RadiansPerSecond>/1.0<DegreesPerHour>
 
module Interval = 
   let (*) (interval : IntervalT) (speed : SpeedT) = interval.Duration * speed
