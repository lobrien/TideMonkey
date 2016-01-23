namespace TideMonkey

open System

[<Measure>]
type Meters

[<Measure>]
type Feet

[<Measure>]
type Knots

[<Measure>]
type Zulu

type PredictionUnitsT = 
   | Meters
   | Feet
   | Knots
   | KnotsSquared
   | Zulu

[<Measure>]
type Seconds

[<Measure>]
type Hours

type Year = int

// Speed:  angular units over time units.
type SpeedT = float<Radians / Seconds>

type AmplitudeT = 
   { Value : float
     Units : PredictionUnitsT }

type IntervalT = 
   { Duration : float<Seconds> }

module Units = 
   let IsHydraulicCurrent u = 
      match u with 
      | KnotsSquared -> true
      | Zulu -> raise <| new ArgumentOutOfRangeException()
      | _ -> false

   let IsCurrent u = 
      match u with
      | Knots | KnotsSquared -> true
      | Zulu -> raise <| new ArgumentOutOfRangeException()
      | _ -> false

   let Flatten u = 
      match u with 
      | KnotsSquared -> Knots
      | Zulu -> raise <| new ArgumentOutOfRangeException()
      | _ -> u

module Speed = 
   let Convert(degreesPerHour : float<Degrees / Hours>) : float<Radians / Seconds> = 
      degreesPerHour * 1.<Hours> / 3600.<Seconds> * (Math.PI * 2.<Radians>) / 360.0<Degrees>

module Interval = 
   let (*) (interval : IntervalT) (speed : SpeedT) = interval.Duration * speed
