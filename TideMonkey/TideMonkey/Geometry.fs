namespace TideMonkey

open System

[<Measure>]
type Radians

[<Measure>]
type Degrees

[<Measure>]
type ArcSeconds

[<Measure>]
type ArcMinutes

[<Measure>]
type DecimalHours

type DMS = 
   { Degrees : int<Degrees>
     Minutes : int<ArcMinutes>
     Seconds : float<ArcSeconds> }

type HMS = 
   { Hours : int
     Minutes : int
     Seconds : float }

type CoordinatesT = 
   { Latitude : float<Degrees>
     Longitude : float<Degrees> }

module Geometry = 
   let radiansPerDegree = (Math.PI * 2.0<Radians>) / 360.0<Degrees>
   let degreesPerRadian = 360.0<Degrees> / (Math.PI * 2.0<Radians>)
   let degreesPerArcSecond = 1.0<Degrees> / 3600.0<ArcSeconds>
   let arcSecondsPerDegree = 3600.0<ArcSeconds> / 1.0<Degrees>
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
      { Degrees = 1<Degrees> * int wholeDegrees
        Minutes = 1<ArcMinutes> * int minutes
        Seconds = secondsDecimal }
   
   let dms2deg (dms : DMS) = 
      let ds = float dms.Degrees
      let m = (float dms.Minutes / 60.0)
      let s = (float dms.Seconds / 3600.0)
      let d = (ds + m + s) * 1.0<Degrees>
      d
   
   let deg2arcsec (degs : float<Degrees>) = degs * arcSecondsPerDegree
   let rad2arcsec (rads : float<Radians>) = rad2deg rads |> fun degs -> degs * arcSecondsPerDegree
   let arcsec2deg (secs : float<ArcSeconds>) = secs / arcSecondsPerDegree
   
   let arcsec2rad (secs : float<ArcSeconds>) = 
      secs
      |> arcsec2deg
      |> deg2rad
   
   let deg2hms (degs : float<Degrees>) = 
      let pctCircle = degs / 360.0<Degrees>
      let pctDay = pctCircle * 24.0
      let hours = int (Math.Floor(pctDay))
      let minutesRemainder = (pctDay - float hours) * 60.0
      let minutes = int (Math.Floor(minutesRemainder))
      let secsRemainder = (minutesRemainder - float minutes) * 60.0
      { Hours = hours
        Minutes = minutes
        Seconds = secsRemainder }
   
   let dms2hms (dms : DMS) = dms2deg dms |> deg2hms
