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

type PredictionValueT = { Amplitude : AmplitudeT }

type PredictionT = { Magnitude : float ; Direction : DirectionT ; Units : PredictionUnitsT}

