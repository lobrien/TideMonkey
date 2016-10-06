namespace TideMonkey

open System

type ConstituentT = 
   { Name : string
     Speed : SpeedT
     FirstValidYear : YearT
     LastValidYear : YearT
     Amplitude : AmplitudeT
     Phase :float<Radians>
     Args : Map<YearT, float<Radians>>
     Nodes : Map<YearT, float> }

module Constituent = 
   open Geometry
   
   let CreateFromUntypedInputs name speedDegreesPerSecond startYear numberOfYears argsDegrees (nodes : float list) amplitude phaseRadians = 
      let typedSpeed = speedDegreesPerSecond * 3600.<Seconds> / 1.<Hours>
      let speed = Speed.Convert(typedSpeed)
      let lastValidYear = startYear + numberOfYears - 1
      AssertTM.IsTrue(fun () -> lastValidYear >= startYear)
      AssertTM.IsTrue(fun () -> 
         numberOfYears = (argsDegrees
                          |> List.ofSeq
                          |> List.length))
      let years : YearT seq = 
         seq { 
            for yr in startYear..(startYear + numberOfYears) -> yr |> YearT
         }
      
      let ra = argsDegrees |> Seq.map Geometry.deg2rad
      let args = Seq.zip years ra |> Map.ofSeq
      AssertTM.IsTrue(fun () -> 
         numberOfYears = (nodes
                          |> List.ofSeq
                          |> List.length))
      let nodes = Seq.zip years nodes |> Map.ofSeq
      { Name = name
        Speed = speed
        FirstValidYear = YearT startYear
        LastValidYear = YearT lastValidYear
        Amplitude = amplitude
        Phase = phaseRadians
        Args = args
        Nodes = nodes }
   
   let ValidateYear constituent year = 
      AssertTM.IsTrue(fun () -> constituent.FirstValidYear >= year && constituent.LastValidYear >= year)
   
   [<Obsolete("Use EquilibriumArgument")>]
   let arg constituent year = 
      ValidateYear constituent year
      constituent.Args.[year]

   let EquilibriumArgument constituent year = arg constituent year
   
   [<Obsolete("Use NodeFactor")>]
   let nod constituent year = 
      ValidateYear constituent year
      constituent.Nodes.[year]
   
   let NodeFactor constituent year = nod constituent year


module Year = 
   let value (YearT y) = y