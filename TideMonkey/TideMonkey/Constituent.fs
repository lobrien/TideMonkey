namespace TideMonkey

open System

type ConstituentT = { Name : string; Speed : SpeedT; FirstValidYear : Year; LastValidYear : Year; Amplitude : AmplitudeT; Phase : float<Radians> ; Args: (Year * float<Radians>) list; Nodes : Map<Year, float>}

module Constituent =    
    open Geometry
   
    let CreateFromUntypedInputs name speedDegreesPerSecond startYear numberOfYears argsDegrees (nodes : float list) amplitude phaseDegrees = 
        let typedSpeed = speedDegreesPerSecond * 1.0<Degrees> * 1.0<Seconds> * 3600.<Seconds> / 1.<Hours>
        let speed = Speed.Convert(typedSpeed)
        let lastValidYear = startYear + numberOfYears - 1
        Assert.IsTrue(fun () -> lastValidYear >= startYear)

        let phaseRadians = deg2rad phaseDegrees

        Assert.IsTrue(fun () -> numberOfYears = (argsDegrees |> List.ofSeq |> List.length) )
        let years : Year seq = seq { for yr in startYear .. (startYear + numberOfYears) -> yr } 
        let ra = argsDegrees |> Seq.map Geometry.deg2rad
        let args = Seq.zip years ra |> List.ofSeq

        Assert.IsTrue(fun () -> numberOfYears = (nodes |> List.ofSeq |> List.length) )
        let nodes = Seq.zip years nodes |> Map.ofSeq

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
        constituent.Nodes.[year]

    let NodeFactor constituent year = 
        nod constituent year

