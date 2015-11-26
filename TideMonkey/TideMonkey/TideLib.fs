namespace TideMonkey

open System
open SkyCal

type StationT = { Name : string }

type PredictionT<[<Measure>] 'u> = { Magnitude : float<'u> ; Direction : DirectionT ; Units : PredictionUnitsT}

type SimpleOffsetsT<[<Measure>] 'u> = { TimeAdd : IntervalT; LevelAdd : PredictionT<'u>; LevelMultiply : float }

type ConstituentSetT<[<Measure>] 'speedT, [<Measure>] 'amplitudeT, [<Measure>] 'predictionUnitsT> = { Constituents : ConstituentT<'speedT, 'amplitudeT> List; Datum : PredictionT<'predictionUnitsT>; Adjustments : SimpleOffsetsT<'predictionUnitsT> List }

type SunMoonEventsT = { Sunrises : DateTime list; Sunsets : DateTime list; Moonrises : DateTime list; Moonsets : DateTime list; MoonPhases : Tuple<DateTime,MoonPhaseT> list }

module Station = 

    let LoadStations (pathToHarmonicsFile : string) = 
       //TODO: Actually do this 
       Some [  { Name = "Kailua Kona" } ;  { Name = "Anchorage" } ;  { Name = "Montery" } ]

    let Named name stations = 
        stations |> List.tryFind (fun s -> s.Name = name)

    type StationT with
        member this.AddSunMoonEvents (startTime : DateTime) (endTime : DateTime)  = 
            //Should return sunrise, sunsets, and next moon phase that is within range, so
            //TODO: Sunrises
            //TODO: Sunsets
            //TODO: Moonrises
            //TODO: Moonsets
            //TODO: MoonPhases
            { Sunrises = []; Sunsets = []; Moonrises = []; Moonsets = []; MoonPhases = [] }


        member this.Predict time = 
            { Magnitude = 0.0<Feet> ; Direction = Rising ; Units = Feet }