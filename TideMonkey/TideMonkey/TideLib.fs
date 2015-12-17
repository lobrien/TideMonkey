namespace TideMonkey

open System
open SkyCal

type StationT = { Name : string ; Coordinates : CoordinatesT}

type PredictionT<[<Measure>] 'u> = { Magnitude : float<'u> ; Direction : DirectionT ; Units : PredictionUnitsT}

type SimpleOffsetsT<[<Measure>] 'u> = { TimeAdd : IntervalT; LevelAdd : PredictionT<'u>; LevelMultiply : float }

type ConstituentSetT<[<Measure>] 'speedT, [<Measure>] 'amplitudeT, [<Measure>] 'predictionUnitsT> = { Constituents : ConstituentT<'speedT, 'amplitudeT> List; Datum : PredictionT<'predictionUnitsT>; Adjustments : SimpleOffsetsT<'predictionUnitsT> List }

type SunMoonEventsT = { Sunrises : DateTime list; Sunsets : DateTime list; Moonrises : DateTime list; Moonsets : DateTime list; MoonPhases : (DateTime * MoonPhaseT) list }

module Station = 

    let LoadStations (pathToHarmonicsFile : string) = 
       //TODO: Actually do this 
       Some [  { Name = "Kailua Kona"; Coordinates = { Latitude = 19.65<Degrees>; Longitude = 155.9942<Degrees> } };  
               { Name = "Anchorage" ; Coordinates = { Latitude = 61.2167<Degrees>; Longitude = 149.9000<Degrees> } };  
               { Name = "Monterey" ; Coordinates = { Latitude = 36.6000<Degrees>; Longitude = 121.9000<Degrees> } }
            ]

    let Named name stations = 
        stations |> List.tryFind (fun s -> s.Name = name)

    type StationT with
        member this.AddSunMoonEvents (startTime : DateTime) (endTime : DateTime)  = 
            //Should return sunrise, sunsets, and next moon phase that is within range, so
            let rec dates (t : DateTime) = seq { 
                let s = endTime.Subtract(t).TotalSeconds
                match s > 0. with
                | true -> 
                    yield t 
                    yield! dates (t.AddDays(1.0))
                | false -> ignore()
            }

            let sunEvents = dates startTime |> Seq.map( fun d -> 
                let h0 = -0.8333<Degrees> //Standard for Sun 
                let ephemeris = Ephemeris.GreenwichEphemeris d "Solar"
                let results = Meeus.RisingAndSetting this.Coordinates ephemeris h0
                results
                )

            let moonEvents = dates startTime |> Seq.map (fun d -> 
                let h0 = 0.125<Degrees> //Mean for moon (approx. see Meeus Ch. 15) 
                let ephemeris = Ephemeris.GreenwichEphemeris d "Lunar"
                let results = Meeus.RisingAndSetting this.Coordinates ephemeris h0
                results
                )

            let moonPhasesInRange = 
                dates startTime 
                |> Seq.map (fun date -> Moon.NextMoonPhase date)
                |> Seq.distinct
                |> List.ofSeq

            let sunrises = sunEvents |> Seq.map (fun d -> d.Rising) |> List.ofSeq
            let sunsets = sunEvents |> Seq.map (fun d -> d.Setting) |> List.ofSeq
            let moonrises = moonEvents |> Seq.map (fun d -> d.Rising) |> List.ofSeq
            let moonsets = moonEvents |> Seq.map (fun d -> d.Setting) |> List.ofSeq


            { Sunrises = sunrises; Sunsets = sunsets; Moonrises = moonrises; Moonsets = moonsets; MoonPhases = moonPhasesInRange }


        member this.Predict time = 
            { Magnitude = 0.0<Feet> ; Direction = Rising ; Units = Feet }