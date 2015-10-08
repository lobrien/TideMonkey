namespace TideMonkey

type StationT = { Name : string }

type PredictionT<[<Measure>] 'u> = { Magnitude : float<'u> ; Direction : DirectionT ; Units : PredictionUnitsT}

type SimpleOffsetsT<[<Measure>] 'u> = { TimeAdd : IntervalT; LevelAdd : PredictionT<'u>; LevelMultiply : float }

type ConstituentSetT<[<Measure>] 'speedT, [<Measure>] 'amplitudeT, [<Measure>] 'predictionUnitsT> = { Constituents : ConstituentT<'speedT, 'amplitudeT> List; Datum : PredictionT<'predictionUnitsT>; Adjustments : SimpleOffsetsT<'predictionUnitsT> List }


module Station = 

   let LoadStations (pathToHarmonicsFile : string) = 
       //TODO: Actually do this 
       Some [  { Name = "Kailua Kona" } ;  { Name = "Anchorage" } ;  { Name = "Montery" } ]

   let Named name stations = 
        stations |> List.tryFind (fun s -> s.Name = name)
        
   let Predict station time = 
        { Magnitude = 0.0<Feet> ; Direction = Rising ; Units = Feet }