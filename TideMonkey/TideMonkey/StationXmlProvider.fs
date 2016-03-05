namespace TideMonkey

open System
open System.Xml.Linq

module StationXmlProvider = 
   type AliasT = { Format : string; Alias : string; Name : string }

   //Note that AmplitudeT.Units is not in XML
   type ConstantT = { Index : int; Name : string; Phase : float<Radians>; AmplitudeValue : float }

   type XmlConstituentT = { Name : string; Definition : string; Speed : float }

   type EpochT = float

   type DataSetT = 
      {
         //TIDE_STATION_HEADER
         //See tcd.hash lines 132-142
         Index : int;
         RecordSize: int;
         RecordType : int; //Not sure about this, but possibly 1 if ReferenceStationT, 2 if SubordinateStationT (tcd.h ln 130)
         Coordinates : CoordinatesT;
         TzFile : int; //Not sure about this. TimeZoneFile, perhaps? (Same as Timezone below?)
         Name : string;
         

         //TIDE_RECORD
         //See tcd.h line 151 - 165
         Country : string;
         Source : string;
         Restriction : string;
         Comments : string;
         Notes : string;
         Legalese : string;
         DateImported : DateTime;
         XFields : string;
         DirectionUnits : int; //Not sure about this. Is this Radians / Degrees distinction? (If so, I've decided Degrees per MinDir MaxDir below)
         MinimumDirection : float<Degrees> option;
         MaximumDirection : float<Degrees> option;
         LevelUnits: int; //Not sure about this. PredictionUnitsT maybe? (Same as Units below?) 
         
         Timezone : string;
         Units : PredictionUnitsT;
         Meridian : string option; //Interesting! 
         OriginalName : string;
         State : string option;

         StationId : string option;  //Almost always an int, but not always. eg: TEC4513
         StationIdContext : string option;
       
      }

   type ReferenceStationT = 
      {
         Common : DataSetT;
         DatumOffset : int;   //No sign of datum_offset or datumoffset in XML
         Datum : float;
         ZoneOffset : int;
         ExpirationDate : DateTime;
         MonthOnStation : int;
         LastDateOnStation : DateTime;
         Confidence : int;
         Amplitudes : AmplitudeT list;
         Epochs : EpochT list;
      }

   type SubordinateStationT = 
      {
         Common : DataSetT;
         RefIndex : int;   // ReferenceStationT.Common.Index of reference station
         MinTimeAdd : float<Hours> option;
         MinLevelAdd : float option;
         MinLevelMultiply : float option;
         MaxTimeAdd : float<Hours> option;
         MaxLevelAdd : float option;
         MaxLevelMultiply : float option;
         FloodBegins : float<Hours> option;
         EbbBegins : float<Hours> option;
      }

   type TideRecordT = 
      | SubordinateStation of SubordinateStationT
      | ReferenceStation of ReferenceStationT

   let flatten =
      function
      | Some a -> a
      | None -> None

   let xn s = XName.Get(s)

   let fromXmlPath (path : string) (fn : XElement -> ('k * 'v)) = 
      let xdoc = XDocument.Load(path)
      let tables = xdoc.Descendants(xn "Table")
      tables |> Seq.map fn |> Map.ofSeq

   let aliasFromXml (xel : XElement) = 
      (
      xel.Element(xn "name").Value,
      { 
      Format = xel.Element(xn "format").Value;
      Alias = xel.Element(xn "alias").Value;
      Name = xel.Element(xn "name").Value;
      }
      )

   let aliasesByName (path : string) = fromXmlPath path aliasFromXml

   let constantFromXml (xel : XElement) = 
      let ix = xel.Element(xn "index").Value |> int
      let name = xel.Element(xn "name").Value
      (
      (ix, name),
      {
         Index = ix;
         Name = name;
         Phase = xel.Element(xn "phase").Value |> float |> (*) 1.0<Radians>;
         AmplitudeValue = xel.Element(xn "amp").Value |> float;
      })

   let constantsByIndex path = fromXmlPath path constantFromXml

   let constituentsByName path = 
      let constituentFromXml (xel : XElement) = 
         (
         xel.Element(xn "name").Value,
         { 
            XmlConstituentT.Name = xel.Element(xn "name").Value;
            Definition = xel.Element(xn "definition").Value;
            //TODO: Double-check to see if this is constant in DB, e.g radians / hour
            Speed = xel.Element(xn "speed").Value |> float;
         }
         )

      fromXmlPath path constituentFromXml

   let dataSetsByIndex path = 
      let dataSetFromXml (xel : XElement) = 
         let TryValue s = 
            match (xel.Element(xn s) = null) with 
            | true -> None
            | false -> xel.Element(xn s).Value |> Some


         let TryParseTimeAdd s =
            try
               // Regular expression. I feel so dirty.
               let pattern = "(?'Negative'\-)?PT(?'Hours'[0-9]+H)?(?'Minutes'[0-9]+M)?"
               let reMatch = System.Text.RegularExpressions.Regex.Match(s, pattern)
               match reMatch.Success with
               | true ->
                  let sign = match reMatch.Groups.["Negative"].Success with
                                   | true -> -1.
                                   | false -> 1.
                                 
                  let hours = match reMatch.Groups.["Hours"].Success with
                              | true -> reMatch.Groups.["Hours"].Value |> fun s -> s.Substring(0, s.Length - 1) |> float
                              | false -> 0.
                  let minutes = match reMatch.Groups.["Minutes"].Success with
                                | true -> reMatch.Groups.["Minutes"].Value |> fun s -> s.Substring(0, s.Length - 1) |> float
                                | false -> 0.

                  let offset =  sign * hours + (minutes / 60.0) |> fun m -> m * 1.0<Hours> |> Some
                  offset
               | false ->
                  System.Console.WriteLine("Failed to parse time offset string " + s) 
                  None

            with
            | x -> 
               System.Console.WriteLine(x.ToString())
               None

         try
            let ix = xel.Element(xn "index").Value |> int;
            let coordinates = {
               Latitude = xel.Element(xn "lat").Value |> float |> (*) 1.<Degrees>;
               Longitude = xel.Element(xn "lng").Value |> float |> (*) 1.<Degrees>;
               }
            let predictionUnits = 
               match (xel.Element(xn "units").Value) with
               | "knots^2" -> PredictionUnitsT.KnotsSquared
               | "knots" -> PredictionUnitsT.Knots
               | "feet" -> PredictionUnitsT.Feet
               | "meters" -> PredictionUnitsT.Meters
               | "zulu" -> PredictionUnitsT.Zulu
               | s -> raise <| new ArgumentOutOfRangeException("Did not recognize prediction unit: " + s)

            let minDir = TryValue "min_dir" |> Option.map float |> Option.map (fun f -> f * 1.<Degrees>)
            let maxDir = TryValue "max_dir" |> Option.map float |> Option.map (fun f -> f * 1.<Degrees>)

            let dateImported = DateTime.Parse(xel.Element(xn "date_imported").Value)

            let commonData : DataSetT = 
               {
                  Index = ix;
                  RecordSize = -1;
                  RecordType = -1; //But maybe 1 if Reference, 2 if Subordinate
                  Coordinates = coordinates;
                  TzFile = -1; 
                  Name = xel.Element(xn "name").Value;
                  StationId = TryValue  "station_id";
                  StationIdContext = TryValue "station_id_context";
                  Country = xel.Element(xn "country").Value;
                  Source = xel.Element(xn "source").Value;
                  Comments = xel.Element(xn "comments").Value;
                  Restriction = xel.Element(xn "restriction").Value;
                  Notes = "";
                  Legalese = "";
                  DateImported = dateImported; 
                  XFields = xel.Element(xn "xfields").Value;
                  DirectionUnits = -1;
                  MinimumDirection = minDir;
                  MaximumDirection = maxDir;
                  LevelUnits = -1;
                  Timezone = xel.Element(xn "timezone").Value;
                  Units = predictionUnits;
                  Meridian = TryValue "meridian";
                  OriginalName = xel.Element(xn "original_name").Value;
                  State = TryValue "state";
               }


            let tideRecord = 
               match TryValue "ref_index" with
               | None ->
                  //Reference Station
                  let amplitudes : List<AmplitudeT> = List.empty //TODO -- these are not in data_sets.xml, so they must be passed in. Probably map DataSetT.Index -> Amplitudes and DataSetT.Index -> Epochs
                  let epochs : List<EpochT> = List.empty     //TODO

                  ReferenceStation { 
                     Common = commonData;
                     DatumOffset = -1; 
                     Datum = xel.Element(xn "datum").Value |> float;
                     ZoneOffset = -1;     //Perhaps the calculated value of the timezone vis a vis UTC? 
                     ExpirationDate = new DateTime(2019, 12, 31)
                     MonthOnStation = -1;
                     LastDateOnStation = new DateTime(1900, 1, 1)
                     Confidence = -1;
                     Amplitudes = amplitudes;
                     Epochs = epochs;
                  } 

               | Some _ ->
                  //SubordinateStation
                  //Note: explicitness required because cannot set breakpoint on inline expression in SubordinateStation ctor
                  let re = xel.Element(xn "ref_index").Value |> int
                  let mta = TryValue "min_time_add"  |> Option.map TryParseTimeAdd |> flatten
                  let mxta = TryValue "max_time_add" |> Option.map TryParseTimeAdd |> flatten
                  let mla = TryValue "min_level_add" |> Option.map float
                  let mlm = TryValue "min_level_multiply" |> Option.map float
                  let mxla = TryValue "max_level_add" |> Option.map float
                  let mxlm = TryValue "max_level_multiply" |> Option.map float
                  let fb = TryValue "flood_begins" |> Option.map TryParseTimeAdd |> flatten
                  let eb = TryValue "ebb_begins" |> Option.map TryParseTimeAdd |> flatten
                  SubordinateStation {
                     Common = commonData;
                     RefIndex = re;
                     MinTimeAdd = mta;
                     MaxTimeAdd = mxta;
                     MinLevelAdd = mla;
                     MinLevelMultiply = mlm;
                     MaxLevelAdd = mxla;
                     MaxLevelMultiply = mxlm;
                     FloodBegins = fb;
                     EbbBegins = eb;
                  } 


            (ix, tideRecord)
         with
         | x -> 
            System.Console.WriteLine(x.ToString())
            raise <| x

      fromXmlPath path dataSetFromXml

   let dropsByName path = 
      let dropFromXml (xel : XElement) = 
         (
         xel.Element(xn "name").Value,
         (xel.Element(xn "name").Value, xel.Element(xn "format").Value)
         )

      fromXmlPath path dropFromXml

   let phasesByStationId path = 
      let phaseFromXml (xel:XElement) = 
         (
         xel.Element(xn "station_id").Value,
         (
         xel.Element(xn "station_id").Value,
         xel.Element(xn "m2phase").Value |> float
         )
         )
      
      fromXmlPath path phaseFromXml

   let equilibriaByConstituentIdAndYear (path : string) = 
      let xdoc = XDocument.Load(path)
      let es = xdoc.Descendants(xn "equilibrium")
      es 
      |> Seq.map (fun el -> 
         let constituentId = el.Element(xn "constituent").Value |> int
         let year = el.Element(xn "year").Value |> int
         let value = el.Element(xn "value").Value |> float
         ((constituentId, year), (constituentId, year, value)))
      |> Map.ofSeq


   let nodeFactorsByConstituentIdAndYear (path : string) = 
      let xdoc = XDocument.Load(path)
      let es = xdoc.Descendants(xn "node_factor")
      es 
      |> Seq.map (fun el -> 
         let constituentId = el.Element(xn "constituent").Value |> int
         let year = el.Element(xn "year").Value |> int
         let value = el.Element(xn "value").Value |> float
         ((constituentId, year), (constituentId, year, value)))
      |> Map.ofSeq


   let EquilibriumFromYearAndFloat (eqs : Map<(int * int), (int * int * float)>) (key : (int *int)) =
     match eqs.ContainsKey(key) with
     | true -> Some (eqs.[key])
     | false -> None

   