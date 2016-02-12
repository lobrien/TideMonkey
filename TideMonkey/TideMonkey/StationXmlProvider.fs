namespace TideMonkey

open System
open System.Xml.Linq

module StationXmlProvider = 
   type AliasT = { Format : string; Alias : string; Name : string }

   //Note that AmplitudeT.Units is not in XML
   type ConstantT = { Index : int; Name : string; Phase : float<Radians>; AmplitudeValue : float }

   type XmlConstituentT = { Name : string; Definition : string; Speed : float }

   type DataSetT = 
      {
         Index : int;
         Name : string;
         StationIdContext : string option;
         StationId : string option;
         Coordinates : CoordinatesT;
         Timezone : string;
         Country : string;
         Units : PredictionUnitsT;
         MinimumDirection : float<Degrees> option;
         MaximumDirection : float<Degrees> option;
         Comments : string;
         Source : string;
         Restriction : string;
         DateImported : DateTime;
         XFields : string;
         RefIndex : int option;
         MinTimeAdd : string option;
         MaxTimeAdd : string option;
         MaxLevelMultiple : float option
         Meridian : string option; //Interesting! 
         Datum : float option;
         OriginalName : string;
         State : string option;
      }

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

            (ix, {
            Index = ix;
            Name = xel.Element(xn "name").Value;
            StationIdContext = TryValue "station_id_context";
            StationId = TryValue "station_id";
            Coordinates = coordinates;
            Timezone = xel.Element(xn "timezone").Value;
            Country = xel.Element(xn "country").Value;
            Units = predictionUnits;
            MinimumDirection = minDir;
            MaximumDirection = maxDir;
            Comments = xel.Element(xn "comments").Value;
            Source = xel.Element(xn "source").Value;
            Restriction = xel.Element(xn "restriction").Value;
            DateImported = dateImported;
            XFields = xel.Element(xn "xfields").Value;
            RefIndex = TryValue "ref_index" |> Option.map int;
            MinTimeAdd = TryValue "min_time_add";
            MaxTimeAdd = TryValue "max_time_add";
            MaxLevelMultiple = TryValue "max_level_multiple" |> Option.map float;
            Meridian = TryValue "meridian"; //Interesting! 
            Datum = TryValue "datum" |> Option.map float;
            OriginalName = xel.Element(xn "original_name").Value;
            State = TryValue "state";
            })
         with
         | x -> 
            System.Console.WriteLine(xel.ToString())
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