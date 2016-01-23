namespace TideMonkey

open System
open System.IO
open System.Xml
open System.Xml.Linq

type ConstantsT = 
   { Index : int
     Name : string
     Phase : float<Degrees>
     Amp : float }

type AliasT = 
   { Format : string
     Alias : string
     Name : string }

type ConstituentsDataT = 
   { Name : string
     Definition : string
     Speed : float<Degrees / Hours> }

type DataSetT = 
   { Index : int
     Name : string
     StationIdContext : string option
     StationId : string option
     Coordinates : CoordinatesT
     Timezone : string
     Country : string
     Units : PredictionUnitsT
     MinDir : CurrentBearingT option
     MaxDir : CurrentBearingT option
     Legalese : string option
     Notes : string option
     Comments : string
     Source : string
     Restriction : string
     DateImported : string
     XFields : string
     Meridian : string option
     DatumKind : string option
     Datum : float option
     MonthsOnStation : int option
     LastDateOnStation : string option
     RefIndex : int option
     MinTimeAdd : string option
     MinLevelAdd : float option
     MinLevelMultiply : float option
     MaxTimeAdd : string option
     MaxLevelAdd : float option
     MaxLevelMultiply : float option
     FloodBegins : string option
     EbbBegins : string option
     OriginalName : string
     State : string option }

type RefT = 
   { StationId : string
     M2Phase : float }

type DbHeaderT = 
   { StartYear : int //1700 (2015)
     NumberOfYears : int //401 (4)
                         }

type EquilibriumT = 
   { Constituent : int
     Year : Year
     Value : float<Degrees> }

type NodeFactorT = 
   { Constituent : int
     Year : Year
     Value : float }

module Harmonics = 
   let aliasesFile = "Resources/Harmonics/aliases.xml"
   let constantsFile = "Resources/Harmonics/constants.xml"
   let constituentsFile = "Resources/Harmonics/constituents.xml"
   let datasetFile = "Resources/Harmonics/data_sets.xml"
   let refsFile = "Resources/Harmonics/refs2014.xml"
   let equilibriaFile = "Resources/Harmonics/equilibria.xml"
   let nodeFactorsFile = "Resources/Harmonics/node_factors.xml"
   let xn s = XName.Get s
   
   let ConstantsFromTableElement(el : XElement) = 
      try 
         { Index = el.Element(xn "index").Value |> int
           Name = el.Element(xn "name").Value
           Phase = 
              el.Element(xn "phase").Value
              |> float
              |> (*) 1.0<Degrees>
           Amp = el.Element(xn "amp").Value |> float }
         |> Some
      with x -> 
         Assert.LogException x
         None
   
   let AliasesFromTableElement(el : XElement) = 
      try 
         { Format = el.Element(xn "format").Value
           Alias = el.Element(xn "alias").Value
           Name = el.Element(xn "name").Value }
         |> Some
      with x -> 
         Assert.LogException x
         None
   
   let ReadFromFile (fileName : string) builder = 
      let xdoc = XElement.Load(fileName)
      xdoc.Elements() |> Seq.map builder
   
   let Aliases = ReadFromFile aliasesFile AliasesFromTableElement
   let Constants = ReadFromFile constantsFile ConstantsFromTableElement
   
   let Constituents = 
      ReadFromFile constituentsFile (fun el -> 
         try 
            { Name = el.Element(xn "name").Value
              Definition = el.Element(xn "definition").Value
              Speed = 
                 el.Element(xn "speed").Value
                 |> float
                 |> fun f -> f * 1.0<Degrees/Hours> }
            |> Some
         with x -> 
            Assert.LogException x
            None)
   
   let Equilibria = 
      ReadFromFile equilibriaFile (fun el -> 
         { EquilibriumT.Constituent = el.Element(xn "constituent").Value |> int
           Year = el.Element(xn "year").Value |> int
           Value = 
              el.Element(xn "value").Value
              |> float
              |> fun f -> f * 1.0<Degrees> })
      |> Seq.zip Constituents
   
   let NodeFactors = 
      ReadFromFile nodeFactorsFile (fun el -> 
         { NodeFactorT.Constituent = el.Element(xn "constituent").Value |> int
           Year = el.Element(xn "year").Value |> int
           Value = el.Element(xn "value").Value |> float })
      |> Seq.zip Constituents
   
   let PredictionUnits(s : string) = 
      match s.ToLower() with
      | "meters" -> Meters
      | "feet" -> Feet
      | "knots" -> Knots
      | "knots^2" -> KnotsSquared
      | "zulu" -> Zulu
      | x -> raise (new ArgumentException(x))
   
   let DataSetFromTableElement(el : XElement) = 
      let OptionEl name t = 
         match el.Element(xn name) = null with
         | true -> None
         | false -> 
            el.Element(xn name).Value
            |> t
            |> Some
      try 
         let index = el.Element(xn "index").Value |> int
         let name = el.Element(xn "name").Value
         
         let lat = 
            el.Element(xn "lat").Value
            |> float
            |> fun f -> f * 1.0<Degrees>
         
         let lng = 
            el.Element(xn "lng").Value
            |> float
            |> fun f -> f * 1.0<Degrees>
         
         let timezone = el.Element(xn "timezone").Value
         let country = el.Element(xn "country").Value
         let units = el.Element(xn "units").Value |> PredictionUnits
         let comments = el.Element(xn "comments").Value
         let source = el.Element(xn "source").Value
         let restrictions = el.Element(xn "restriction").Value
         let dateImported = el.Element(xn "date_imported").Value
         let xfields = el.Element(xn "xfields").Value
         
         let minDir = 
            OptionEl "min_dir" float |> Option.bind (fun f -> 
                                           f * 1.0<Degrees>
                                           |> fun d -> 
                                              { Degrees = d
                                                IsDegreesTrue = true }
                                           |> Some)
         
         let maxDir = 
            OptionEl "max_dir" float |> Option.bind (fun f -> 
                                           f * 1.0<Degrees>
                                           |> fun d -> 
                                              { Degrees = d
                                                IsDegreesTrue = true }
                                           |> Some)
         
         let stationIdContext = OptionEl "station_id_context" string
         let stationId = OptionEl "station_id" string
         let datumKind = OptionEl "datumkind" string
         let legalese = OptionEl "legalese" string
         let notes = OptionEl "notes" string
         let monthsOnStation = OptionEl "months_on_station" int
         let lastDateOnStation = OptionEl "last_date_on_station" string
         let refIndex = OptionEl "ref_index" int
         { Index = index
           Name = name
           StationIdContext = stationIdContext
           StationId = stationId
           Coordinates = 
              { Latitude = lat
                Longitude = lng }
           Timezone = timezone
           Country = country
           Units = units
           MinDir = minDir //aka Ebb direction
           MaxDir = maxDir //aka Flood direction
           Legalese = legalese
           Notes = notes
           Comments = comments
           Source = source
           Restriction = restrictions
           DateImported = dateImported
           XFields = xfields
           Meridian = OptionEl "meridian" string //Ref.
           DatumKind = datumKind //Ref.
           Datum = OptionEl "datum" float //Ref.
           MonthsOnStation = monthsOnStation //Ref.
           LastDateOnStation = lastDateOnStation //Ref.
           RefIndex = refIndex //Sub.
           MinTimeAdd = OptionEl "min_time_add" string
           MinLevelAdd = OptionEl "min_level_add" float
           MinLevelMultiply = OptionEl "min_level_multiply" float
           MaxTimeAdd = OptionEl "max_time_add" string
           MaxLevelAdd = OptionEl "max_level_add" float
           MaxLevelMultiply = OptionEl "max_level_multiply" float
           FloodBegins = OptionEl "flood_begins" string
           EbbBegins = OptionEl "ebb_begins" string
           OriginalName = el.Element(xn "original_name").Value
           State = OptionEl "state" string }
         |> Some
      with x -> 
         Assert.LogException x
         None
   
   let DataSets = ReadFromFile datasetFile DataSetFromTableElement
   
   let Refs = 
      ReadFromFile refsFile (fun el -> 
         try 
            { StationId = el.Element(xn "station_id").Value
              M2Phase = el.Element(xn "m2phase").Value |> float }
            |> Some
         with x -> 
            Assert.LogException x
            None)
   
   (*
    let BuildConstituentFrom (constant : ConstantsT) (constituent : ConstituentsT) = 
        let name = constituent.Name
        let speed = constituent.Speed
        let firstValidYear = 1700
        let lastValidYear = 2100
        let amplitude = constant.Amp
        let phase = Geometry.deg2rad constant.Phase
        let args = Map.empty
        let nodes = Map.empty
        { 
        Name = name; 
        Speed = speed; 
        FirstValidYear = firstValidYear; 
        LastValidYear = lastValidYear; 
        Amplitude = amplitude;
        Phase = phase;
        Args = args;
        Nodes = nodes; 
        }


    let BuildConstituentSet index (constants : ConstantsT seq) (constituents : ConstituentsT seq) =
        let constantsRelatingToStation = constants |> Seq.filter (fun c -> c.Index = index)
        let constituentsRelatingToConstants = 
            constituents
            |> Seq.filter (fun constituent -> constantsRelatingToStation |> Seq.exists (fun constant -> constant.Name = constituent.Name) )
         

        constituentsRelatingToConstants
        |> Seq.map (fun constituent -> 
                constantsRelatingToStation 
                |> Seq.find (fun c -> c.Name = constituent.Name)
                |> fun constant -> BuildConstituentFrom constant constituent 
                )

    *)

   let Constituent name (speed : float<Radians / Seconds>) startYear numberOfYears 
       (equilibria : (Year * float<Degrees>) list) (node_factors : Map<Year, float>) amplitude (phase : float<Degrees>) = 
      let args = equilibria |> List.map (fun (y, d) -> (y, Geometry.deg2rad d)) |> Map.ofList
      { Name = name
        Speed = speed
        FirstValidYear = startYear
        LastValidYear = startYear + numberOfYears
        Amplitude = amplitude
        Phase = Geometry.deg2rad phase
        Args = args
        Nodes = node_factors }
   
   let GetConstituents (station : StationT) (adjustments : SimpleOffsetsT) (db : DbHeaderT) = 
      Assert.IsTrue(fun () -> station.StationRef.IsNone)
   
   let StationFrom (dbh : DbHeaderT) (ds : DataSetT) (constants : ConstantsT seq) 
       (constituentsData : ConstituentsDataT seq) (simpleOffset : SimpleOffsetsT) = 
      let name = ds.Name
      let coordinate = ds.Coordinates
      let timezone = ds.Timezone
      let note = ds.Notes
      //TODO: Verify this
      let isReferenceStation = ds.RefIndex.IsNone
      
      let stationTypePrefix = 
         match isReferenceStation with
         | true -> "Reference station, "
         | false -> "Subordinate station, "
      
      let isCurrent = ds.Units = Knots || ds.Units = KnotsSquared
      let isHydraulicCurrent = ds.Units = KnotsSquared
      
      let stationTypeText = 
         stationTypePrefix + match (isCurrent, isHydraulicCurrent) with
                             | (false, _) -> "tide"
                             | (true, true) -> "hydraulic current"
                             | (true, false) -> "current"
      
      let minCurrentBearing = ds.MinDir
      let maxCurrentBearing = ds.MaxDir
      let step = None
      let stationRef = None
      let metadata = None
      //constituents are only in reference
      let constantsRelatingToStation = constants |> Seq.filter (fun c -> c.Index = ds.Index)
      let cdRelatingToConstants = 
         constituentsData 
         |> Seq.filter 
               (fun constituent -> 
               constantsRelatingToStation |> Seq.exists (fun constant -> constant.Name = constituent.Name))
      
      let ConstituentBuilder (cd : ConstituentsDataT) constant args_degrees node_factors = 
         let name = cd.Name
         let speed = Speed.Convert cd.Speed
         let startYear = dbh.StartYear
         let numberOfYears = dbh.NumberOfYears
         
         let amplitude = 
            { Value = constant.Amp
              Units = ds.Units }
         
         let phase = constant.Phase
         Constituent name speed startYear numberOfYears args_degrees node_factors amplitude phase
      
      let ArgsBuilder(constituent : ConstituentsDataT) = 
         let equilibrium = 
            Equilibria
            |> Seq.filter (fun (c, _) -> c.IsSome && c.Value.Name = constituent.Name)
            |> Seq.map snd
         equilibrium |> Seq.map (fun e -> (e.Year, e.Value))
      
      let NodesBuilder(constituent : ConstituentsDataT) = 
         let node_factors = 
            NodeFactors
            |> Seq.filter (fun (c, _) -> c.IsSome && c.Value.Name = constituent.Name)
            |> Seq.map snd
            |> Seq.map (fun t -> (t.Year, t.Value))
            |> Map.ofSeq
         node_factors
      
      let constituents = 
         cdRelatingToConstants
         |> Seq.map (fun cd -> 
               constantsRelatingToStation
               |> Seq.find (fun constant -> constant.Name = cd.Name)
               |> fun constant -> (cd, constant))
         |> Seq.map (fun (constituentData, constant) -> 
               let args = ArgsBuilder constituentData |> List.ofSeq
               let nodes = NodesBuilder constituentData
               ConstituentBuilder constituentData constant args nodes)
         |> List.ofSeq
      
      Assert.IsTrue(fun () -> ds.Datum.IsSome)
      let datum = 
         { Amplitude = 
              { Value = ds.Datum.Value
                Units = ds.Units } }
      
      // Normalize the meridian to UTC.
      // To compensate for a negative meridian requires a positive offset.
      // (This adjustment is combined with any that were passed in.)
      // This is the only place where mutable offsets would make even a
      // little bit of sense.
      let intervalDelta = -0.0<Seconds> //TODO: Where can I get timezone offset for record? 
      let ta = simpleOffset.TimeAdd.Duration + intervalDelta
      
      let adjustments = 
         { TimeAdd = { Duration = ta }
           LevelAdd = simpleOffset.LevelAdd
           LevelMultiply = simpleOffset.LevelMultiply }
      
      //TODO: GARBAGE GARBAGE GARBAGE
      let constituentSet = 
         { Constituents = constituents
           Datum = datum
           Amplitudes = []
           Phases = [ 0.<Radians> ]
           MaxAmplitude = 
              { Value = 0.
                Units = Meters }
           MaxDt = []
           CurrentYear = 2016
           Epoch = new DateTime(2016, 1, 1)
           NextEpoch = new DateTime(2016, 1, 1)
           PreferredLengthUnits = Meters }
      
      let minimumTimeOffset = { Duration = 0.0<Seconds> } //TODO
      let maximumTimeOffset = { Duration = 0.0<Seconds> } //TODO
      { Name = name
        Coordinates = coordinate
        Timezone = timezone
        Note = note
        IsCurrent = isCurrent
        MinCurrentBearing = minCurrentBearing
        MaxCurrentBearing = maxCurrentBearing
        Step = step
        StationRef = stationRef
        Metadata = metadata
        ConstituentSet = constituentSet
        MinimumTimeOffset = minimumTimeOffset
        MaximumTimeOffset = maximumTimeOffset }
