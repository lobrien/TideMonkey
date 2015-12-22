namespace TideMonkey

open System
open System.IO
open System.Xml
open System.Xml.Linq

type ConstantsT = { Index : int; Name : string; Phase : float; Amp : float }
type AliasT = { Format : string; Alias : string; Name : string }
type ConstituentsT = { Name : string; Definition : string; Speed : float }
type DataSetT = { 
    Index : int; 
    Name : string; 
    StationIdContext : string option;
    StationId : string option;
    Coordinates : CoordinatesT; 
    Timezone: string; 
    Country : string; 
    Units : PredictionUnitsT;
    MinDir : float<Degrees> option;
    MaxDir : float<Degrees> option;
    Legalese : string option;
    Notes : string option;
    Comments : string;
    Source : string;
    Restriction : string;
    DateImported : string;
    XFields : string;
    Meridian : string option;
    DatumKind : string option;
    Datum : float option;
    MonthsOnStation : int option;
    LastDateOnStation : string option;
    RefIndex : int option;
    MinTimeAdd : string option;
    MinLevelAdd : float option;
    MinLevelMultiply : float option;
    MaxTimeAdd : string option;
    MaxLevelAdd : float option;
    MaxLevelMultiply : float option;
    FloodBegins : string option;
    EbbBegins : string option;
    OriginalName : string;
    State : string option;
    }



module Harmonics = 

    let aliasesFile = "Resources/Harmonics/aliases.xml"
    let constantsFile = "Resources/Harmonics/constants.xml"
    let constituentsFile = "Resources/Harmonics/constituents.xml"
    let datasetFile = "Resources/Harmonics/data_sets.xml"

    let xn s = XName.Get s

    let ConstantsFromTableElement (el : XElement) = 
        try
            { 
                Index = el.Element(xn "index").Value |> int
                Name = el.Element(xn "name").Value
                Phase = el.Element(xn "phase").Value |> float
                Amp = el.Element(xn "amp").Value |> float
            }
            |> Some
        with 
        | x -> Assert.LogException x; None

    let AliasesFromTableElement (el : XElement) = 
        try
            { 
                Format = el.Element(xn "format").Value
                Alias = el.Element(xn "alias").Value
                Name = el.Element(xn "name").Value
            }
            |> Some
        with 
        | x -> Assert.LogException x; None

    let ReadFromFile (fileName : string) builder = 
        let xdoc = XElement.Load(fileName)
        xdoc.Elements()
        |> Seq.map builder
    
    let Aliases = ReadFromFile aliasesFile AliasesFromTableElement

    let Constants = ReadFromFile constantsFile ConstantsFromTableElement

    let Constituents = ReadFromFile constituentsFile (fun el -> 
        try 
            {
                Name = el.Element(xn "name").Value;
                Definition = el.Element(xn "definition").Value;
                Speed = el.Element(xn "speed").Value |> float;
            } |> Some
        with 
        | x -> Assert.LogException x; None
        )

    let PredictionUnits (s : string) = 
        match s.ToLower() with 
        | "meters" -> Meters
        | "feet" -> Feet
        | "knots" -> Knots
        | "knots^2" -> KnotsSquared
        | "zulu" -> Zulu
        | x -> raise (new ArgumentException(x))


    let DataSetFromTableElement (el : XElement) = 
        let OptionEl name t = 
                match el.Element(xn name) = null with 
                | true -> None
                | false -> el.Element(xn name).Value |> t |> Some

        try
            let index = el.Element(xn "index").Value |> int
            let name = el.Element(xn "name").Value
            let lat = el.Element(xn "lat").Value |> float |> fun f -> f * 1.0<Degrees>
            let lng = el.Element(xn "lng").Value |> float |> fun f -> f * 1.0<Degrees>

            let timezone = el.Element(xn "timezone").Value
            let country = el.Element(xn "country").Value
            let units = el.Element(xn "units").Value |> PredictionUnits

            let comments = el.Element(xn "comments").Value
            let source = el.Element(xn "source").Value
            let restrictions = el.Element(xn "restriction").Value
            let dateImported = el.Element(xn "date_imported").Value
            let xfields = el.Element(xn "xfields").Value


            let minDir = OptionEl "min_dir" float |> Option.bind (fun f -> f * 1.0<Degrees> |> Some) 
            let maxDir = OptionEl "max_dir" float |> Option.bind (fun f -> f * 1.0<Degrees> |> Some) 

            let stationIdContext = OptionEl "station_id_context" string
            let stationId = OptionEl "station_id" string
            let datumKind = OptionEl "datumkind" string
            let legalese = OptionEl "legalese" string
            let notes = OptionEl "notes" string
            let monthsOnStation = OptionEl "months_on_station" int
            let lastDateOnStation = OptionEl "last_date_on_station" string
            let refIndex = OptionEl "ref_index" int
           
            {
                Index = index;
                Name = name;
                StationIdContext = stationIdContext;
                StationId = stationId;
                Coordinates = { Latitude = lat; Longitude = lng };
                Timezone = timezone;
                Country =  country;
                Units = units;
                MinDir = minDir;
                MaxDir = maxDir;
                Legalese = legalese;
                Notes = notes;
                Comments = comments;
                Source = source;
                Restriction = restrictions;
                DateImported = dateImported;
                XFields = xfields;
                Meridian = OptionEl "meridian" string;
                DatumKind = datumKind
                Datum = OptionEl "datum" float;
                MonthsOnStation = monthsOnStation;
                LastDateOnStation = lastDateOnStation;
                RefIndex = refIndex;
                MinTimeAdd = OptionEl "min_time_add" string;
                MinLevelAdd = OptionEl "min_level_add" float;
                MinLevelMultiply = OptionEl "min_level_multiply" float;
                MaxTimeAdd = OptionEl "max_time_add" string;
                MaxLevelAdd = OptionEl "max_level_add" float;
                MaxLevelMultiply = OptionEl "max_level_multiply" float;
                FloodBegins = OptionEl "flood_begins" string;
                EbbBegins = OptionEl "ebb_begins" string;
                OriginalName = el.Element(xn "original_name").Value;
                State = OptionEl "state" string;
            }
            |> Some
        with 
        | x -> Assert.LogException x; None

    let DataSets = ReadFromFile datasetFile DataSetFromTableElement