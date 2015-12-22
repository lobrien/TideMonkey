namespace TideMonkey

open System
open System.IO

module HarmonicsFileParser = 

    type AsciiHeaderData = {
        Version : string;
        Major : int;
        Minor : int;
        LastModified : DateTime;
        AsciiHeaderSize : int64;
        NumberOfRecords : int;
        StartYear : int;
        NumberOfYears : int;
        SpeedBits : int;
        SpeedScale : int;
        SpeedOffset : int;
        EquilibriumBits : int;
        EquilibriumScale : int;
        EquilibriumOffset : int;
        NodeBits : int;
        NodeScale : int;
        NodeOffset : int;
        AmplitudeBits : int;
        AmplitudeScale : int;
        EpochBits : int;
        EpochScale : int;
        RecordTypeBits : int;
        LatitudeBits : int;
        LatitudeScale : int;
        LongitudeBits : int;
        LongitudeScale : int;
        RecordSizeBits : int;
        StationBits : int;
        DatumOffsetBits : int;
        DatumOffsetScale : int;
        DateBits : int;
        MonthsOnStationBits : int;
        ConfidenceValueBits : int;
        TimeBits : int;
        LevelAddBits : int;
        LevelAddScale : int;
        LevelMultiplyBits : int;
        LevelMultiplyScale : int;
        DirectionBits : int;
        LevelUnitBits : int;
        LevelUnitTypes : int;
        LevelUnitSize : int;
        DirectionUnitBits : int;
        DirectionUnitTypes : int;
        DirectionUnitSize : int;
        RestrictionBits : int;
        RestrictionTypes : int;
        RestrictionSize : int;
        DatumBits : int;
        DatumTypes : int;
        DatumSize : int;
        LegaleseBits : int;
        LegaleseTypes : int;
        LegaleseSize : int;
        ConstituentBits : int;
        Constituents : int;
        ConstituentSize : int;
        TzFileBits : int;
        TzFiles : int;
        TzFileSize : int;
        CountryBits : int;
        Countries : int;
        CountrySize : int;
        EndOfFile : int;
    }


    type HarmonicsFileHeader = {
        AsciiSection : AsciiHeaderData ;
        LevelUnitTypes : string list ; 
        DirectionUnitTypes : string list ; 
        RestrictionTypes : string list ;
        LegaleseTypes : string list;
        TzFiles : string list;
        Countries : string list; 
        DatumTypes : string list;
        Constituents : byte[] list;
    } with 
        override this.ToString() = sprintf "%A" this

    type TideIndex = { 
        Address          : int32;
        RecordSize       : uint32;
        TzFile           : uint16;
        ReferenceStation : int32;
        Latitude         : int32;
        Longitude        : int32;
        RecordType       : byte;
        Name             : string
    }  with
        override this.ToString() = sprintf "%A" this

    type TideStationHeader = {
        RecordNumber     : int32;
        RecordSize       : uint32;
        RecordType       : byte;
        Latitude         : double; //Note difference from index!
        Longitude        : double;
        ReferenceStation : int32;
        TzFile           : int16;
        Name             : string;
    }  with
        override this.ToString() = sprintf "%A" this

    type TideRecord = {
        Header             : TideStationHeader;
        Country            : int16;
        Source             : string;
        Restriction        : byte;
        Comments           : string;
        Notes              : string;
        Legalese           : byte;
        StationIdContext   : string;
        StationId          : string;
        DateImported       : uint32;
        XFields            : string;
        DirectionUnits     : byte;
        MinDirection       : int32;
        MaxDirection       : int32;
        LevelUnits         : byte;

        //Type 1
        DatumOffset        : float;
        Datum              : int16;
        ZoneOffset         : int32;
        ExpirationDate     : uint32;
        MonthsOnStation    : uint16;
        LastDateOnStation  : uint32;
        Confidence         : byte;
        Amplitude          : float[];
        Epoch              : float[];

        //Type 2 
        MinTimeAdd         : int32;
        MinLevelAdd        : float;
        MinLevelMultiply   : float;
        MaxTimeAdd         : int32;
        MaxLevelAdd        : float;
        MaxLevelMultiply   : float;
        FloodBegins        : int32;
        EbbBegins          : int32;
    } with
        override this.ToString() = sprintf "%A" this


    let filename = "Resources/harmonics-dwf-20141224-free.tcd"
    let mutable fp : File = null 
    (* static / globals? may be poor choice for async optimization *)
    let mutable tideIndex : TideIndex option = None
    let mutable currentIndex = -1
    let mutable currentRecord = -1
    let mutable currentSearchIndex = 0ul

    let ReadAsciiFileHeader (rdr : BinaryReader) = 
        rdr.BaseStream.Seek(0L, SeekOrigin.Begin) |> ignore

        let ReadHeaderLineData (rdr : StreamReader) = 
            rdr.ReadLine()
            |> fun s -> s.Split([| '=' |])
            |> List.ofArray
            |> List.map (fun s -> s.Trim())
            |> fun l -> (l.Head, l.Tail.Head)

        let textReader = new StreamReader(rdr.BaseStream)
        {
        Version = textReader.ReadLine();
        Major = textReader |> ReadHeaderLineData |> snd |> int;
        Minor = textReader |> ReadHeaderLineData |> snd |> int;
        LastModified = 
            textReader 
            |> ReadHeaderLineData 
            |> snd 
            //Hard-coded EST because 3-letter TZs are ill-specified (http://stackoverflow.com/questions/241789/parse-datetime-with-time-zone-of-form-pst-cest-utc-etc)
            |> fun s -> s.Replace("EST","-0500")
            |> fun s -> DateTime.ParseExact(s, "yyyy-MM-dd HH:mm zzzzz", System.Globalization.CultureInfo.CreateSpecificCulture("en-us"));
        AsciiHeaderSize = textReader |> ReadHeaderLineData |> snd |> int64;
        NumberOfRecords = textReader |> ReadHeaderLineData |> snd |> int;
        StartYear = textReader |> ReadHeaderLineData |> snd |> int;
        NumberOfYears = textReader |> ReadHeaderLineData |> snd |> int;
        SpeedBits = textReader |> ReadHeaderLineData |> snd |> int;
        SpeedScale = textReader |> ReadHeaderLineData |> snd |> int;
        SpeedOffset = textReader |> ReadHeaderLineData |> snd |> int;
        EquilibriumBits = textReader |> ReadHeaderLineData |> snd |> int;
        EquilibriumScale = textReader |> ReadHeaderLineData |> snd |> int;
        EquilibriumOffset = textReader |> ReadHeaderLineData |> snd |> int;
        NodeBits = textReader |> ReadHeaderLineData |> snd |> int;
        NodeScale = textReader |> ReadHeaderLineData |> snd |> int;
        NodeOffset = textReader |> ReadHeaderLineData |> snd |> int;
        AmplitudeBits = textReader |> ReadHeaderLineData |> snd |> int;
        AmplitudeScale = textReader |> ReadHeaderLineData |> snd |> int;
        EpochBits = textReader |> ReadHeaderLineData |> snd |> int;
        EpochScale = textReader |> ReadHeaderLineData |> snd |> int;
        RecordTypeBits = textReader |> ReadHeaderLineData |> snd |> int;
        LatitudeBits = textReader |> ReadHeaderLineData |> snd |> int;
        LatitudeScale = textReader |> ReadHeaderLineData |> snd |> int;
        LongitudeBits = textReader |> ReadHeaderLineData |> snd |> int;
        LongitudeScale = textReader |> ReadHeaderLineData |> snd |> int;
        RecordSizeBits = textReader |> ReadHeaderLineData |> snd |> int;
        StationBits = textReader |> ReadHeaderLineData |> snd |> int;
        DatumOffsetBits = textReader |> ReadHeaderLineData |> snd |> int;
        DatumOffsetScale = textReader |> ReadHeaderLineData |> snd |> int;
        DateBits = textReader |> ReadHeaderLineData |> snd |> int;
        MonthsOnStationBits = textReader |> ReadHeaderLineData |> snd |> int;
        ConfidenceValueBits = textReader |> ReadHeaderLineData |> snd |> int;
        TimeBits = textReader |> ReadHeaderLineData |> snd |> int;
        LevelAddBits = textReader |> ReadHeaderLineData |> snd |> int;
        LevelAddScale = textReader |> ReadHeaderLineData |> snd |> int;
        LevelMultiplyBits = textReader |> ReadHeaderLineData |> snd |> int;
        LevelMultiplyScale = textReader |> ReadHeaderLineData |> snd |> int;
        DirectionBits = textReader |> ReadHeaderLineData |> snd |> int;
        LevelUnitBits = textReader |> ReadHeaderLineData |> snd |> int;
        LevelUnitTypes = textReader |> ReadHeaderLineData |> snd |> int;
        LevelUnitSize = textReader |> ReadHeaderLineData |> snd |> int;
        DirectionUnitBits = textReader |> ReadHeaderLineData |> snd |> int;
        DirectionUnitTypes = textReader |> ReadHeaderLineData |> snd |> int;
        DirectionUnitSize = textReader |> ReadHeaderLineData |> snd |> int;
        RestrictionBits = textReader |> ReadHeaderLineData |> snd |> int;
        RestrictionTypes = textReader |> ReadHeaderLineData |> snd |> int;
        RestrictionSize = textReader |> ReadHeaderLineData |> snd |> int;
        DatumBits = textReader |> ReadHeaderLineData |> snd |> int;
        DatumTypes = textReader |> ReadHeaderLineData |> snd |> int;
        DatumSize = textReader |> ReadHeaderLineData |> snd |> int;
        LegaleseBits = textReader |> ReadHeaderLineData |> snd |> int;
        LegaleseTypes = textReader |> ReadHeaderLineData |> snd |> int;
        LegaleseSize = textReader |> ReadHeaderLineData |> snd |> int;
        ConstituentBits = textReader |> ReadHeaderLineData |> snd |> int;
        Constituents = textReader |> ReadHeaderLineData |> snd |> int;
        ConstituentSize = textReader |> ReadHeaderLineData |> snd |> int;
        TzFileBits = textReader |> ReadHeaderLineData |> snd |> int;
        TzFiles = textReader |> ReadHeaderLineData |> snd |> int;
        TzFileSize = textReader |> ReadHeaderLineData |> snd |> int;
        CountryBits = textReader |> ReadHeaderLineData |> snd |> int;
        Countries = textReader |> ReadHeaderLineData |> snd |> int;
        CountrySize = textReader |> ReadHeaderLineData |> snd |> int;
        EndOfFile = textReader |> ReadHeaderLineData |> snd |> int; 
        }


    let ReadBinaryHeader (rdr : BinaryReader) (location : int64) (asciiFileHeader : AsciiHeaderData)= 
        rdr.BaseStream.Seek(location, SeekOrigin.Begin) |> ignore
        //The cursor doesn't seem to be advancing on the outer loop. Do I need to force the eval before the next read?

        let readAsciiStringGroup count recordSize = 
            [1 .. count ] 
            |> List.map (fun _ -> rdr.ReadBytes(recordSize) )
            |> List.map (fun bytes -> System.Text.Encoding.ASCII.GetString(bytes).TrimEnd(char 0))

        let curpos = rdr.BaseStream.Position

        let levelUnitTypes = readAsciiStringGroup asciiFileHeader.LevelUnitTypes asciiFileHeader.LevelUnitSize

        let directionUnitTypes = readAsciiStringGroup asciiFileHeader.DirectionUnitTypes asciiFileHeader.DirectionUnitSize

        let restrictionTypes = readAsciiStringGroup asciiFileHeader.RestrictionTypes asciiFileHeader.RestrictionSize


        let postRetrictionPos = rdr.BaseStream.Position
        //TODO: Check this!
        let legaleseTypes = readAsciiStringGroup asciiFileHeader.LegaleseTypes asciiFileHeader.LegaleseSize

        //TODO: I've gone awry somehow and need to seek
        let add = asciiFileHeader.LegaleseTypes * asciiFileHeader.LegaleseSize
        let startOfTzFiles = 4730L
        rdr.BaseStream.Seek(startOfTzFiles, SeekOrigin.Begin) |> ignore

        let tzFiles = readAsciiStringGroup asciiFileHeader.TzFiles asciiFileHeader.TzFileSize

        //TODO: Again, another big seek
        let seekPos = rdr.BaseStream.Position
        let dataPos = 35420L
        rdr.BaseStream.Seek(dataPos, SeekOrigin.Begin) |> ignore

        let countries = readAsciiStringGroup asciiFileHeader.Countries asciiFileHeader.CountrySize


        //TODO: I think datum types starts at 
        rdr.BaseStream.Seek(61020L, SeekOrigin.Begin) |> ignore

        let datumTypes = readAsciiStringGroup asciiFileHeader.DatumTypes asciiFileHeader.DatumSize

        //TODO: Find out what data type this is actually supposed to be!
        let constituents = 
            [ 1 .. asciiFileHeader.Constituents ]
            |> List.map (fun _ -> rdr.ReadBytes(asciiFileHeader.ConstituentSize ) )

        //TODO: Inner constituents. 
        //TODO: My guess is that these are the ones that start at 71100

        { 
            AsciiSection = asciiFileHeader ;
            LevelUnitTypes = levelUnitTypes ; 
            DirectionUnitTypes = directionUnitTypes ; 
            RestrictionTypes = restrictionTypes ;
            LegaleseTypes = legaleseTypes;
            TzFiles = tzFiles;
            Countries = countries; 
            DatumTypes = datumTypes;
            Constituents = constituents;
        }

    let ReadRecord position (rdr : BinaryReader) = 
        rdr.BaseStream.Seek(position, SeekOrigin.Begin) |> ignore
        let recordTypeBits = 4
        let latitudeBits = 25
        let longitudeBits = 26
        let tzFileBits = 10

        let beforeNameRecordSize = 
            recordTypeBits + 
            latitudeBits + 
            longitudeBits + 
            tzFileBits 
            |> int64

        rdr.BaseStream.Seek(- beforeNameRecordSize, SeekOrigin.Current) |> ignore
        let beforeData = rdr.ReadBytes (int beforeNameRecordSize / 8)
        let stationName = rdr.ReadBytes(50) |> fun bytes -> System.Text.Encoding.ASCII.GetString(bytes)

        -1

    let Index () = 
        try
            use fileStream = File.OpenRead(filename) 
            use binaryReader = new BinaryReader(fileStream)

            let asciiFileHeader = ReadAsciiFileHeader binaryReader
            let dataBegin = asciiFileHeader.AsciiHeaderSize 
            //Hard-code the extra 4 bytes because that seems to fit the data!
            let fileHeader = ReadBinaryHeader binaryReader (dataBegin + 4L) asciiFileHeader
            Some fileHeader
        with
        | x -> System.Console.WriteLine(x.ToString()); None