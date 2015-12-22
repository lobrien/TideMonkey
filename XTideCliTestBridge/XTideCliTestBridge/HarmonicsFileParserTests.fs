namespace XTideCliTestBridge

open System
open NUnit.Framework
open TideMonkey
open System.IO

[<TestFixture>]
type HarmonicsFileParserTests() = 
      
    [<Test>]
    member x.CanReadHarmonicsFileHeader() =
        let results = HarmonicsFileParser.Index() 
        Assert.AreEqual(true, results.IsSome)
        let r = results.Value.AsciiSection

        //Not inherently important, just a series of known values
        Assert.AreEqual(2, r.Major)
        Assert.AreEqual(10000000, r.SpeedScale)
        Assert.AreEqual(16, r.EpochBits)
        Assert.AreEqual(25, r.LatitudeBits)
        Assert.AreEqual(16, r.RecordSizeBits)
        Assert.AreEqual(18, r.StationBits)
        Assert.AreEqual(27, r.DateBits)
        Assert.AreEqual(1000, r.LevelAddScale)
        Assert.AreEqual(61, r.DatumTypes)
        Assert.AreEqual(70, r.LegaleseSize)
        Assert.AreEqual(175, r.Constituents)
        Assert.AreEqual(1930846, r.EndOfFile)

        let hdr = results.Value
        Assert.AreEqual(r.Constituents, hdr.Constituents.Length)
        Assert.AreEqual(r.Countries, hdr.Countries.Length)
        let c = List.rev hdr.Countries |> List.head
        let cs = c.ToCharArray()
        Assert.AreEqual(6, cs.Length)
        Assert.AreEqual("F.S.M.", c)


    [<Test>]
    member x.CanReadHarmonicsFile() = 
        let results = HarmonicsFileParser.Index() 
        Assert.AreEqual(true, results.IsSome)
        Assert.Fail() 


    [<Test>] 
    member x.CanCompareStringFromBin() = 
        let bytes = [| 0x46uy ; 0x2Euy; 0x53uy; 0x2Euy; 0x4Duy; 0x2Euy; 0x00uy; 0x00uy |]
        let s = System.Text.Encoding.ASCII.GetString(bytes).TrimEnd(char 0)
        Assert.AreEqual("F.S.M.", s)

    [<Test>]
    member x.CanReadRecord() = 
        let offset = 346098L
        let filename = "Resources/harmonics-dwf-20141224-free.tcd"
        use fileStream = File.OpenRead(filename) 
        use binaryReader = new BinaryReader(fileStream)
        let results = HarmonicsFileParser.ReadRecord offset binaryReader
        Assert.Fail()