namespace XTideCliTestBridge

open System
open NUnit.Framework
open TideMonkey
open System.IO
open System.Xml.Linq

[<TestFixture>]
type HarmonicsFileTests () = 

    [<Test>]
    member x.CanReadAliases() =
        let aliases = Harmonics.Aliases
        Assert.AreEqual(24, aliases |> List.ofSeq |> List.length)

    [<Test>]
    member x.CanReadDatasets() = 
        let ds = Harmonics.DataSets |> Seq.choose id
        Assert.AreEqual(4372, ds |> List.ofSeq |> List.length)


        let subordinates = ds |> Seq.filter(fun d -> d.StationIdContext.IsSome) 
        Assert.AreEqual(3084, subordinates |> List.ofSeq |> List.length)




    [<Test>]
    member x.CanParseADataSetElement() = 
        let ss = ["""<Table>
        <index>46141</index>
        <name>Peck Lake, ICWW, Florida</name>
        <station_id_context>NOS</station_id_context>
        <station_id>8722404</station_id>
        <lat>27.1133</lat>
        <lng>-80.145</lng>
        <timezone>:America/New_York</timezone>
        <country>U.S.A.</country>
        <units>feet</units>
        <comments>Data from web snapshot taken 2014-12-16
        </comments>
        <source>http://tidesandcurrents.noaa.gov/</source>
        <restriction>Public domain</restriction>
        <date_imported>2014-12-23T00:00:00-10:00</date_imported>
        <xfields>Credit:NOAA data processed by David Flater for XTide
        http://www.flaterco.com/xtide/</xfields>
        <ref_index>43291</ref_index>
        <min_time_add>PT2H10M</min_time_add>
        <min_level_multiply>1</min_level_multiply>
        <max_time_add>PT1H13M</max_time_add>
        <max_level_multiply>0.58</max_level_multiply>
        <original_name>Peck Lake, ICWW, FLORIDA, East Coast, Florida</original_name>
        <state>FL</state>
        </Table>
        """;
        """<Table>
    <index>43369</index>
    <name>St. Simons Lighthouse, St. Simons Island, Georgia</name>
    <station_id_context>NOS</station_id_context>
    <station_id>8677344</station_id>
    <lat>31.1333333333333</lat>
    <lng>-81.3966666666667</lng>
    <timezone>:America/New_York</timezone>
    <country>U.S.A.</country>
    <units>feet</units>
    <comments>Harmonic constants from web snapshot taken 2014-12-16
Datum from benchmark sheet, publication date 2011-09-30
</comments>
    <source>http://tidesandcurrents.noaa.gov/</source>
    <restriction>Public domain</restriction>
    <date_imported>2014-12-23T00:00:00-10:00</date_imported>
    <xfields>Credit:NOAA data processed by David Flater for XTide
 http://www.flaterco.com/xtide/</xfields>
    <meridian>PT0S</meridian>
    <datumkind>Mean Lower Low Water</datumkind>
    <datum>3.56299212598425</datum>
    <original_name>St.Simons Island, GA</original_name>
    <state>GA</state>
  </Table>""";
  """<Table>
    <index>43669</index>
    <name>Johnston Atoll, Pacific Ocean</name>
    <station_id_context>NOS</station_id_context>
    <station_id>1619000</station_id>
    <lat>16.7383333333333</lat>
    <lng>-169.53</lng>
    <timezone>:Pacific/Johnston</timezone>
    <country>U.S.A.</country>
    <units>feet</units>
    <comments>Harmonic constants from web snapshot taken 2014-12-16
Datum from benchmark sheet, publication date 2003-04-21
</comments>
    <source>http://tidesandcurrents.noaa.gov/</source>
    <restriction>Public domain</restriction>
    <date_imported>2014-12-23T00:00:00-10:00</date_imported>
    <xfields>Credit:NOAA data processed by David Flater for XTide
 http://www.flaterco.com/xtide/</xfields>
    <meridian>PT0S</meridian>
    <datumkind>Mean Lower Low Water</datumkind>
    <datum>1.05314960629921</datum>
    <original_name>Johnston Atoll, USA</original_name>
  </Table>""";
  """
  <Table>
    <index>46532</index>
    <name>Texas City, Turning Basin, Galveston Bay, Texas</name>
    <station_id_context>NOS</station_id_context>
    <station_id>TEC4513</station_id>
    <lat>29.3833</lat>
    <lng>-94.8833</lng>
    <timezone>:America/Chicago</timezone>
    <country>U.S.A.</country>
    <units>feet</units>
    <comments>Data from web snapshot taken 2014-12-16
</comments>
    <source>http://tidesandcurrents.noaa.gov/</source>
    <restriction>Public domain</restriction>
    <date_imported>2014-12-23T00:00:00-10:00</date_imported>
    <xfields>Credit:NOAA data processed by David Flater for XTide
 http://www.flaterco.com/xtide/</xfields>
    <ref_index>43712</ref_index>
    <min_time_add>PT41M</min_time_add>
    <min_level_multiply>1</min_level_multiply>
    <max_time_add>PT33M</max_time_add>
    <max_level_multiply>1</max_level_multiply>
    <original_name>Texas City, Turning Basin, Galveston Bay, Texas</original_name>
    <state>TX</state>
  </Table>
  """

        ]
        ss |> Seq.iter (fun s ->
            let xml = XElement.Parse(s)
            let d = Harmonics.DataSetFromTableElement xml
            Assert.IsNotNull(d)
            )