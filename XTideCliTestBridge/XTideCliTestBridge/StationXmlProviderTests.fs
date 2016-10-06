namespace XTideCliTestBridge
open System
open NUnit.Framework
open TideMonkey
open TideMonkey.StationXmlProvider
open Units


[<TestFixture>]
type StationXmlProviderTests() = 
   let pathToHarmonicsFiles = "../../../../TideMonkey/TideMonkey/Resources/Harmonics/"
   let pathToAliases = pathToHarmonicsFiles + "aliases.xml"
   let pathToConstants = pathToHarmonicsFiles + "constants.xml"
   let pathToConstituents = pathToHarmonicsFiles + "constituents.xml"
   let pathToDataSets = pathToHarmonicsFiles + "data_sets.xml"
   let pathToDrops = pathToHarmonicsFiles + "drops.xml"
   let pathToRefs2014 = pathToHarmonicsFiles + "refs2014.xml"
   let pathToEquilibria = pathToHarmonicsFiles + "equilibria.xml"
   let pathToNodeFactors = pathToHarmonicsFiles + "node_factors.xml"

   let DPH2RPS degreesPerHour = degreesPerHour * (Math.PI * 2.0) / 360.0 / 3600.0




   [<Test>]
   member x.CanLoadAliases() =
      let aliases = aliasesByName pathToAliases
      Assert.IsFalse (Map.isEmpty aliases)
      let alias = aliases |> Map.find "OQ2-HORN"
      //Note: Actually, the aliases file has two entries for OQ2-HORN, one in format "G" other in "N".
      Assert.AreEqual("G", alias.Format)
      Assert.AreEqual("OQ2", alias.Alias)

   [<Test>]
   member x.CanLoadConstants() = 
     let constants = constantsByIndex pathToConstants
     Assert.IsFalse (Map.isEmpty constants)
     let constant = constants |> Map.find (16141, "NU2")
     Assert.AreEqual ( 43.0<Radians>, constant.Phase)
     //Note: Amplitude is not associated with units (not in XML)
     Assert.AreEqual ( 0.078, constant.AmplitudeValue)

   [<Test>]
   member x.CanLoadConstituents() = 
      let cs = constituentsByName pathToConstituents
      Assert.IsFalse(Map.isEmpty cs)
      let c = cs |> Map.find ("OQ2-HORN")
      Assert.AreEqual("Compound 1 0 0 0 0 0 0 0 1", c.Definition)
      //Note: c.Speed is Radians/Second while cs.Speed is Degrees/Hour
      Assert.IsTrue(Math.Abs(0.000132556285279447 - (float) c.Speed) <= 0.00001)

   [<Test>]
   member x.CanLoadDataSets() = 
     let ds = dataSetsByIndex pathToDataSets
     Assert.IsFalse(Map.isEmpty ds)
     let d = Map.find 16144 ds
     match d with
     | ReferenceStation t ->
        Assert.AreEqual("Baltimore Harbor Approach (off Sandy Point), Maryland Current", t.Common.Name)
     | _ -> Assert.Fail()

   [<Test>]
   member x.CanLoadDrops() = 
      let ds = dropsByName pathToDrops
      Assert.IsFalse(Map.isEmpty ds)
      Assert.AreEqual(("MA2", "G"), Map.find "MA2" ds)
      Assert.AreEqual(("MB2", "G"), Map.find "MB2" ds)

   [<Test>]
   member x.CanLoadRefs() = 
      let ps = phasesByStationId pathToRefs2014
      Assert.IsFalse(Map.isEmpty ps)
      Assert.AreEqual(170.6, Map.find "8731439" ps |> snd, 0.001)

   [<Test>]
   member x.CanLoadEquilibria() = 
      let es = equilibriaByConstituentIdAndYear pathToEquilibria
      Assert.IsFalse(Map.isEmpty es)
      Assert.AreEqual((4, 2016, 211.44), Map.find (4,2016) es)

   [<Test>]
   member x.CanLoadNodeFactors() = 
      let nfs = nodeFactorsByConstituentIdAndYear pathToNodeFactors
      Assert.IsFalse(Map.isEmpty nfs)
      Assert.AreEqual((4, 2016, 1.4048), Map.find (4,2016) nfs)

   [<Test>]
   member x.CanAssociateConstituentsAndEquilibria() = 
      let cx = constituentsByName pathToConstituents
      let eqs = equilibriaByConstituentIdAndYear pathToEquilibria
      let e4c = equilibriaForConstituents cx eqs
      Assert.IsNotNull(e4c)

   [<Test>]
   member x.CanBuildConstituent() = 
      let cx = constituentsByName pathToConstituents |> Map.find ("OQ2-HORN")
      let eqs = equilibriaByConstituentIdAndYear pathToEquilibria
      //Comes from Station / TideRecord .Amplitude field
      let amp = { Value = 20.0;
        Units = PredictionUnitsT.Feet }
      //Comes from TideRecord.epoch field (from year)
      let phase = 0.5<Radians> 
      //Comes from HamonicsFile.get_equilibriums
      let args = [| (2015, 0.5<Radians>) |] |> Map.ofSeq
      //Comes from HarmonicsFile.get_node_factors
      let nodes = [| (2015, 0.5) |] |> Map.ofSeq

      let ct = buildConstituentFrom cx eqs amp phase args nodes
      Assert.AreEqual(cx.Name, ct.Name)
      Assert.IsTrue( Math.Abs( (float) 0.000132556285279447<Radians/Seconds> - (float) ct.Speed) <= 0.0001) 
      Assert.AreEqual(2015,  ct.FirstValidYear)
      Assert.AreEqual(2019, ct.LastValidYear)
      Assert.AreEqual({ Value = 20.0; Units = PredictionUnitsT.Feet }, ct.Amplitude)
      Assert.AreEqual(0.5<Radians>, ct.Phase)
  