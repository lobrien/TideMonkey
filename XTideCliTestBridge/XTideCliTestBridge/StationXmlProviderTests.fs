namespace XTideCliTestBridge
open System
open NUnit.Framework
open TideMonkey
open TideMonkey.StationXmlProvider

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
      //Note: No values (double check to see if this is constant across database -- radians / hour perhaps?)
      Assert.AreEqual(27.3416965, c.Speed)

   [<Test>]
   member x.CanLoadDataSets() = 
     let ds = dataSetsByIndex pathToDataSets
     Assert.IsFalse(Map.isEmpty ds)
     let d = Map.find 16144 ds
     Assert.AreEqual("Baltimore Harbor Approach (off Sandy Point), Maryland Current", d.Name)

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