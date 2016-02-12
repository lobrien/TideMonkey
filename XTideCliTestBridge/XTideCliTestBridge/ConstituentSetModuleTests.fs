namespace XTideCliTestBridge
open System
open NUnit.Framework
open TideMonkey
open ConstituentSet

[<TestFixture>]
type ConstituentSetModuleTests() = 

   let MockConstituents() =
      let mock : ConstituentT = 
         {  Name = "?"
            Speed = Math.PI * 1.0<Radians/Seconds> 
            Amplitude = 
               { Value = 0.035999998450279236
                 Units = PredictionUnitsT.Feet }
            Phase = Math.PI / 2.0 * 1.0<Radians>
            FirstValidYear = 2015
            LastValidYear = 2019
            Args = 
               [ (2015, 0.<Radians>)
                 (2016, Math.PI/2. * 1.<Radians>)
                 (2017, Math.PI * 1.<Radians>)
                 (2018, Math.PI * 2.<Radians>)
                 (2019, Math.PI / 2. * 1.<Radians>) ] |> Map.ofList
            Nodes = 
               [ (2015, 0.82779997587203979)
                 (2016, 0.83429998159408569)
                 (2017, 0.86690002679824829)
                 (2018, 0.91759997606277466)
                 (2019, 0.97610002756118774) ]
               |> Map.ofList 
         } 

      let name = "?"
      let speedDegreesPerSecond = 180. * 1.0<Degrees/Seconds>
      let startYear = 2015
      let numberOfYears = 5
      let argsDegrees = 
         [ 0.<Degrees>;
           90.<Degrees>;
           180.<Degrees>;
           360.<Degrees>;
           90.<Degrees>;
         ] 
      let nodes = [
         0.82779997587203979;
         0.83429998159408569;
         0.86690002679824829;
         0.91759997606277466;
         0.97610002756118774
         ]

      let amplitude = { Value = 0.035999998450279236; Units = PredictionUnitsT.Feet }
      let phaseDegrees = 90.0<Degrees>

      let c = Constituent.CreateFromUntypedInputs name speedDegreesPerSecond startYear numberOfYears argsDegrees nodes amplitude phaseDegrees
      [ c; c; c; ]


   let MockPredictionValue() : PredictionValueT = { Amplitude = { Value = 1.; Units = PredictionUnitsT.Feet } }

   let MockAdjustments() = { 
      TimeAdd  = { Duration = 1.0<Seconds> }; 
      LevelAdd = { Amplitude = { Value = 1.; Units = PredictionUnitsT.Feet } };
      LevelMultiply = 1.01 
      }


   [<Test>]
   member x.CanCreate() =
      let constituents = MockConstituents()
      let datum = MockPredictionValue()
      let adjustments = MockAdjustments()

      try
         let cs = ConstituentSet.Create constituents datum adjustments
         Assert.IsNotNull(cs)
      with
      | x -> 
         System.Console.WriteLine(x.ToString())
         Assert.Fail(x.Message)


   [<Test>]
   member x.CanProperlyCalculateDerivative() = 
      //Insanely important test.
      let constituents = MockConstituents()
      let datum = MockPredictionValue()
      let adjustments = MockAdjustments()

      try
         let cs = ConstituentSet.Create constituents datum adjustments

         let sinceEpoch = { Duration = (60.<Seconds> * 60. * 12.) }
         let derivs = seq { 0 .. 1 .. 3 } |> Seq.map (cs.TideDerivative sinceEpoch) |> Array.ofSeq
         //Note: have not confirmed these values manually
         Assert.AreEqual(0., derivs.[0].Amplitude.Value, 0.01)
         Assert.AreEqual(-0.89, derivs.[1].Amplitude.Value, 0.01)
         Assert.AreEqual(-1.70, derivs.[2].Amplitude.Value, 0.01)
         Assert.AreEqual(5.51, derivs.[3].Amplitude.Value, 0.01)

      with
      | x -> 
         System.Console.WriteLine(x.ToString())
         Assert.Fail(x.Message)

   [<Test>]
   member x.CanBlendWeight() = 
      Assert.AreEqual(0.0, ConstituentSet.BlendWeight 2. 2)
      Assert.AreEqual(1.0, ConstituentSet.BlendWeight 2. 0)

      let rv = ConstituentSet.BlendWeight 0.5 2
      Assert.AreEqual(0.896, ConstituentSet.BlendWeight 0.5 0, 0.001)
      Assert.AreEqual(0.527, ConstituentSet.BlendWeight 0.5 1, 0.001)
      Assert.AreEqual(-1.406, ConstituentSet.BlendWeight 0.5 2, 0.001)
      try
         ConstituentSet.BlendWeight 0.5 2 |> ignore
         Assert.Fail("Should have thrown")
      with
      | _ -> Assert.IsTrue(true) //As expected, argument out of range
