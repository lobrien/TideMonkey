namespace XTideCliTestBridge
open System
open NUnit.Framework
open TideMonkey

[<TestFixture>]
type ConstituentModuleTests() = 

    [<Test>]
    member x.CanBuildFromUntypedInputs() =
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
      let phaseRadians = 90.0<Degrees> |> Geometry.deg2rad

      let c = Constituent.CreateFromUntypedInputs name speedDegreesPerSecond startYear numberOfYears argsDegrees nodes amplitude phaseRadians

      Assert.AreEqual(mock.Name, c.Name)
      Assert.AreEqual(mock.Speed, c.Speed)
      Assert.AreEqual(mock.Amplitude, c.Amplitude)
      Assert.AreEqual(mock.Phase, c.Phase)
      Assert.AreEqual(mock.FirstValidYear, c.FirstValidYear)
      Assert.AreEqual(mock.LastValidYear, c.LastValidYear)
      (*
      Assert.AreEqual(mock.Args.Keys.Length, c.Args.Length)
      List.zip mock.Args c.Args |> List.iter (fun (m, v) -> Assert.AreEqual(m, v))
      Assert.AreEqual(mock.Nodes.Length, c.Nodes.Length)
      List.zip mock.Nodes c.Nodes |> List.iter (fun (m, v) -> Assert.AreEqual(m, v))
      *)

      Assert.AreEqual(mock, c)