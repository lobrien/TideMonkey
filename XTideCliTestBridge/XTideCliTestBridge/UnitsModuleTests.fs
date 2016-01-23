namespace XTideCliTestBridge
open System
open TideMonkey
open NUnit.Framework

[<TestFixture>]
type UnitsModuleTests() = 

   [<Test>]
   member x.KnowsHydraulicUnits() =
       let notHydraulic = [ PredictionUnitsT.Meters; PredictionUnitsT.Knots; PredictionUnitsT.Feet; ]
       let hydraulic = [ KnotsSquared ]
       let noGood = [ Zulu ]

       hydraulic |> List.iter (fun u -> Assert.IsTrue (Units.IsHydraulicCurrent u))
       notHydraulic |> List.iter (fun u -> Assert.IsFalse(Units.IsHydraulicCurrent u))

       noGood |> List.iter (fun u -> 
         try 
             Units.IsHydraulicCurrent u |> ignore
             Assert.Fail("Should have thrown")
         with 
         | _ -> Assert.IsTrue(true) 
         )

   [<Test>]
   member x.KnowsCurrentUnits() = 
      let notCurrent = [ PredictionUnitsT.Meters; PredictionUnitsT.Feet; ]
      let current = [ KnotsSquared; PredictionUnitsT.Knots ]
      let noGood = [ Zulu ]

      current |> List.iter (fun u -> Assert.IsTrue (Units.IsCurrent u))
      notCurrent |> List.iter (fun u -> Assert.IsFalse(Units.IsCurrent u))

      noGood |> List.iter (fun u -> 
      try 
          Units.IsCurrent u |> ignore
          Assert.Fail("Should have thrown")
      with 
      | _ -> Assert.IsTrue(true) 
      )

   [<Test>]
   member x.CanFlatten() = 
      let notFlattenable = [ PredictionUnitsT.Meters; PredictionUnitsT.Feet; PredictionUnitsT.Knots ]
      let flattenable = [ KnotsSquared;  ]
      let noGood = [ Zulu ]

      flattenable |> List.iter (fun u -> Assert.AreNotEqual(u, Units.Flatten u))
      notFlattenable |> List.iter (fun u -> Assert.AreEqual(u, Units.Flatten u))

      noGood |> List.iter (fun u -> 
      try 
          Units.Flatten u |> ignore
          Assert.Fail("Should have thrown")
      with 
      | _ -> Assert.IsTrue(true) 
      )