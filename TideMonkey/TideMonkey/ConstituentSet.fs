namespace TideMonkey

open System

type ConstituentSetT = 
   { Constituents : ConstituentT List
     Datum : PredictionValueT
     MaxAmplitude : AmplitudeT
     MaxDt : AmplitudeT list
     PreferredLengthUnits : PredictionUnitsT
     mutable CurrentYear : Year
     (*
     TODO: Mo' Functional, Mo' Betta: 
     All of the following could be made functions whose value is dependent on CurrentYear, based 
     on the equations in ConstituentSetT.ChangeYear()
     *)
     mutable Epoch : DateTime 
     mutable NextEpoch : DateTime
     mutable Amplitudes : PredictionValueT list //Could be a function based on CurrentYear (see ChangeYear)
     mutable Phases : float<Radians> list
      }

module ConstituentSet = 

   type ConstituentSetT with

       (* 

           From XTide: tideDerivative (Interval sinceEpoch, unsigned deriv)
        *
        * Calculate (deriv)th time derivative of the normalized tide for time
        * in s since the beginning (UTC) of currentYear, WITHOUT changing
        * years or blending.
        *
        * Note:  This function does not check for changes in year.  This is
        * important to our algorithm, since for times near new year's, we
        * interpolate between the tides calculated using one year's
        * coefficients and the next year's coefficients.

        * TODO: Confirm claim that "Xtide spends more time in this method than anywhere else."
       *)
      member this.TideDerivative (sinceEpoch : IntervalT) (deriv : int) = 
         let tempd = (float deriv) * Math.PI / 2.0 * 1.0<Radians>

         let term (amplitude : PredictionValueT, constituent : ConstituentT, phase : float<Radians>)  = 
            let inner = float tempd * float constituent.Speed * float sinceEpoch.Duration + float phase
            let initialTerm = amplitude.Amplitude.Value * cos(inner)
            let mutable term = initialTerm
            for b in deriv .. -1 .. 0 do
               term <- term * float constituent.Speed //Speed is defined in radians/sec, which is what is desired
            term

         let predictionValue = 
            List.zip3 this.Amplitudes this.Constituents this.Phases
            |> List.map term
            |> List.sum 
         { Amplitude = { Value = predictionValue; Units = this.PreferredLengthUnits } }

      
      // Update amplitudes, phases, epoch, nextEpoch, and currentYear.
      member this.ChangeYear (newYear : Year) = 
         this.CurrentYear <- newYear

         // Apply node factor.
         this.Amplitudes <- 
            this.Constituents 
            |> List.map (fun constituent ->
               { Amplitude = { Value = constituent.Amplitude.Value * constituent.Nodes.[this.CurrentYear];
                 Units = constituent.Amplitude.Units 
               }} )

         // Apply equilibrium argument.  Recall that phases have been pre-negated
         // per -k'.
         this.Phases <-
            this.Constituents 
            |> List.map (fun constituent -> 
               let argForYear = constituent.Args.[this.CurrentYear]
               let phase = constituent.Phase
               phase + argForYear 
               )
         this

   let MAX_DT = 2 //Maximum derivative supported by tideDerivative and related functions

   let Create (constituents : ConstituentT list) (datum : PredictionValueT) adjustments = 
      let currentYear = 2016
      let preferredLengthUnits = datum.Amplitude.Units
      AssertTM.IsTrue(fun () -> datum.Amplitude.Units = adjustments.LevelAdd.Amplitude.Units)
      let adjDatum = 
         { Amplitude = 
              { Value = datum.Amplitude.Value * adjustments.LevelMultiply + adjustments.LevelAdd.Amplitude.Value
                Units = datum.Amplitude.Units } }
      
      let adjConstituents = 
         constituents 
         |> Seq.map 
               (fun constituent -> 
               { Name = constituent.Name
                 Speed = constituent.Speed
                 FirstValidYear = constituent.FirstValidYear
                 LastValidYear = constituent.LastValidYear
                 Args = constituent.Args
                 Nodes = constituent.Nodes
                 Amplitude = 
                    { Value = constituent.Amplitude.Value * adjustments.LevelMultiply
                      Units = constituent.Amplitude.Units }
                 //To move tides one hour later, you need to turn BACK the phases
                 Phase = 
                    constituent.Phase 
                    - (1.0<Seconds> * float adjustments.TimeAdd.Duration) 
                      * (1.0<Radians/Seconds> * float constituent.Speed) })
         |> List.ofSeq
      
      //Nasty loop to figure maxdt and maxAmplitude
      let numYears = constituents.Head.LastValidYear - constituents.Head.FirstValidYear + 1
      let dtAmplitudeUnits = constituents.Head.Amplitude.Units
      //Assert( all constituents have the same units)

      let mutable maxAmplitudeV = 0.
      let mutable maxDts = 
         [ 0..MAX_DT + 1 ]
         |> List.map (fun _ -> 0.)
         |> Array.ofList
      for deriv in [ 0..MAX_DT + 1 ] do
         for tempYear in [ constituents.Head.FirstValidYear .. constituents.Head.LastValidYear ] do
            let max = 
               constituents
               |> List.map (fun (c : ConstituentT) -> 
                     let node = c.Nodes.[tempYear]
                     //c.Speed is radians per second
                     let pow = Math.Pow(float c.Speed, float deriv)
                     c.Amplitude.Value * node * pow)
               |> List.sum
            if max > maxDts.[deriv] then maxDts.[deriv] <- max
         if deriv = 0 then
            maxAmplitudeV <- maxDts.[deriv]
         maxDts.[deriv] <- maxDts.[deriv] * 1.1 //Add a little safety margin
      
      let maxDt = maxDts |> List.ofArray |> List.map (fun v -> { Value = v ; Units = dtAmplitudeUnits })

      let maxAmplitude = 
         match Units.IsHydraulicCurrent dtAmplitudeUnits with 
         | true ->  { Value = maxAmplitudeV; Units = Units.Flatten dtAmplitudeUnits } 
         | false -> { Value = maxAmplitudeV; Units = dtAmplitudeUnits }
      AssertTM.IsTrue(fun () -> maxAmplitude.Value > 0.)

      // Harmonics file range of years may exceed that of this platform.
      // Try valiantly to find a safe initial value.
      let currentYear : Year = 
         let b = constituents.Head.FirstValidYear
         let e = constituents.Head.LastValidYear
         match (b, e) with 
         | (b,e) when b < 2000 && e >= 2000 -> 2000
         | (b,e) when b <= 1970 && e >= 1970 -> 1970
         | (b,e) when b <= 2037 && e >= 2037 -> 2037
         | _ -> (b + e) / 2

           
      let cs = 
         {  Constituents = adjConstituents;
            Datum = adjDatum;
            MaxAmplitude = maxAmplitude;
            MaxDt = maxDt;
            PreferredLengthUnits = preferredLengthUnits;

            CurrentYear  = 0; //? Modified below
            Epoch = new DateTime();
            NextEpoch = new DateTime();
            Amplitudes = List.init constituents.Length (fun _ -> { Amplitude =  { Value = 0.; Units = preferredLengthUnits } });
            Phases = List.init constituents.Length (fun _ -> 0.<Radians>);
      } 
      cs.ChangeYear currentYear
   
   (*

      From XTide: 

      Returns the value (deriv)th derivative of the "blending function" w(x):
       *
       *   w(x) =  0,     for x <= -1
       *
       *   w(x) =  1/2 + (15/16) x - (5/8) x^3 + (3/16) x^5,
       *                  for  -1 < x < 1
       *
       *   w(x) =  1,     for x >= 1
       *
       * This function has the following desirable properties:
       *
       *    w(x) is exactly either 0 or 1 for |x| > 1
       *
       *    w(x), as well as its first two derivatives are continuous for all x.
   *)
   let BlendWeight x deriv = 
      let x2 = x * x

      match x2 > 1.0 with
      | true -> 
         match (deriv = 0 && x > 0.0) with
         | true -> 1.0
         | false -> 0.0
      | false ->
         match deriv with
         | 0 -> ((3.0 * x2 - 10.0) * x2 + 15.0) * x / 16.0 + 0.5
         | 1 -> ((x2 - 2.0) * x2 + 1.0) * (15.0/16.0)
         | 2 -> (x2 - 1.0) * x * (15.0/4.0)
         | _ -> raise <| new ArgumentOutOfRangeException("Expected deriv < 3; received " + deriv.ToString()) 

