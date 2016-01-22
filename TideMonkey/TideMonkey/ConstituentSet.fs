namespace TideMonkey

open System

type ConstituentSetT = 
   { Constituents : ConstituentT List
     Datum : PredictionValueT
     Amplitudes : PredictionValueT list
     Phases : float<Radians>
     MaxAmplitude : AmplitudeT
     MaxDt : AmplitudeT list
     CurrentYear : Year
     Epoch : DateTime
     NextEpoch : DateTime
     PreferredLengthUnits : PredictionUnitsT }

module ConstituentSet = 
   let MAX_DT = 2 //Maximum derivative supported by tideDerivative and related functions
   
   let Create (constituents : ConstituentT list) (datum : PredictionValueT) adjustments = 
      let currentYear = 2016
      let preferredLengthUnits = datum.Amplitude.Units
      Assert.IsTrue(fun () -> datum.Amplitude.Units = adjustments.LevelAdd.Amplitude.Units)
      let adjustedDatum = 
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
      
      //Nasty loop to figure maxdt and maxAmplitude
      let loop = 
         let numYears = constituents.Head.LastValidYear - constituents.Head.FirstValidYear + 1
         seq { 
            for deriv in 0..MAX_DT + 1 do
               for year in 0..numYears do
                  yield (deriv, year)
         } TODO
         --- BEGIN AGAIN HERE
      
      let loopBody = 
         fun (deriv, year) -> 
            let maxEls = 
               constituents |> List.map (fun (c : ConstituentT) -> 
                                  let node = c.Nodes.[year]
                                  let pow = Math.Pow(c.Speed, deriv)
                                  c.Amplitude * node * pow)
            ignore()
      
      loop |> Seq.iter loopBody
      let allAmps : List<List<AmplitudeT>> = 
         let loopYear = constituents.Head.FirstValidYear
         [ 0..constituents.Length ] |> List.map (fun _ -> 
                                          //Then, the inner list maps to the amplitudes in the constituents
                                          constituents |> List.map (fun constituent -> 
                                                             let nodeVal = constituent.Nodes.[loopYear]
                                                             let amp = constituent.Amplitude.Value * nodeVal
                                                             { Value = amp
                                                               Units = constituent.Amplitude.Units }))
      ignore()
   
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
    *)
   type ConstituentSetT with
      member this.TideDerivative (sinceEpoch : IntervalT) (deriv : float) = 
         (*
            let tempd = deriv * Math.PI / 2.0 * 1.0<Radians>
            let term (constituent : ConstituentT<Radians, _>) = 
                let inner = tempd * constituent.Speed * sinceEpoch.Duration + constituent.Phase
                constituent.Amplitude * cos(inner)

            terms |> List.sum |> PredictionValue
            *)
         raise (new NotImplementedException())