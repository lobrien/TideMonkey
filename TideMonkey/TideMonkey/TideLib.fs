namespace TideMonkey

open System
open SkyCal

type PredictionValueT = { Amplitude : AmplitudeT }

type PredictionT = { Magnitude : float ; Direction : DirectionT ; Units : PredictionUnitsT}

type SimpleOffsetsT = { TimeAdd : IntervalT; LevelAdd : PredictionValueT; LevelMultiply : float }

type ConstituentSetT<[<Measure>] 'speedT, [<Measure>] 'amplitudeT> = { 
    Constituents : ConstituentT<'speedT, 'amplitudeT> List; 
    Datum : PredictionValueT; 
    Amplitudes : PredictionValueT list;
    Phases : float<Radians>;
    MaxAmplitude : AmplitudeT; 
    MaxDt : AmplitudeT list;
    CurrentYear : Year;
    Epoch : DateTime;
    NextEpoch : DateTime;
    PreferredLengthUnits : PredictionUnitsT;

    }

type SunMoonEventsT = { Sunrises : DateTime list; Sunsets : DateTime list; Moonrises : DateTime list; Moonsets : DateTime list; MoonPhases : (DateTime * MoonPhaseT) list }

type MetaField = { Name : string; Value : string }

type TideEventType = 
    | Max
    | Min
    | SlackRise
    | SlackFall
    | MarkRise
    | MarkFall
    | Sunrise
    | Sunset
    | Moon of MoonPhaseT
    | RawReading

type TideEventT = { Date : DateTime ; EventType : TideEventType; PredictionValue : PredictionValueT option } 


type StationT<[<Measure>] 'speedT, [<Measure>] 'amplitudeT, [<Measure>] 'predictionUnitsT> = { 
    Name : string ; 
    Coordinates : CoordinatesT; 
    Timezone : string; 
    MinCurrentBearing : CurrentBearingT option; //aka ebb direction
    MaxCurrentBearing : CurrentBearingT option; //aka flood direction
    Note : string option;
    IsCurrent : bool;
    Step : IntervalT option;
    //Protected
    StationRef : StationT<'speedT, 'amplitudeT, 'predictionUnitsT> option; //TODO: Maybe kill this -- it's a * to StationRef from station
    ConstituentSet : ConstituentSetT<'speedT, 'amplitudeT>; 
    Metadata : MetaField list option; //TODO: Maybe kill this -- it's the string data before conversion
    MinimumTimeOffset : IntervalT;
    MaximumTimeOffset : IntervalT;
}


module Station = 

    let LoadStations (pathToHarmonicsFile : string) = 
       //TODO: Actually do this 
       (* Some [  { Name = "Kailua Kona"; Coordinates = { Latitude = 19.65<Degrees>; Longitude = 155.9942<Degrees> } };  
               { Name = "Anchorage" ; Coordinates = { Latitude = 61.2167<Degrees>; Longitude = 149.9000<Degrees> } };  
               { Name = "Monterey" ; Coordinates = { Latitude = 36.6000<Degrees>; Longitude = 121.9000<Degrees> } }
            ]
        *)
        let konaMockConstituentSet = 
            let konaMockConstituents = 
                [
                    { 
                        Name = "?"
                        Speed = 0.00007556036137996953<RadiansPerSecond> ;
                        Amplitude = { Value = 0.035999998450279236; Units = Feet };
                        Phase = -4.2812927084053118<Radians>
                        FirstValidYear = 2015
                        LastValidYear = 2019
                        Args = 
                            [
                            (2015, 1.4552554823759725<Radians>);
                            (2016, 2.9045769092110301<Radians>);
                            (2017, 4.6134285774384356<Radians>);
                            (2018, 6.1042890588501679<Radians>);
                            (2019, 1.3425072286761053<Radians>);
                            ]
                        Nodes = 
                            [
                                (2015, 0.82779997587203979);
                                (2016, 0.83429998159408569);
                                (2017, 0.86690002679824829);
                                (2018, 0.91759997606277466);
                                (2019, 0.97610002756118774);
                            ] |> Map.ofList
                    }
                ]

            let datum = { Amplitude = {Value = 1.1449999809265137; Units = Feet } }

            let offsets = { 
                TimeAdd = { Duration = 2220.<Seconds> };
                LevelAdd = { Amplitude = { Value = 0.0;  Units = Feet } };
                LevelMultiply = 0.67
                }
            //TODO: I don't think this is even close to finished
            (*
            *)

            {
                Constituents = konaMockConstituents;
                Datum = datum;
                Amplitudes = [ { Amplitude = { Value = 1.0; Units = Feet } } ];
                Phases = 3.0<Radians>;
                MaxAmplitude = { Value = 2.0; Units = Feet };
                MaxDt = [ { Value = 1.0; Units = Feet } ]; 
                CurrentYear = 2016
                Epoch = new DateTime(1970, 1, 1); 
                NextEpoch = new DateTime(2525, 1, 1);
                PreferredLengthUnits = Feet;
            }

        Some [
            {
                Name = "Kailua Kona, Hawaii Island, Hawaii";
                Coordinates = { Latitude = 19.6433<Degrees>; Longitude = -156.0<Degrees> };
                Timezone = ":Pacific/Honolulu";
                MinCurrentBearing = Some { Degrees = 262.0<Degrees>; IsDegreesTrue = true } ;
                MaxCurrentBearing = Some { Degrees = 82.0<Degrees>; IsDegreesTrue = true };
                Note = None;
                IsCurrent = false; //Because units = 'feet'
                Step = Some { Duration = 3600.0<Seconds> } ;
                StationRef = None; //Should be Some to index 43373 -> Hilo
                ConstituentSet = konaMockConstituentSet; //TODO there are actually 23 constituents for Kona
                Metadata = None;
                MinimumTimeOffset = { Duration = 2220.0<Seconds> };
                MaximumTimeOffset = { Duration = 2280.0<Seconds> };

            };
            ]

    let Named name stations = 
        stations |> List.tryFind (fun s -> s.Name = name)

  
    type StationT<[<Measure>] 'speedT, [<Measure>] 'amplitudeT, [<Measure>] 'predictionUnitsT> with
        member this.AddSunMoonEvents (startTime : DateTime) (endTime : DateTime)  = 
            //Should return sunrise, sunsets, and next moon phase that is within range, so
            let rec dates (t : DateTime) = seq { 
                let s = endTime.Subtract(t).TotalSeconds
                match s > 0. with
                | true -> 
                    yield t 
                    yield! dates (t.AddDays(1.0))
                | false -> ignore()
            }

            let sunEvents = dates startTime |> Seq.map( fun d -> 
                let h0 = -0.8333<Degrees> //Standard for Sun 
                let ephemeris = Ephemeris.GreenwichEphemeris d "Solar"
                let results = Meeus.RisingAndSetting this.Coordinates ephemeris h0
                results
                )

            let moonEvents = dates startTime |> Seq.map (fun d -> 
                let h0 = 0.125<Degrees> //Mean for moon (approx. see Meeus Ch. 15) 
                let ephemeris = Ephemeris.GreenwichEphemeris d "Lunar"
                let results = Meeus.RisingAndSetting this.Coordinates ephemeris h0
                results
                )

            let moonPhasesInRange = 
                dates startTime 
                |> Seq.map (fun date -> Moon.NextMoonPhase date)
                |> Seq.distinct
                |> List.ofSeq

            let sunrises = sunEvents |> Seq.map (fun d -> d.Rising) |> List.ofSeq
            let sunsets = sunEvents |> Seq.map (fun d -> d.Setting) |> List.ofSeq
            let moonrises = moonEvents |> Seq.map (fun d -> d.Rising) |> List.ofSeq
            let moonsets = moonEvents |> Seq.map (fun d -> d.Setting) |> List.ofSeq


            { Sunrises = sunrises; Sunsets = sunsets; Moonrises = moonrises; Moonsets = moonsets; MoonPhases = moonPhasesInRange }


        member this.Predict time = 
            { Magnitude = 0.0; Direction = Rising ; Units = Feet }

        
        

module ConstituentSet = 
    let MAX_DT = 2 //Maximum derivative supported by tideDerivative and related functions

    let Create (constituents : ConstituentT<'u,'v> list) datum adjustments =
        let currentYear = 2016
        let preferredLengthUnits = datum.Units
        Assert.IsTrue(fun () -> datum.Units = adjustments.LevelAdd.Amplitude.Units)
        let adjustedDatum = 
            {
                Direction = datum.Direction;
                Magnitude = datum.Magnitude * adjustments.LevelMultiply + adjustments.LevelAdd.Amplitude.Value;
                Units = datum.Units;
            }
        let adjConstituents = 
            constituents
            |> Seq.map (fun constituent ->
                    { 
                        Name = constituent.Name;
                        Speed = constituent.Speed;
                        FirstValidYear = constituent.FirstValidYear;
                        LastValidYear = constituent.LastValidYear;
                        Args = constituent.Args;
                        Nodes = constituent.Nodes;

                        Amplitude = 
                            { 
                            Value = constituent.Amplitude.Value * adjustments.LevelMultiply;
                            Units = constituent.Amplitude.Units
                            }
                        //To move tides one hour later, you need to turn BACK the phases
                        Phase = constituent.Phase - adjustments.TimeAdd.Duration * constituent.Speed;

                    })
        //Nasty loop to figure maxdt and maxAmplitude
        let loop = 
            let numYears = constituents.Head.LastValidYear - constituents.Head.FirstValidYear + 1;
            seq {
            for deriv in 0 .. MAX_DT + 1 do
                for year in 0 .. numYears do
                    yield (deriv, year)
            }
            (* TODO --- BEGIN AGAIN HERE
        let loopBody = fun (deriv, year) -> 
            let maxEls = 
                constituents
                |> List.map (fun (c : ConstituentT) -> 
                    let node = c.Nodes.[year]
                    let pow = Math.Pow(c.Speed, deriv)
                    c.Amplitude * node * pow
                    )
            ignore()

        loop |> Seq.iter loopBody
        *)

        let allAmps : List<List<AmplitudeT>> = 
            let loopYear = constituents.Head.FirstValidYear
            [ 0 .. constituents.Length ] |> List.map ( fun _ ->
                //Then, the inner list maps to the amplitudes in the constituents
                constituents |> List.map ( fun constituent -> 
                    let nodeVal = constituent.Nodes.[loopYear]
                    let amp = constituent.Amplitude.Value * nodeVal
                    { Value = amp; Units = constituent.Amplitude.Units }
                )
            )

        

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
    type ConstituentSetT<'speedT,'amplitudeT> with
        member this.TideDerivative (sinceEpoch : IntervalT) (deriv : float)  = 
            (*
            let tempd = deriv * Math.PI / 2.0 * 1.0<Radians>
            let term (constituent : ConstituentT<Radians, _>) = 
                let inner = tempd * constituent.Speed * sinceEpoch.Duration + constituent.Phase
                constituent.Amplitude * cos(inner)

            terms |> List.sum |> PredictionValue
            *)
            raise (new NotImplementedException())

   
         