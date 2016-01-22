namespace TideMonkey

open System
open SkyCal

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
