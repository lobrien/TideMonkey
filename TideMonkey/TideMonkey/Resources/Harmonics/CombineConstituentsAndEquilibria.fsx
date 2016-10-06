#r "System.Xml"
#r "System.Xml.Linq"

open System.Xml.Linq

let xn s = XName.Get(s)

let eqDoc = XDocument.Load("/Users/larryobrien/Documents/src/xamarin_mine/TideMonkey/TideMonkey/TideMonkey/Resources/Harmonics/equilibria.xml")
let conDoc = XDocument.Load("/Users/larryobrien/Documents/src/xamarin_mine/TideMonkey/TideMonkey/TideMonkey/Resources/Harmonics/constituents.xml")

let eqEls = eqDoc.Root.Elements(xn "equilibrium")

let conEls = conDoc.Root.Elements(xn "Table")

let indexFor (eq : XElement) = eq.Element(xn "constituent").Value |> int

let eqsByIndex = eqEls |> Seq.groupBy indexFor 

let printSeq seq1 = Seq.iter (printf "%A ") seq1; printfn ""

eqsByIndex |> printSeq

let ts = Seq.zip conEls eqsByIndex

let stripIndex t = 
 let con = fst t
 let (_ , els) = snd t
 (con, els)

let makeSubEl t  = 
 let (con : XElement, els) = t
 els |> Seq.iter (fun (el : XElement) -> con.Add(el))
 con

let addTypes (el : XElement) = 
 el.Name <- xn "Constituent" 
 el.Element(xn "speed").SetAttributeValue(xn "unit", "DegreesPerHour")
 el

ts 
|> Seq.map stripIndex
|> Seq.map makeSubEl
|> Seq.map addTypes
|> Seq.iter  (printfn "%A")

conDoc.Save("/Users/larryobrien/Documents/src/xamarin_mine/TideMonkey/TideMonkey/TideMonkey/Resources/Harmonics/ConstituentsAndEquilibria.xml")