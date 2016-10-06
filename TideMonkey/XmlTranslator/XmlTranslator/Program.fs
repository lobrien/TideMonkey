open System.Xml.Linq


let pathToEqFile = "/Users/larryobrien/Documents/src/xamarin_mine/TideMonkey/TideMonkey/TideMonkey/Resources/Harmonics/equilibria.xml"
let pathToConFile = "/Users/larryobrien/Documents/src/xamarin_mine/TideMonkey/TideMonkey/TideMonkey/Resources/Harmonics/constitutents.xml"

let xn s = XName.Get(s)

let eqDoc = XDocument.Load(pathToEqFile)
let conDoc = XDocument.Load(pathToConFile)

let eqEls = eqDoc.Root.Elements(xn "equilibrium")

let conEls = conDoc.Root.Elements(xn "Table")

let indexFor (eq : XElement) = eq.Element(xn "constituent").Value |> int

let eqsByIndex = eqEls |> Seq.groupBy indexFor 

let printSeq seq1 = Seq.iter (printf "%A ") seq1; printfn ""



[<EntryPoint>]
let main argv = 
   //eqEls |> printSeq
   printfn "Starting"
   //eqEls |> List.ofSeq |> List.length |> printfn "%d"
   0 // return an integer exit code

