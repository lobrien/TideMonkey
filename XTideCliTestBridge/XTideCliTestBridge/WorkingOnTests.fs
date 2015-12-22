namespace XTideCliTestBridge
open System
open NUnit.Framework
open TideMonkey
open TideMonkey.Station

[<TestFixture>]
type WorkingOnTests() = 

    [<Test>]
    member x.ParseComponentFile() = 
        Assert.Fail()

