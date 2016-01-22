namespace TideMonkey

open System

module Assert = 
    let IsTrue (fn : unit -> bool) = 
        match fn() with 
        | true -> ignore()
        | false -> raise (new Exception("AssertionFailed"))

    let LogException x = 
        printf "%A" x
        raise x

 

