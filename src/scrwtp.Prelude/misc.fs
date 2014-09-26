namespace scrwtp.Prelude

module Misc = 

    /// Constant function. Returns the value of its first argument and drops the second.
    let konst x _ = x

    /// Returns a function f' that takes its first two arguments in the reverse order of f.
    let flip f a b =
        f b a

    /// Increments an integer.
    let inc = (+) 1
    
    /// Decrements an integer.
    let dec = (-) 1

    /// Applies f until pred holds.
    let until pred f x =
        let rec loop acc =
            if pred acc 
                then acc
                else loop (f acc)
        loop x

    /// Calls f n times, passing in the counter value.
    let times n f =
        until ((=) n) (inc >> fun x -> f x; x) 0 
        |> ignore<int>