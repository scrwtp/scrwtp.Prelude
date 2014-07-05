namespace scrwtp.Prelude

/// A type representing one of two choices.
type Either<'a, 'b> =
    | Left of 'a
    | Right of 'b

module Either =
    /// True if it's the left choice, false otherwise.
    let isLeft  = function 
        | Left _ -> true  
        | Right _ -> false
    
    /// True if it's the right choice, false otherwise.
    let isRight = function 
        | Left _ -> false 
        | Right _ -> true
    
    /// Maps an Either value using one of two provided functions.
    let map lf rf = function 
        | Left value -> Left (lf value) 
        | Right value -> Right (rf value)

    /// Partitions a list of Eithers into two lists containing all the left and the right choices respectively.
    let partition lst = 
        let lefts, rights = 
            List.fold (fun (lefts, rights) either -> 
                match either with
                | Left value -> (value :: lefts, rights)
                | Right value -> (lefts, value :: rights)) ([],[]) lst
        List.rev lefts, List.rev rights