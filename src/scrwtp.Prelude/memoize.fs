namespace scrwtp.Prelude

module Memoize =
    // memoize an 'a -> 'b function using a Dictionary as a cache.
    let memoize f = 
        let cache = System.Collections.Generic.Dictionary<_, _>()
        fun x -> 
            match cache.TryGetValue(x) with
            | true, value  -> value
            | false, _ -> 
                let res = f x
                cache.[x] <- res
                res
    
    // memoize an 'a -> 'b function using a WeakDictionary as a cache.    
    let memoizeWeak f = 
        let cache = WeakDictionary<_, _>()
        fun x -> 
            match cache.TryFind(x) with
            | Some value  -> value
            | None -> 
                let res = f x
                cache.Add(x, res)
                res
