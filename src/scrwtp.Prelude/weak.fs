namespace scrwtp.Prelude

open System

/// A simple typed wrapper over System.WeakReference.
type WeakRef<'a> (target: 'a, ?trackResurrection: bool) =
    let track = defaultArg trackResurrection false

    let reference = WeakReference(target, track)

    member this.IsAlive = reference.IsAlive
    member this.TrackResurrection = track

    member this.AsOption : 'a option = 
        if reference.IsAlive
            then Some <| (reference.Target :?> 'a)
            else None
            
/// A dictionary that stores values through weak references, thus allowing them to be GC'ed.
type WeakDictionary<'k, 'v when 'k : equality>(capacity: int, comparer) = 
    let dictionary = System.Collections.Generic.Dictionary<'k, WeakRef<'v>>(capacity, comparer)

    new () = WeakDictionary(0, HashIdentity.Structural)

    /// Tries to find a value by the key, returns Some value if an element is found,
    /// None if not or the reference is no longer alive.
    member this.TryFind(key: 'k) : 'v option = 
        lock dictionary <| fun () ->
            match dictionary.TryGetValue(key) with
            | true, ref -> 
                let res = ref.AsOption
                if res.IsNone then
                    dictionary.Remove(key) |> ignore
                res                
            | false, _ -> None

    /// Adds a value under a given key, will override existing element on collision.
    member this.Add(key: 'k, value: 'v) = 
        lock dictionary <| fun () ->
            dictionary.[key] <- WeakRef(value)

    /// Returns the contents of the dictionary as a sequence. 
    /// Will only access a weak reference once the sequence is enumerated, meaning the target might already be gone at that point.
    member this.AsSeq = 
        dictionary
        |> Seq.choose (fun kvp ->
            match kvp.Value.AsOption with
            | Some value -> Some (kvp.Key, value)
            | None -> None)

module WeakDict = 
    /// Tries to find a value by the key, returns Some value if an element is found,
    /// None if not or the reference is no longer alive.
    let tryFind key (dict: WeakDictionary<_,_>) = 
        dict.TryFind(key)

    /// Adds a value under a given key, will override existing element on collision.
    let add key value (dict: WeakDictionary<_,_>) = 
        dict.Add(key,value)
    
    /// Returns the contents of the dictionary as a sequence. 
    /// Will only access a weak reference once the sequence is enumerated, meaning the target might already be gone at that point.
    let toSeq (dict: WeakDictionary<_,_>) = 
        dict.AsSeq

    /// Returns a WeakDictionary created from a sequence of key-value tuples.
    let fromSeq (coll: #seq<'k * 'v>) = 
        let dict = WeakDictionary<_,_>()
        coll |> Seq.iter dict.Add
        dict          
