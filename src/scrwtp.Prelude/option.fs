namespace scrwtp.Prelude

module Option =

    /// Returns the value of an option.
    /// If the option is None, uses a provided default value.
    let defaultTo def value = 
        match value with
        | Some v -> v
        | None   -> def

    /// Returns the value of an option.
    /// If the option is None, uses the result of provided function f.
    let defaultToFunc f value = 
        match value with
        | Some v -> v
        | None   -> f ()

    /// Returns the value of an option.
    /// If the option is None, uses the value of a provided Lazy object.
    let defaultToLazy (lz: Lazy<_>) value = 
        match value with
        | Some v -> v 
        | None   -> lz.Value

    /// If a is Some, returns a, else returns b.
    /// Be careful when pipelining (order of the arguments).
    let combine a b = 
        match a with
        | Some _ -> a
        | None   -> b

    /// Reduces a list of options using combine.
    let combineAll opts =
        List.reduce combine opts        