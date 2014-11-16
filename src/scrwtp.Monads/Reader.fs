namespace scrwtp.Monads

module ReaderMonad = 

    type Reader<'a> = R of 'a    
    
    module Reader = 
        let runReader (R read) env = read env

        let bind f (reader: Reader<_>) = 
            Reader<_>.R (fun env -> runReader (f (runReader reader env)) env)

        let ask f = Reader<_>.R(f)

    type ReaderBuilder () = 
        member this.Bind(reader, f) = Reader.bind f reader      
        member this.Return(v:'a) = Reader<_>.R(fun _ -> v)

    let reader = ReaderBuilder()