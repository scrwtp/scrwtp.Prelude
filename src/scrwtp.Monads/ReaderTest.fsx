#I @".\bin\Debug"
#r "scrwtp.Prelude"

#load "Reader.fs"

open scrwtp.Prelude
open scrwtp.Monads.ReaderMonad

type TestContext = { value: int; anotherValue: string }

type TestOutput = { v1: int; v2: string }

module Getters =
    let getValue : Reader<TestContext -> int> = Reader.ask (fun ctx -> ctx.value)
    let getAnotherValue : Reader<TestContext -> string> = Reader.ask  (fun ctx -> ctx.anotherValue)

let output = 
    reader {
        let! v1 = Getters.getValue
        let! v2 = Getters.getAnotherValue

        return { v1 = v1; v2 = v2 }
    }
    |> flip Reader.runReader { value = 42; anotherValue = "Doug" }

