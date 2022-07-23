module Fake.Extensions.Release.Tests
open Expecto
open ReleaseNotes.Tests

let all = testList "all" [
    tests_SemVer
    tests_SemVerArgs
    tests_ReleaseNotes_Extensions
]

[<EntryPoint>]
let main argv =
    Expecto.Tests.runTestsWithCLIArgs [] [||] all
