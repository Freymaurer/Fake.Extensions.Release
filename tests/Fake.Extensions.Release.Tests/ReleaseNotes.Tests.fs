module ReleaseNotes.Tests

open Expecto
open Fake.Core
open Fake.Extensions.Release.ReleaseNotes.Aux

[<Tests>]
let tests_SemVer =
    let semVer_1 = SemVer.parse "5.23.7-alpha.01+b0216ab"
    testList "SemVer" [
        testCase "Update SemVer Major" <| fun _ ->
            let result = updateSemVer Major "NewHash00a29f" semVer_1 |> SemVerInfo.toSemVer
            Expect.equal result "6.0.0+NewHash00a29f" ""

        testCase "Update SemVer Minor" <| fun _ ->
            let result = updateSemVer Minor "NewHash00a29f" semVer_1 |> SemVerInfo.toSemVer
            Expect.equal result "5.24.0+NewHash00a29f" ""

        testCase "Update SemVer Patch" <| fun _ ->
            let result = updateSemVer Patch "NewHash00a29f" semVer_1 |> SemVerInfo.toSemVer
            Expect.equal result "5.23.8+NewHash00a29f" ""

        testCase "Update SemVer Pre" <| fun _ ->
            let result = updateSemVer (Pre "alpha.02") "NewHash00a29f" semVer_1 |> SemVerInfo.toSemVer
            Expect.equal result "5.23.7-alpha.02+NewHash00a29f" ""
        testCase "Update SemVer WIP" <| fun _ ->
            let result = updateSemVer WIP "NewHash00a29f" semVer_1 |> SemVerInfo.toSemVer
            Expect.equal result "5.23.7-alpha.01+NewHash00a29f" ""
    ]

[<Tests>]
let tests_SemVerArgs =
    testList "SemVerArgs" [
        testCase "Test SemVerArg :major" <| fun _ ->
            let result = ["semver:major"] |> matchSemVerArg 
            Expect.equal result Major ""
        testCase "Test SemVerArg :minor" <| fun _ ->
            let result = ["semver:minor"] |> matchSemVerArg 
            Expect.equal result Minor ""
        testCase "Test SemVerArg :patch" <| fun _ ->
            let result = ["semver:patch"] |> matchSemVerArg 
            Expect.equal result Patch ""
        testCase "Test SemVerArg :pre-alpha.02" <| fun _ ->
            let result = ["semver:pre-alpha.02"] |> matchSemVerArg 
            Expect.equal result (Pre "alpha.02") ""
    ]

[<Tests>]
let tests_ReleaseNotes_Extensions =
    testList "SemVerArgs" [
        testCase "ReleaseNotes.ReleaseNotes.initReleaseNotes" <| fun _ ->
            let newNotes = ReleaseNotes.ReleaseNotes.initReleaseNotes()
            Expect.equal (newNotes.SemVer.ToSemVer()) "0.0.0" "newNotes.SemVer.ToSemVer()"
    ]
