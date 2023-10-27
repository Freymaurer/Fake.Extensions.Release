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
    ]

[<Tests>]
let tests_SemVerPreArgs =
    testList "SemVerPreArgs" [
        testCase "Test SemVerPreArg pre:alpha.02" <| fun _ ->
            let result = ["pre:alpha.02"] |> matchPreArg
            Expect.isSome result "Is not some"
            Expect.equal result.Value (Pre "alpha.02") "Has incorrect SemVerPrerelease tag"
    ]

[<Tests>]
let tests_ReleaseNotes_Extensions =
    testList "SemVerArgs" [
        testCase "ReleaseNotes.ReleaseNotes.initReleaseNotes" <| fun _ ->
            let newNotes = ReleaseNotes.ReleaseNotes.initReleaseNotes()
            Expect.equal (newNotes.SemVer.ToSemVer()) "0.0.0" "newNotes.SemVer.ToSemVer()"
    ]

[<Tests>]
let tests_filterOutUnimportantCommits =
    testList "filterOutUnimportantCommits" [
        let testCommits = [|
            [|"2b59f8423783574a47e3e6dc7a4bc6108e74aea8"; "2b59f84"; "Add description to Regex pattern";|]
            [|"968c7d53e6eb38cf67038a26f8e4403a84b11b3d"; "968c7d5"; "Add patterns to match PR merge commits & RN bumps";|]
            [|"626265c22ee0862a52ebf814d131dfd1137e22d3"; "626265c"; "Update README.md";|]
            [|"626265c22ee0862a52ebf814d131dfd1137e22d3"; "626265c"; "Update release notes";|]
            [|"626265c22ee0862a52ebf814d131dfd1137e22d3"; "626265c"; "Update release_notes";|]
            [|"626265c22ee0862a52ebf814d131dfd1137e22d3"; "626265c"; "Update release notes.md";|]
            [|"626265c22ee0862a52ebf814d131dfd1137e22d3"; "626265c"; "Bump release notes.md";|]
            [|"b4f4a6fc22c5e918a3d2f45e67fd9e20a12a6cfb"; "b4f4a6f"; "Add nuget package commit link removal.";|]
            [|"210575d5da41cd315d6bf0a38cc8b08b282c6cf7"; "210575d"; "Merge branch 'main' of https://github.com/Freymaurer/Fake.Extensions.Release";|]
            [|"210575d5da41cd315d6bf0a38cc8b08b282c6cf7"; "210575d"; "Merge Pull Request #31 of ... to ...";|]
            [|"458c87f0d1fb41ae999b156a713dfdfb2d9496fd"; "458c87f"; "Update RELEASE_NOTES.md"|]
            [|"210575d5da41cd315d6bf0a38cc8b08b282c6cf7"; "210575d"; "Merge PR #69 of ... to ...";|]
        |]
        let testCommitsExpected = [|
            [|"2b59f8423783574a47e3e6dc7a4bc6108e74aea8"; "2b59f84"; "Add description to Regex pattern";|]
            [|"968c7d53e6eb38cf67038a26f8e4403a84b11b3d"; "968c7d5"; "Add patterns to match PR merge commits & RN bumps";|]
            [|"626265c22ee0862a52ebf814d131dfd1137e22d3"; "626265c"; "Update README.md";|]
            [|"b4f4a6fc22c5e918a3d2f45e67fd9e20a12a6cfb"; "b4f4a6f"; "Add nuget package commit link removal.";|]
        |]
        testCase "Test filterOutUnimportantCommits" <| fun _ ->
            let result = filterOutUnimportantCommits testCommits
            Expect.sequenceEqual result testCommitsExpected ""
    ]