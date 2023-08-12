#I "src/Fake.Extensions.Release/bin/Release/netstandard2.0"
#I "src/Fake.Extensions.Release/bin/Debug/netstandard2.0"
#r "Fake.Extensions.Release.dll"

#r "nuget: Fake.Core.Target"
#r "nuget: Fake.Core.ReleaseNotes"
#r "nuget: Fake.Tools.Git"


open Fake.Extensions.Release.ReleaseNotes.Aux
open Fake.Core


// ++++++++++++++++++++
// CopyPasta from files
// ++++++++++++++++++++

/// Updates RELEASE_NOTES.md by accessing git commits.
let update(owner : string, repoName : string, config : TargetParameter)=

    printfn "works1"

    let nOfLastCommitsToCheck =
        let opt =
            config.Context.Arguments
            |> List.tryFind (fun x -> x.StartsWith "n:")
        if opt.IsSome then opt.Value.Replace("n:","") else "30"
    printfn "works2"

    let lastReleaseNotes, prevReleaseNotes = 
        // must change to System.IO because Fake.IO ofc only works in Fake context -_-
        let all = Fake.IO.File.read "RELEASE_NOTES.md" |> ReleaseNotes.parseAll
        if all.Length = 0 then
            ReleaseNotes.ReleaseNotes.initReleaseNotes(), []
        else 
            all.Head, all.Tail

    Trace.tracefn "Found latest release notes (%A, from %A)" lastReleaseNotes.SemVer.Original lastReleaseNotes.Date

    let lastCommitHash = if lastReleaseNotes.SemVer.BuildMetaData <> "" then Some <| lastReleaseNotes.SemVer.BuildMetaData else None

    match lastCommitHash with
    | Some hash -> Trace.tracef "Found last commit '%s'.%A" hash System.Environment.NewLine
    | None -> Trace.tracef "No last commit found. Add the last (if existing) '%s' commits." nOfLastCommitsToCheck

    //https://git-scm.com/book/en/v2/Git-Basics-Viewing-the-Commit-History#pretty_format
    let (_,gitCommits,_) = Fake.Tools.Git.CommandHelper.runGitCommand "" ("log -" + nOfLastCommitsToCheck + " --pretty=format:\"%H;%h;%s\"")

    /// returns all commits done after the one notes in "SemVer.BuildMetaData"
    let newGitCommits =
        if lastCommitHash.IsSome then
            let tryFindLastCommitIndex = gitCommits |> List.tryFindIndex (fun y -> y.Contains lastCommitHash.Value)
            if tryFindLastCommitIndex.IsNone then
                failwithf
                    "Could not find last version git hash: %s in the last %s commits.
                    You can increase the number of searched commits by passing a argument
                    as such \"dotnet fake build -t release n:50\""
                    lastReleaseNotes.SemVer.BuildMetaData nOfLastCommitsToCheck
            gitCommits
            |> List.take (tryFindLastCommitIndex.Value)
        else
            gitCommits

    newGitCommits

// ++++++++++++++++++++


let target : Target = {
    Name                = ""
    Dependencies        = []
    SoftDependencies    = []
    Description         = None
    Function            = (fun _ -> ())
}

let config = {
    TargetInfo  = target
    Context     = TargetContext.Create "" [] [] (new System.Threading.CancellationToken())
}

let project = "Fake.Extensions.Release"

let gitOwner = "Freymaurer"

let projectRepo = $"https://github.com/{gitOwner}/{project}"

update(gitOwner, projectRepo, config)


open System.Text.RegularExpressions

let filterOutUnimportantCommits commitNoteArr =
    printfn "%A" commitNoteArr
    // matches: "update(or)bump release_(or)-(or) notes(followed by anything)"
    //let releaseNotesPattern = Regex @"^(update|bump).+release[_\s-]?notes(.*)?$"
    let releaseNotesPattern = Regex @"^(update|bump).+release[_\s-]?notes"
    let mergePattern = Regex @"^merge.+(branch|pull request)"
    commitNoteArr
    |> Array.filter (fun (x: string []) ->
        let line = x.[2].ToLower()
        match line with
        | y when (releaseNotesPattern.Match y).Success || (mergePattern.Match y).Success -> false
        | _ -> true
    )

filterOutUnimportantCommits [|
    [|"lal"; "schmüh"; "gadi"|]
    [|"lal"; "schmüh"; "PULL REQUEST!"|]
    [|"lal"; "schmüh"; "MERGE PULL REQUEST!"|]
    [|"lal"; "schmüh"; "MERGE BRANCH     asd"|]
    [|"lal"; "schmüh"; "Update dein Gesicht!"|]
    [|"lal"; "schmüh"; "Update deine release notes"|]
    [|"lal"; "schmüh"; "Bump mommy's release notes"|]
    [|"lal"; "schmüh"; "Bump mommy's release_notes.md"|]
|]
