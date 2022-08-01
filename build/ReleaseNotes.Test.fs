module ReleaseNotes


// This file is only used to test functions in the library!

open Fake.Core
open System


module ReleaseNotes =
    
    /// This module contains helper functions. These are only visible to allow for detailed unit tests.
    module Aux =
        
        type SemVerInfo with
            member this.ToSemVer() =
                let build = if this.Build > 0I then ("." + this.Build.ToString("D")) else ""

                let pre = 
                    match this.PreRelease with
                    | Some preRelease -> ("-" + preRelease.Origin)
                    | None -> ""   
        
                let meta = 
                    match this.BuildMetaData with
                    | "" -> ""
                    | metaData -> "+" + metaData

                sprintf "%d.%d.%d%s%s%s" this.Major this.Minor this.Patch build pre meta

        module SemVerInfo =
            let toSemVer (semVer:SemVerInfo) = semVer.ToSemVer()

        type SemVerRelease =
        | Major
        | Minor
        | Patch
        | Pre of string
        | WIP

        type private ReleaseNotesDescriptors =
        | Additions
        | Deletions
        | Bugfixes
    
            /// | Additions -> "Additions:" | Deletions -> "Deletions:" | Bugfixes  -> "Bugfixes:"
            member this.toString =
                match this with
                | Additions -> "Additions:"
                | Deletions -> "Deletions:"
                | Bugfixes  -> "Bugfixes:"
    
            static member DescriptorList =
                [Additions.toString; Deletions.toString; Bugfixes.toString]

        let createDateString (dt:System.DateTime) = sprintf "%i-%i-%i" dt.Year dt.Month dt.Day

        type ReleaseNotes.ReleaseNotes with
            /// Writes ReleaseNotes to established format. includes DateTime, SemVer, and Notes.
            member this.ComposeNotes() =
                [
                    sprintf "### %s (Released %s)" (SemVerInfo.toSemVer this.SemVer) (this.Date |> Option.defaultValue System.DateTime.Now |> createDateString)
                    yield! this.Notes
                    ""
                ]

            static member initReleaseNotes() =
                ReleaseNotes.ReleaseNotes.New("0.0.0","0.0.0", Some System.DateTime.Now, [
                        "* Additions:"
                        "    * Initial set up for RELEASE_Notes.md"
                    ])

        let updateSemVer (semVerReleaseType:SemVerRelease) (newestCommitHash:string) (prevSemVer:SemVerInfo) =
            match semVerReleaseType with
            | Major ->
                { prevSemVer with
                    Major = prevSemVer.Major + 1u
                    Minor = 0u
                    Patch = 0u
                    PreRelease = None
                    Build = 0I
                    BuildMetaData = newestCommitHash.Trim('#') }
            | Minor ->
                { prevSemVer with
                    Minor = prevSemVer.Minor + 1u
                    Patch = 0u
                    PreRelease = None
                    Build = 0I
                    BuildMetaData = newestCommitHash.Trim('#') }
            | Patch ->
                { prevSemVer with
                    Patch = prevSemVer.Patch + 1u
                    PreRelease = None
                    Build = 0I
                    BuildMetaData = newestCommitHash.Trim('#') }
            | Pre preRelease -> 
                { prevSemVer with
                    PreRelease = PreRelease.TryParse preRelease
                    Build = 0I
                    BuildMetaData = newestCommitHash.Trim('#') }
            | WIP ->
                { prevSemVer with
                    BuildMetaData = newestCommitHash.Trim('#') }
    
        // This is later used to try and sort the commit messages to the three fields additions, bugs and deletions.
        let rec sortCommitsByKeyWords (all:string list) (additions:string list) (deletions:string list) (bugs:string list) =
            let bugKeyWords = [|"bug"; "problem"; "fix"|] |> Array.map String.toLower
            let deleteKeyWords = [|"delete"; "remove"; "cut"|] |> Array.map String.toLower
            let isHeadBugKeyWord (head:string) = Array.exists (fun (x: string) -> head.ToLower().Contains x) bugKeyWords
            let isHeadDeleteKeyWord (head:string) = Array.exists (fun (x: string)  -> head.ToLower().Contains x) deleteKeyWords
            match all with
            | head::rest when isHeadBugKeyWord head
                    -> sortCommitsByKeyWords rest additions deletions (head::bugs)
            | head::rest when isHeadDeleteKeyWord head
                    -> sortCommitsByKeyWords rest additions (head::deletions) bugs
            | head::rest -> sortCommitsByKeyWords rest (head::additions) deletions bugs
            | []
                -> additions, deletions, bugs
            |> fun (x,y,z) -> List.rev x, List.rev y, List.rev z  
    
        let splitPreviousReleaseNotes releaseNotes =
            let addOpt = releaseNotes |> List.tryFindIndex (fun x -> x = Additions.toString), Additions
            let deleteOpt = releaseNotes |> List.tryFindIndex (fun x -> x = Deletions.toString), Deletions
            let bugOpt = releaseNotes |> List.tryFindIndex (fun x -> x = Bugfixes.toString), Bugfixes
            let indList = [addOpt;deleteOpt;bugOpt] |> List.filter (fst >> Option.isSome)
            let addedDescriptors =
                releaseNotes
                |> List.mapi (fun i x ->
                    let descriptor = indList |> List.tryFindBack (fun (descInd,_) -> descInd.Value <= i && ReleaseNotesDescriptors.DescriptorList |> List.contains x |> not)
                    if descriptor.IsNone then None else Some (snd descriptor.Value,x)
                )
            let findCommitsByDescriptor descriptor (commitOptionList:(ReleaseNotesDescriptors*string) option list) =
                commitOptionList
                |> List.choose (fun x -> 
                    if x.IsSome && fst x.Value = descriptor then Some (snd x.Value) else None
                )
            let prevAdditions = findCommitsByDescriptor Additions addedDescriptors
            let prevDeletions = findCommitsByDescriptor Deletions addedDescriptors
            let prevBugs = findCommitsByDescriptor Bugfixes addedDescriptors
            prevAdditions, prevDeletions, prevBugs

        let matchSemVerArg (args0: string list) =
            let args = args0 |> List.map (fun x -> x.ToLower().Trim()) 
            let opt = args |> List.tryFind (fun x -> x.StartsWith "semver:")
            match opt with
            | Some "semver:major"->
                Trace.trace "Increase major for next release notes."
                Major
            | Some "semver:minor" ->
                Trace.trace "Increase minor for next release notes."
                Minor
            | Some "semver:patch" ->
                Trace.trace "Increase patch for next release notes."
                Patch
            | Some isPre when isPre.StartsWith "semver:pre-" -> 
                Pre <| isPre.Replace("semver:pre-", "")
            | Some x ->
                Trace.traceError (sprintf "Unrecognized argument: \"%s\". Default to \"semver:wip\"." x)
                WIP
            | None | Some "semver:wip" ->
                Trace.trace "Add new commits to current release."
                WIP
    
    open Aux
    
    /// Checks if RELEASE_NOTES.md exists and if not creates it.
    /// 'folder' is the path containing the README.md
    let ensure() =
        let releaseNotesPath = "RELEASE_NOTES.md"
        let isExisting = Fake.IO.File.exists releaseNotesPath
        if isExisting = false then
            let newReleaseNotes = ReleaseNotes.ReleaseNotes.initReleaseNotes().ComposeNotes()
            Fake.IO.File.create releaseNotesPath
            Fake.IO.File.write
                true
                releaseNotesPath
                newReleaseNotes
            Trace.traceImportant "RELEASE_Notes.md created"
        else
            Trace.trace "RELEASE_Notes.md found"

    /// Checks if Release_NOTES.md exists and if not creates it.
    /// Deprecated after 0.2.0
    [<Obsolete("Use Release.ensure() instead!")>]
    let exists() = ensure()

    /// Updates RELEASE_NOTES.md by accessing git commits.
    let update(owner:string, repoName:string, config:TargetParameter)=

        let nOfLastCommitsToCheck =
            let opt =
                config.Context.Arguments
                |> List.tryFind (fun x -> x.StartsWith "n:")
            if opt.IsSome then opt.Value.Replace("n:","") else "30"

        let lastReleaseNotes, prevReleaseNotes = 
            let all = Fake.IO.File.read "RELEASE_NOTES.md" |> ReleaseNotes.parseAll
            if all.Length = 0 then
                ReleaseNotes.ReleaseNotes.initReleaseNotes(), []
            else 
                all.Head, all.Tail

        Trace.tracefn "Found latest release notes (%A, from %A)" lastReleaseNotes.SemVer.Original lastReleaseNotes.Date

        let lastCommitHash = if lastReleaseNotes.SemVer.BuildMetaData <> "" then Some <| lastReleaseNotes.SemVer.BuildMetaData else None

        match lastCommitHash with
        | Some hash -> Trace.tracefn "Found last commit '%s'. %A." hash System.Environment.NewLine
        | None -> Trace.tracefn "No last commit found. Add the last (if existing) '%s' commits." nOfLastCommitsToCheck

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

        let writeNewReleaseNotes =

            let semVer = matchSemVerArg config.Context.Arguments
            let commitNoteArr = newGitCommits |> Array.ofList |> Array.map (fun x -> x.Split([|";"|], StringSplitOptions.None))
            let latestCommitHash =
                // try pick latest from commit array
                if Array.isEmpty commitNoteArr |> not then sprintf "#%s" commitNoteArr.[0].[1] 
                // if no latest try pick last used
                elif lastCommitHash.IsSome then lastCommitHash.Value 
                // if all fails then empty
                else ""
            let newSemVer = updateSemVer semVer latestCommitHash lastReleaseNotes.SemVer
            /// This will be used to directly create the release notes
            let formattedCommitNoteList =
                commitNoteArr
                // filter out unimportant commits
                |> Array.filter (fun (x: string []) ->
                    let (lineContains: string -> bool) = x.[2].ToLower().Contains
                    match lineContains with
                    | x when x "update release_notes.md" || x "update release notes" -> false
                    | _ -> true
                )
                |> Array.map (fun x ->
                    sprintf "[[#%s](https://github.com/%s/%s/commit/%s)] %s" x.[1] owner repoName x.[0] x.[2]
                )
                |> List.ofArray

            let additions, deletions, bugs =
                let additions, deletions, bugs = sortCommitsByKeyWords formattedCommitNoteList [] [] []
                if semVer <> WIP then   
                    additions, deletions, bugs
                else
                    let prevAdditions, prevDeletions, prevBugs =
                        splitPreviousReleaseNotes lastReleaseNotes.Notes
                    additions@prevAdditions,deletions@prevDeletions,bugs@prevBugs

            let newNotes =
                ReleaseNotes.ReleaseNotes.New(newSemVer.ToSemVer(),newSemVer.ToSemVer(),Some DateTime.Now, 
                    [
                        if List.isEmpty additions |> not then
                            "Additions:"; 
                            yield! additions; 
                        if List.isEmpty deletions |> not then
                            "Deletions:"
                            yield! deletions
                        if List.isEmpty bugs |> not then
                            "Bugfixes:"
                            yield! bugs
                    ]
                ).ComposeNotes()

            /// add previous notes to new updated notes
            let allUpdatedNotes = 
                prevReleaseNotes |> List.collect (fun x -> x.ComposeNotes())
                |> fun prev -> 
                    if semVer <> WIP then   
                        newNotes@(lastReleaseNotes.ComposeNotes())@prev
                    else
                        newNotes@prev
                // ReleaseNotes.parseAll trims "*" and " " from lines, thereby removes list md
                // Apply list style to release notes.
                |> List.map (fun line ->
                    match line with
                    | "Additions:" -> "* Additions:"
                    | "Deletions:" -> "* Deletions:"
                    | "Bugfixes:" -> "* Bugfixes:"
                    | header when line.StartsWith "##" -> header
                    | "" -> ""
                    | anyElse -> "    * " + anyElse
                )

            Fake.IO.File.write
                false
                "RELEASE_NOTES.md"
                allUpdatedNotes

        Trace.trace "Update RELEASE_NOTES.md done!"
        