namespace ReleaseNotes.FAKE

open Fake.Core
open System

module Release =
    
    type private SemVerRelease =
    | Major
    | Minor
    | Patch
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
    
    let private createCurrentDateString() =
        let n = System.DateTime.Now
        sprintf "%i-%i-%i" n.Year n.Month n.Day

    let private createNewSemVer (semVerReleaseType:SemVerRelease) (newestCommitHash:string) (previousSemVer:SemVerInfo)=
        match semVerReleaseType with
        | Major ->
            sprintf "%i.0.0+%s" (previousSemVer.Major+1u) newestCommitHash
        | Minor ->
            sprintf "%i.%i.0+%s" (previousSemVer.Major) (previousSemVer.Minor+1u) newestCommitHash
        | Patch ->
            sprintf "%i.%i.%i+%s" (previousSemVer.Major) (previousSemVer.Minor) (previousSemVer.Patch+1u) newestCommitHash
        | WIP ->
            sprintf "%i.%i.%i+%s" (previousSemVer.Major) (previousSemVer.Minor) (previousSemVer.Patch) newestCommitHash
    
    // This is later used to try and sort the commit messages to the three fields additions, bugs and deletions.
    let rec private sortCommitsByKeyWords (all:string list) (additions:string list) (deletions:string list) (bugs:string list) =
        let bugKeyWords = [|"bug"; "problem"; "fix"|] |> Array.map String.toLower
        let deleteKeyWords = [|"delete"; "remove"|] |> Array.map String.toLower
        let isHeadBugKeyWord (head:string) = Array.exists (fun x -> head.ToLower().Contains x) bugKeyWords
        let isHeadDeleteKeyWord (head:string) = Array.exists (fun x -> head.ToLower().Contains x) deleteKeyWords
        match all with
        | head::rest when isHeadBugKeyWord head
                -> sortCommitsByKeyWords rest additions deletions (head::bugs)
        | head::rest when isHeadDeleteKeyWord head
                -> sortCommitsByKeyWords rest additions (head::deletions) bugs
        | head::rest -> sortCommitsByKeyWords rest (head::additions) deletions bugs
        | head::[] when isHeadBugKeyWord head
            -> additions, deletions, (head::bugs)
        | head::[] when isHeadDeleteKeyWord head
            -> additions, (head::deletions), bugs
        | head::[]
            -> (head::additions), deletions, bugs
        | []
            -> additions, deletions, bugs
        |> fun (x,y,z) -> List.rev x, List.rev y, List.rev z  
    
    
    let private splitPreviousReleaseNotes releaseNotes =
        let addOpt = releaseNotes |> List.tryFindIndex (fun x -> x = Additions.toString)
        let deleteOpt = releaseNotes |> List.tryFindIndex (fun x -> x = Deletions.toString)
        let bugOpt = releaseNotes |> List.tryFindIndex (fun x -> x = Bugfixes.toString)
        let indList = [addOpt,Additions;deleteOpt,Deletions;bugOpt,Bugfixes] |> List.choose (fun (x,y) -> if x.IsSome then Some (x.Value, y) else None)
        let addedDescriptors =
            releaseNotes
            |> List.mapi (fun i x ->
                let descriptor = indList |> List.tryFindBack (fun (descInd,_) -> descInd <= i && ReleaseNotesDescriptors.DescriptorList |> List.contains x |> not)
                if descriptor.IsNone then None else Some (snd descriptor.Value,x)
            )
        let findCommitsByDescriptor descriptor (commitOptionList:(ReleaseNotesDescriptors*string) option list) =
            commitOptionList
            |> List.choose (fun x -> 
                if x.IsSome && fst x.Value = descriptor then Some (snd x.Value) else None
            )
            |> List.map (fun x -> sprintf "    * %s" x)
        let prevAdditions = 
            findCommitsByDescriptor Additions addedDescriptors
            // REMOVE this line as soon as parsing of semver metadata is fixed.
            |> List.filter (fun x -> x.StartsWith "    * latest commit #" |> not)
        let prevDeletions = findCommitsByDescriptor Deletions addedDescriptors
        let prevBugs = findCommitsByDescriptor Bugfixes addedDescriptors
        prevAdditions, prevDeletions, prevBugs

    let private checkSemVerTargetParam (config:TargetParameter)=
        let opt =
            config.Context.Arguments
            |> List.map (fun x -> x.ToLower())
            |> List.tryFind (fun x -> x.StartsWith "semver:")
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
        | Some "semver:wip" ->
            Trace.trace "Add new commits to current release."
            WIP
        | Some x ->
            Trace.traceError (sprintf "Unrecognized argument: \"%s\". Default to \"semver:wip\"." x)
            WIP
        | None ->
            Trace.trace "Add new commits to current release."
            WIP

    /// Checks if Release_NOTES.md exists and if not creates it.
    let exists() =
        let isExisting = Fake.IO.File.exists "RELEASE_NOTES.md"
        if isExisting = false then
            Fake.IO.File.create "RELEASE_NOTES.md"
            Fake.IO.File.write
                true
                "RELEASE_NOTES.md"
                [
                    sprintf "### 0.0.0 (Released %s)" (createCurrentDateString())
                    "* Additions:"
                    "    * Initial set up for RELEASE_Notes.md"
                ]
            Trace.traceImportant "RELEASE_Notes.md created"
        else
            Trace.trace "RELEASE_Notes.md found"


    /// Updates RELEASE_NOTES.md by accessing git commits.
    let update(config:TargetParameter)=

        let nOfLastCommitsToCheck =
            let opt =
                config.Context.Arguments
                |> List.tryFind (fun x -> x.StartsWith "n:")
            if opt.IsSome then opt.Value.Replace("n:","") else "30"

        let prevReleaseNotes =
            Fake.IO.File.read "RELEASE_NOTES.md"

        let release = ReleaseNotes.load "RELEASE_NOTES.md"

        // REMOVE this line as soon as parsing of semver metadata is fixed.
        // This should be in release.SemVer.MetaData
        let (tryFindPreviousReleaseCommitHash: string option) =
            release.Notes
            |> List.tryFind (fun x -> x.TrimStart([|' ';'*'|]).StartsWith "latest commit #")
            |> fun x ->
                if x.IsSome then x.Value.Replace("latest commit ","") |> Some else None

        if tryFindPreviousReleaseCommitHash.IsSome then
            Trace.trace (sprintf "Found PreviousCommit: %s" tryFindPreviousReleaseCommitHash.Value)
        else
            Trace.traceError "Did not find previous Commit!"

        //https://git-scm.com/book/en/v2/Git-Basics-Viewing-the-Commit-History#pretty_format
        let allGitCommits =
            Fake.Tools.Git.CommandHelper.runGitCommand "" ("log -" + nOfLastCommitsToCheck + " --pretty=format:\"%H;%h;%s\"" )

        let cutCommitsAtPreviousReleaseCommit =
            allGitCommits
            |> fun (_,gitCommits,_) ->
                if tryFindPreviousReleaseCommitHash.IsSome then
                    let indOpt =
                        gitCommits |> List.tryFindIndex (fun y -> y.Contains tryFindPreviousReleaseCommitHash.Value.[1..])
                    let ind =
                        if indOpt.IsSome then
                            indOpt.Value
                        else
                            failwithf
                                "Could not find last version git hash: %s in the last %s commits.
                                You can increase the number of searched commits by passing a argument
                                as such \"dotnet fake build -t release n:50\""
                                tryFindPreviousReleaseCommitHash.Value nOfLastCommitsToCheck
                    gitCommits
                    |> List.take (ind)
                else
                    gitCommits

        Trace.trace "Updating RELEASE_NOTES.md ..."

        let writeNewReleaseNotes =

            let semVer = checkSemVerTargetParam config
            let currentDateString = createCurrentDateString()
            let commitNoteArr = cutCommitsAtPreviousReleaseCommit |> Array.ofList |> Array.map (fun x -> x.Split([|";"|],StringSplitOptions.None))
            // REMOVE this line as soon as parsing of semver metadata is fixed.
            // This should be in release.SemVer.MetaData
            let latestCommitHash =
                let newCommit = if tryFindPreviousReleaseCommitHash.IsSome then tryFindPreviousReleaseCommitHash.Value else ""
                if Array.isEmpty commitNoteArr then newCommit else sprintf "#%s" commitNoteArr.[0].[1]
            let newSemVer =
                createNewSemVer semVer latestCommitHash.[1..] release.SemVer
            /// This will be used to directly create the release notes
            let formattedCommitNoteList =
                commitNoteArr
                |> Array.filter (fun x ->
                    match x.[2].ToLower().Contains with
                    | x when x "update release_notes.md" || x "update release notes" ->
                        false
                    | _ -> true
                )
                |> Array.map (fun x ->
                    sprintf "    * [[#%s](https://github.com/nfdi4plants/Swate/commit/%s)] %s" x.[1] x.[0] x.[2]
                )
                |> List.ofArray
            let additions, deletions, bugs = sortCommitsByKeyWords formattedCommitNoteList [] [] []

            let newNotes =
                if semVer <> WIP then
                    [
                        sprintf "### %s (Released %s)" newSemVer currentDateString
                        // Additions will not need to be checked, as in the current version the latest commit hash needs to be th first entry here.
                        "* Additions:"
                        // REMOVE this line as soon as parsing of semver metadata is fixed.
                        sprintf "    * latest commit %s" latestCommitHash
                        yield! additions
                        if List.isEmpty deletions |> not then
                            "* Deletions:"
                            yield! deletions
                        if List.isEmpty bugs |> not then
                            "* Bugfixes:"
                            yield! bugs
                        ""
                        yield! prevReleaseNotes
                    ]
                else
                    let prevAdditions, prevDeletions, prevBugs =
                        splitPreviousReleaseNotes release.Notes
                    let appendAdditions, appendDeletions, appendBugfixes =
                        additions@prevAdditions,deletions@prevDeletions,bugs@prevBugs
                    let skipPrevVersionOfReleaseNotes =
                        let findInd =
                            prevReleaseNotes
                            |> Seq.indexed
                            |> Seq.choose (fun (i,x) -> if x.StartsWith "###" then Some i else None)
                            |> Seq.skip 1
                        if Seq.isEmpty findInd then 0 else Seq.head findInd
                    [
                        sprintf "### %s (Released %s)" newSemVer currentDateString
                        if List.isEmpty appendAdditions |> not then
                            "* Additions:"
                            // REMOVE this line as soon as parsing of semver metadata is fixed.
                            sprintf "    * latest commit %s" latestCommitHash
                            yield! appendAdditions
                        if List.isEmpty appendDeletions |> not then
                            "* Deletions:"
                            yield! appendDeletions
                        if List.isEmpty appendBugfixes |> not then
                            "* Bugfixes:"
                            yield! appendBugfixes
                        ""
                        yield! (Seq.skip skipPrevVersionOfReleaseNotes prevReleaseNotes)
                    ]

            Fake.IO.File.write
                false
                "RELEASE_NOTES.md"
                newNotes

        Trace.trace "Update RELEASE_NOTES.md done!"
        