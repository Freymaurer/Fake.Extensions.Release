namespace ReleaseNotes.FAKE

open Fake.Core
open Fake.IO
open Fake.Api

/// Module used to draft a new GitHub release.
module Github =
    
    /// Zip all files in relative 'sourcePath' and places zip file 'fileName' in 'targetPath'
    let zipAssets(sourcePath:string, targetPath:string, fileName:string) =

        let assetPath = System.IO.Path.Combine(__SOURCE_DIRECTORY__,sourcePath)
        let assetDir = Fake.IO.DirectoryInfo.ofPath assetPath

        let files =
            let assetsPaths = Fake.IO.DirectoryInfo.getFiles assetDir
            assetsPaths |> Array.map (fun x -> x.FullName)

        let outputPath = Path.combine targetPath fileName

        Zip.zip assetDir.FullName outputPath files

    /// Draft Github release with latest release notes.
    /// 'owner' = Github repo owner 
    /// 'repoName' = Github repository name
    /// 'releaseBody' = text for draft body, will be shown above latest release notes
    /// 'assetPath' = path to .zip file added to Github draft
    ///
    /// Needs Github Token passed as 'token:my-github-token' or environmental variabe 'github_token'
    let draft(  
                owner:string,            
                repoName:string,
                releaseBody:string option,
                assetPath: string option,
                config:TargetParameter
                ) =

        let prevReleaseNotes = Fake.IO.File.read "RELEASE_NOTES.md"
        let takeLastOfReleaseNotes =
            let findInd =
                prevReleaseNotes
                |> Seq.indexed
                |> Seq.choose (fun (i,x) -> if x.StartsWith "###" then Some i else None)
            match Seq.length findInd with
            | 1 ->
                prevReleaseNotes
            | x when x >= 2 ->
                let indOfSecondLastRN = findInd|> Seq.item 1
                Seq.take (indOfSecondLastRN - 1) prevReleaseNotes
            | _ ->
                failwith "Previous RELEASE_NOTES.md not found or in wrong formatting"

        let bodyText =
            [
                if releaseBody.IsSome then releaseBody.Value
                yield! takeLastOfReleaseNotes
            ]

        let tokenOpt =
            config.Context.Arguments
            |> List.tryFind (fun x -> x.StartsWith "token:")

        let release = ReleaseNotes.load "RELEASE_NOTES.md"
        let semVer = (sprintf "v%i.%i.%i" release.SemVer.Major release.SemVer.Minor release.SemVer.Patch)

        let token =
            match Environment.environVarOrDefault "github_token" "", tokenOpt with
            | s, None when System.String.IsNullOrWhiteSpace s |> not -> s
            | s, Some token when System.String.IsNullOrWhiteSpace s |> not ->
                Trace.traceImportant "Environment variable for token and token argument found. Will proceed with token passed as argument 'token:my-github-token'"
                token.Replace("token:","")
            | _, Some token ->
                token.Replace("token:","")
            | _, None ->
                failwith "please set the github_token environment variable to a github personal access token with repro access or pass the github personal access token as argument as in 'token:my-github-token'."

        let files (assetPath:string) =
            let assetDir = Fake.IO.DirectoryInfo.ofPath assetPath
            /// This only accesses files and not folders. So in this case it will only access the .zip file created by "ZipAssets"
            let assetsPaths = Fake.IO.DirectoryInfo.getFiles assetDir
            assetsPaths |> Array.map (fun x -> x.FullName)

        let draft = 
            if assetPath.IsSome then

                GitHub.createClientWithToken token
                |> GitHub.draftNewRelease owner repoName semVer (release.SemVer.PreRelease <> None) bodyText
                |> GitHub.uploadFiles (files assetPath.Value)
                |> Async.RunSynchronously

            else 
                
                GitHub.createClientWithToken token
                |> GitHub.draftNewRelease owner repoName semVer (release.SemVer.PreRelease <> None) bodyText
                |> Async.RunSynchronously

        Trace.trace "Draft successfully created!"