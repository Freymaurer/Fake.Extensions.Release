namespace Fake.Extensions.Release

open Fake.Core

module AssemblyVersion =
    
    /// Creates AssemblyVersion.fs for current release.
    let create(repoName:string) =
        
        let releaseNotes = ReleaseNotes.load "RELEASE_NOTES.md" 

        let releaseDate = if releaseNotes.Date.IsSome then releaseNotes.Date.Value.ToShortDateString() else "WIP"

        Trace.trace "Creating AssemblyVersion.fs ..."

        Fake.DotNet.AssemblyInfoFile.createFSharp  "AssemblyVersion.fs"
            [   Fake.DotNet.AssemblyInfo.Title repoName
                Fake.DotNet.AssemblyInfo.Version releaseNotes.AssemblyVersion
                Fake.DotNet.AssemblyInfo.Metadata ("ReleaseDate",releaseDate)
            ]

        Trace.trace "Creating AssemblyVersion.fs done!"
