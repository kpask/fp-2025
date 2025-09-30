# fp-2025

## Setup

### To get started, you first need to open the project using Visual Studio Code and having Docker Desktop
1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

### To Build & Test the Project, run the following commands
1. `stack build`
2. `stack test`

### BNF
```
<command> ::= "dump" <dumpable>
            | "create playlist" <playlistName>
            | "add song" <title> "by" <artist> <duration> "to playlist" <playlistName>
            | "add playlist" <nestedPlaylistName> "to playlist" <parentPlaylistName>
            | "show playlist" <playlistName>
            | "total duration of playlist" <playlistName>
            | "play playlist" <playlistName>

<playlistName> ::= <string>
<nestedPlaylistName> ::= <playlistName>
<parentPlaylistName> ::= <playlistName>

<title> ::= <string>
<artist> ::= <string>
<duration> ::= <integer>

<dumpable> ::= "examples"

```