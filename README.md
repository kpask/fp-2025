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
<Command> ::= "dump" <dumpable>
            | "create playlist" <playlistName>
            | "add song" <song> "to playlist" <playlistName>
            | "add playlist" <playlistName> "to playlist" <playlistName>
            | "show playlist" <playlistName>
            | "total duration of playlist" <playlistName>
            | "play playlist" <playlistName>

<playlistName> ::= <string>

<song> ::= <title> "by" <artist> <duration>
<playlist> ::= <song> | <song> <playlist>

<title> ::= <string>
<artist> ::= <string>
<duration> ::= <Integer>

<Integer> ::= [0-9]
<string> ::= "a"
<dumpable> ::= "examples"
```