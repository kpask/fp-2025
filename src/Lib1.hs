module Lib1
    ( examples, Command(..), Dumpable(..)
    ) where

-- Optional type synonyms for clarity
type PlaylistName = String
type Title        = String
type Artist       = String
type Duration     = Int

-- Dumpable target
data Dumpable = Examples
  deriving Show

-- DSL commands
data Command
  = Dump Dumpable
  | CreatePlaylist PlaylistName
  | AddSong Title Artist Duration PlaylistName
  | AddPlaylist PlaylistName PlaylistName      -- nestedPlaylist -> parentPlaylist
  | ShowPlaylist PlaylistName
  | TotalDuration PlaylistName
  | PlayPlaylist PlaylistName
  deriving Show

-- Example commands
example0 :: Command
example0 = CreatePlaylist "MyPlaylist"

example1 :: Command
example1 = AddSong "Yesterday" "The Beatles" 125 "MyPlaylist"

example2 :: Command
example2 = AddPlaylist "Favorites" "MyPlaylist"  -- nested inside parent

example3 :: Command
example3 = TotalDuration "Favorites"

example4 :: Command
example4 = Dump Examples  -- self-referential, required for Lab 1

-- List of examples (used by tests)
examples :: [Command]
examples = [example0, example1, example2, example3, example4]
