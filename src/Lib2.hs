{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lib2(
    parseCommand
    , ToCliCommand(..)
    , process) where

import qualified Lib1

type ErrorMsg = String
type Parser a = String -> Either ErrorMsg (a, String)

parseCommand :: Parser Lib1.Command
parseCommand input =
    parseDump input
    `orElse` parseCreatePlaylist input
    `orElse` parseAddSong input
    `orElse` parseAddPlaylist input
    `orElse` parseShowPlaylist input
    `orElse` parseTotalDuration input
    `orElse` parsePlayPlaylist input

-- PARSER COMBINATORS
-- Try the first parser, if it fails try the second
orElse :: Either a b -> Either a b -> Either a b
orElse (Right x) _ = Right x
orElse (Left _) y = y

-- Combine two parsers in sequence
and2 :: Parser a -> Parser b -> Parser (a, b)
and2 p1 p2 input = do
    (a, rest1) <- p1 input
    (b, rest2) <- p2 rest1
    Right ((a, b), rest2)

-- Combine three parsers in sequence
and3 :: Parser a -> Parser b -> Parser c -> Parser ((a, b), c)
and3 p1 p2 p3 input = do
    (a, rest1) <- p1 input
    (b, rest2) <- p2 rest1
    (c, rest3) <- p3 rest2
    Right (((a, b), c), rest3)

-- Combine four parsers in sequence
and4 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser (((a, b), c), d)
and4 p1 p2 p3 p4 input = do
    (a, rest1) <- p1 input
    (b, rest2) <- p2 rest1
    (c, rest3) <- p3 rest2
    (d, rest4) <- p4 rest3
    Right ((((a, b), c), d), rest4)

-- ============================================================
-- PRIMITIVE PARSERS
-- ============================================================

parseChar :: Char -> Parser Char
parseChar c [] = Left $ "Expected '" ++ [c] ++ "', but got end of input"
parseChar c (x:xs)
    | c == x = Right (c, xs)
    | otherwise = Left $ "Expected '" ++ [c] ++ "', but got '" ++ [x] ++ "'"

parseString :: String -> Parser String
parseString "" input = Right ("", input)
parseString (c:cs) input = do
    (_, rest1) <- parseChar c input
    (_, rest2) <- parseString cs rest1
    Right (c:cs, rest2)

parseDigit :: Parser Char
parseDigit [] = Left "Expected digit, but got end of input"
parseDigit (x:xs)
    | x >= '0' && x <= '9' = Right (x, xs)
    | otherwise = Left $ "Expected digit, but got '" ++ [x] ++ "'"

parseNumber :: Parser Int
parseNumber input = do
    (numStr, rest) <- parseDigits input
    if null numStr then Left "Expected number" else Right (read numStr, rest)
  where
    parseDigits "" = Right ("", "")
    parseDigits str@(x:xs)
        | x >= '0' && x <= '9' = do
            (rest, remaining) <- parseDigits xs
            Right (x:rest, remaining)
        | otherwise = Right ("", str)

parseWhitespace :: Parser String
parseWhitespace input = Right (span isWhitespace input)
  where isWhitespace c = c == ' ' || c == '\t' || c == '\n'

parseQuotedString :: Parser String
parseQuotedString input = do
    (_, rest1) <- parseChar '"' input
    (str, rest2) <- parseStringContent rest1
    (_, rest3) <- parseChar '"' rest2
    Right (str, rest3)
  where
    parseStringContent "" = Left "Unexpected end of input in string"
    parseStringContent ('"':xs) = Right ("", '"':xs)
    parseStringContent (x:xs) = do
        (rest, remaining) <- parseStringContent xs
        Right (x:rest, remaining)

-- ============================================================
-- DOMAIN-SPECIFIC PARSERS
-- ============================================================

parseDumpable :: Parser Lib1.Dumpable
parseDumpable input = do
    (_, rest) <- parseString "examples" input
    Right (Lib1.Examples, rest)

parseSong :: String -> Either ErrorMsg (String, String, Int, String)
parseSong input = do
    (title, rest1) <- parseQuotedString input
    (_, rest2) <- parseWhitespace rest1
    (_, rest3) <- parseString "by" rest2
    (_, rest4) <- parseWhitespace rest3
    (artist, rest5) <- parseQuotedString rest4
    (_, rest6) <- parseWhitespace rest5
    (duration, rest7) <- parseNumber rest6
    Right (title, artist, duration, rest7)

-- ============================================================
-- COMMAND PARSERS
-- ============================================================

parseDump :: Parser Lib1.Command
parseDump input = do
    (_, rest1) <- parseString "dump" input
    (_, rest2) <- parseWhitespace rest1
    (dumpable, rest3) <- parseDumpable rest2
    Right (Lib1.Dump dumpable, rest3)

parseCreatePlaylist :: Parser Lib1.Command
parseCreatePlaylist input = do
    (_, rest1) <- parseString "create playlist" input
    (_, rest2) <- parseWhitespace rest1
    (name, rest3) <- parseQuotedString rest2
    Right (Lib1.CreatePlaylist name, rest3)

parseAddSong :: Parser Lib1.Command
parseAddSong input = do
    (_, rest1) <- parseString "add song" input
    (_, rest2) <- parseWhitespace rest1
    (title, artist, duration, rest3) <- parseSong rest2
    (_, rest4) <- parseWhitespace rest3
    (_, rest5) <- parseString "to playlist" rest4
    (_, rest6) <- parseWhitespace rest5
    (playlist, rest7) <- parseQuotedString rest6
    Right (Lib1.AddSong title artist duration playlist, rest7)

parseAddPlaylist :: Parser Lib1.Command
parseAddPlaylist input = do
    (_, rest1) <- parseString "add playlist" input
    (_, rest2) <- parseWhitespace rest1
    (nested, rest3) <- parseQuotedString rest2
    (_, rest4) <- parseWhitespace rest3
    (_, rest5) <- parseString "to playlist" rest4
    (_, rest6) <- parseWhitespace rest5
    (parent, rest7) <- parseQuotedString rest6
    Right (Lib1.AddPlaylist nested parent, rest7)

parseShowPlaylist :: Parser Lib1.Command
parseShowPlaylist input =
    case and3 (parseString "show playlist") parseWhitespace parseQuotedString input of
        Right (((_, _), name), rest) -> Right (Lib1.ShowPlaylist name, rest)
        Left err -> Left err

parseTotalDuration :: Parser Lib1.Command
parseTotalDuration input =
    case and3 (parseString "total duration of playlist") parseWhitespace parseQuotedString input of
        Right (((_, _), name), rest) -> Right (Lib1.TotalDuration name, rest)
        Left err -> Left err

parsePlayPlaylist :: Parser Lib1.Command
parsePlayPlaylist input = do
    (_, rest1) <- parseString "play playlist" input
    (_, rest2) <- parseWhitespace rest1
    (name, rest3) <- parseQuotedString rest2
    Right (Lib1.PlayPlaylist name, rest3)


-- PROCESS FUNCTION TO STRING
process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = "Examples:" : map toCliCommand Lib1.examples
process c = ["Parsed as " ++ show c]

-- HELPER FUNCTIONS
quoted :: String -> String
quoted s = "\"" ++ s ++ "\""

-- TO CLI COMMAND

class ToCliCommand a where
    toCliCommand :: a -> String
-- Convert Command ADT back to DSL string format
instance ToCliCommand Lib1.Command where
    toCliCommand :: Lib1.Command -> String
    toCliCommand (Lib1.Dump Lib1.Examples) = 
        "dump examples"
    toCliCommand (Lib1.CreatePlaylist name) = 
        "create playlist " ++ quoted name
    toCliCommand (Lib1.AddSong title artist dur playlist) = 
        "add song " ++ quoted title ++ " by " ++ quoted artist 
        ++ " " ++ show dur ++ " to playlist " ++ quoted playlist
    toCliCommand (Lib1.AddPlaylist nested parent) = 
        "add playlist " ++ quoted nested ++ " to playlist " ++ quoted parent
    toCliCommand (Lib1.ShowPlaylist name) = 
        "show playlist " ++ quoted name
    toCliCommand (Lib1.TotalDuration name) = 
        "total duration of playlist " ++ quoted name
    toCliCommand (Lib1.PlayPlaylist name) = 
        "play playlist " ++ quoted name

-- EQ INSTANCE (Manual equality checking)
-- Manual Eq instance for Command (deriving Eq is forbidden)
instance Eq Lib1.Command where
    (==) :: Lib1.Command -> Lib1.Command -> Bool
    (Lib1.Dump Lib1.Examples) == (Lib1.Dump Lib1.Examples) = True
    (Lib1.CreatePlaylist n1) == (Lib1.CreatePlaylist n2) = n1 == n2
    (Lib1.AddSong t1 a1 d1 p1) == (Lib1.AddSong t2 a2 d2 p2) = 
        t1 == t2 && a1 == a2 && d1 == d2 && p1 == p2
    (Lib1.AddPlaylist n1 p1) == (Lib1.AddPlaylist n2 p2) = n1 == n2 && p1 == p2
    (Lib1.ShowPlaylist n1) == (Lib1.ShowPlaylist n2) = n1 == n2
    (Lib1.TotalDuration n1) == (Lib1.TotalDuration n2) = n1 == n2
    (Lib1.PlayPlaylist n1) == (Lib1.PlayPlaylist n2) = n1 == n2
    _ == _ = False