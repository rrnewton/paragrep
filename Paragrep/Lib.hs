{-# LANGUAGE OverloadedStrings, NamedFieldPuns, 
             ScopedTypeVariables, CPP, RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

-- This module contains the core algorithmic pieces of the paragrep program.

module Paragrep.Lib where 

import Paragrep.Globals

import Control.Monad 

--import Data.ByteString.Char8  as B
import qualified Data.ByteString.Lazy.Char8    as BL
import qualified Data.ByteString.Lazy.Internal as BI
import qualified Data.ByteString.Char8         as BS

-- This switch determines whether files are read lazily or in one go:
-- #define LAZYMODE
-- [2011.03.09] Right now I'm getting errors from too many files open in lazy mode.
#ifdef LAZYMODE
import qualified Data.ByteString.Lazy.Char8 as B
#else 
import qualified Data.ByteString.Char8      as B
#endif 

import Data.List.Split 
import qualified StringTable.AtomSet as S 
import StringTable.Atom
import Data.Maybe
import Data.Char
import Data.Either

-- import Text.Regex
import Text.Regex.Posix
import Prelude as P
-- import System.Directory.Tree
import System.FilePath.Find as FP

import System.FilePath
import System.IO


#define TEMP

#ifdef TEMP
import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJClass
#endif


-- Our main datatype for text includes both a wordset and the raw text:
type Lines = [(B.ByteString, S.AtomSet)]

delines :: Lines -> B.ByteString 
delines = B.unlines . map fst 

-- Hmm, why is this not defined in StringTable.AtomSet?
instance Show S.AtomSet where 
  show as = "fromList "++ show (S.toList as)

----------------------------------------------------------------------
-- Partitioning schemes:

-- A partitioning stage takes a list of lines and groups them.
data Partitioner = 
   Partitioner { pname :: String,
		 fun   :: Lines -> [Lines] }

--date_regex :: String
date_regex :: BL.ByteString
date_regex = -- mkRegex $ 
   "^ *\\[[0123456789][0123456789][0123456789][0123456789]\\.[0123456789][0123456789]\\.[0123456789][0123456789].*\\]"

empty_line :: BL.ByteString
-- empty_line = "^\\s*$"
-- empty_line = "^[ ]*$"
empty_line = "^[ \t]*$"
-- empty_line = "^\\t*$"

-- Partition by lines with a leading date tag, e.g. "[2011.02.19]".

partition_dateTags ::  Partitioner
-- TODO: Since we are only matching the beginning of the line can this be made more efficient?
partition_dateTags = 
  Partitioner ("DateTaggedEntries")
	      (split (keepDelimsL$ whenElt ((=~ date_regex) . fst)))

partition_paragraphs ::  Partitioner
partition_paragraphs = 
  Partitioner ("Paragraphs")
	      (split (keepDelimsL$ whenElt ((=~ empty_line) . fst)))

----------------------------------------------------------------------
-- Quick test for a binary bytestream.  

-- We could run "file" or some other program, but hopefully this is
-- quicker and will only draw only the first chunk of the file
-- (probably 32K based on lazy bytestring chunk size).

isBinaryFile :: BS.ByteString -> Bool
isBinaryFile bs = 
   if bool 
   then -- trace ("File disqualified based on: "++ show (BS.take 20$ BS.filter (not . isValidASCII) bs)) 
	True
   else False
  where 
   bool = BS.any (not . isValidASCII) bs


-- Read only the first part of a file to determine if it is binary:
-- Here we suffer extra system calls to read the file twice (peek then
-- full read).  But we don't want to grep through large binary files!
-- 
-- 
checkForBinaryFile path = 
 do hndl  <- openFile path ReadMode
    let target = 2000 -- Target number of bytes we want to check.
    bytes <- B.hGet hndl target 
    return (isBinaryFile bytes)



-- Hmm... what could go here?
isValidASCII char =
  isPrint char ||
  isSeparator char || 
  isSpace char 


----------------------------------------------------------------------
-- Generic Hierarchical Matching Implementation
----------------------------------------------------------------------

-- type Loc = (String,String,Int)
data MatchHit = MatchHit { file::String, method::String, line::Int, matchtext::Lines }
  deriving Show

--data MatchTree = Match (String,Int,Lines) | Node [MatchTree]
data MatchTree = Match MatchHit | Node [MatchTree]
  deriving Show

#ifdef TEMP
instance Pretty MatchTree where 
  pPrint (Match (MatchHit{method,line})) = text$ show (method,line)
  pPrint (Node ls) = text "(" <> sep (map pPrint ls) <> text ")"
#endif

type PartialMatch = S.AtomSet -- Just the subset of terms that were matched.

-- | @findHelp@ is a pure function that searches through a stream of
--   text using a hierarchical scheme for subdividing its extent.
findHelp :: String -> String -> [String] -> [Partitioner] -> Lines -> MatchTree
findHelp initmethod filename terms partitioners lines = 
    case loop initmethod partitioners 0 lines of 
      Left _  -> Node []
      Right x -> x
 where 
   termset    = S.fromList (map toAtom terms)
   targetsize = S.size termset

   -- See if we've hit all the terms:
   checkComplete partials possible = 
      if S.size partials == targetsize
      then Right$ Match possible
      else Left$ partials

   
   -- This loop returns a pair of (termMatches,MatchTree)
   loop :: String -> [Partitioner] -> Int -> Lines -> Either PartialMatch MatchTree
   loop method [] lineOffset text = 
      -- Here we are at the "leaves" of the partitioning hierarchy.
      -- This is where we look for matches.
      -- let allwords = S.unions (map snd text) 
      -- 	  matches  = S.intersection termset allwords in 
      -- This version may be more efficient:
      let matches = S.unions$ map ((S.intersection termset) . snd) text in
      checkComplete matches (MatchHit filename method lineOffset text)


   loop method (Partitioner {pname,fun} : rest) lineOffset text = 
      let 
          -- Here we recursively partition the text according to the NEXT partitioning scheme in the list.
          blocks     = fun text 
	  -- We keep track of the line offset of all the blocks:
	  indices    = scanl (+) lineOffset $ map length blocks
	  submatches = zipWith (loop pname rest) indices blocks
	  completes  = rights submatches
	  allpartials = S.unions (lefts  submatches)
      in
       if not (null completes) 
       -- If there are complete matches, we're not interested in sibling partial matches.
       then Right$ Node completes
       -- We use method == "" to indicate that this level of the hierarchy should NOT generate output matches.
       else if method == ""
	    then Left allpartials
	    else checkComplete allpartials (MatchHit filename method lineOffset text)

-- | Simplest method for presenting results:
printMatchTree :: MatchTree -> IO ()
printMatchTree (Match MatchHit{..}) =
  do -- putStrLn$ chatter_tag ++ "Match in "++show name++" starting at line "++show int
     let msg = chatter_tag++"Match in "++show file ++
	                    ", granularity "++ show method++
			    ", starting at line "++show line++":"
	 sep = take (length msg) (repeat '-')
--     putStrLn sep
     putStrLn ""
     putStrLn msg
     putStrLn sep
     B.putStrLn (delines matchtext)
--     putStrLn$ "--------------------------------------------------------------------------------"
printMatchTree (Node ls) = mapM_ printMatchTree ls 
			   

--------------------------------------------------------------------------------

#if 0
-- Traverse a directory to get a list of paths to files, and symbolic links that point to files.
depthFirst :: AnchoredDirTree String -> [String]
depthFirst (root :/ tree) = loop tree
 where 
  abs = isAbsolute root
  -- This is unintuitive but if the root dir was absolute then the filenames are also absolute:
  loop (File name file)    = if abs then [file] else [root ++ file]
  loop (Dir name contents) = concatMap loop contents
  loop (Failed name err)   = 
     unsafePerformIO$
       do chatter 1$ " ERROR reading file "++ name++ ": "++ show err
	  return []
#endif


-- Get all the files we're intrested in from within a directory.
fileNames :: String -> IO [String]
fileNames root = FP.find always pred root
 where
  pred =
    do stat  <- followStatus 
       case stat of 
         Nothing   -> (fileType ==? RegularFile)
	 Just stat -> return (statusType stat == RegularFile)


-- The file extensions that we think represent text files.
isTextFile path = 
   case takeExtension path of 
     -- NOTE: Assuming that files with no extension are text files:
     ""     -> True
     ".txt" -> True
     ".hs"  -> True
     ".cpp" -> True
     ".c"   -> True
     _      -> False

-- An instance for lazy bytestrings:
instance ToAtom BL.ByteString where
  toAtom BI.Empty                  = toAtom (""::String)
  -- A small optimization for one-chunk lazy bytestrings:
  toAtom (BI.Chunk chunk BI.Empty) = toAtom chunk
  -- Otherwise we have to copy it:
  toAtom other = toAtom$ BS.concat (BL.toChunks other)



-- #ifdef LAZYMODE
--         isBin = case BL.toChunks bytes of 
-- 	          []  -> False -- Empty file.
-- 		  h:_ -> isBinaryFile h
-- #else
--         isBin = isBinaryFile bytes
-- #endif

readAsLines :: Bool -> String -> IO (Maybe Lines)
readAsLines caseinsensitive path = 
 do 
    isBin <- checkForBinaryFile path 

    if isBin then do
      chatter 3$ " Ignoring binary file: " ++ path
      return Nothing

     else do

      bytes <- B.readFile path
      let 
          lower = if caseinsensitive then B.map toLower else id
          lines     = B.lines bytes
	  -- Atoms are currently capped at 256 characters.. this should probably be documented...
	  tryAtom x = if B.length x > 256 then Nothing
		      else Just$ toAtom (lower x)

	  doline line = (line, S.fromList$ mapMaybe tryAtom$ B.words line)
	  pairs = map doline lines

      return$ Just pairs




-- Find help within one file.
findHelpFile :: Bool -> String -> [String] -> [Partitioner] -> FilePath -> IO MatchTree
findHelpFile caseinsensitive methodname  terms partitioners file = 
  do x <- readAsLines caseinsensitive file
     case x of 
       Just lines -> return$ findHelp methodname file terms partitioners lines
       Nothing    -> return$ Node []

-- | Find help within multiple files.
findHelpFiles :: Bool -> String ->  [String] -> [Partitioner] -> [FilePath] -> IO MatchTree
findHelpFiles caseinsensitive methodname terms partitioners files = 
  do allhelp <- P.mapM (findHelpFile caseinsensitive methodname terms partitioners) files
     return$ Node allhelp
