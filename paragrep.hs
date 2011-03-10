{-# LANGUAGE OverloadedStrings, NamedFieldPuns, 
             ScopedTypeVariables, CPP, RecordWildCards #-}

{- 

  [2011.02.19]



  [2011.02.21]  
    Observing Mac/Linux differences.  First with respect to the
    matching of the "empty_line" pattern against lines containing only
    spaces.  Second, with the failure (or lack of) on broken symlinks.

    The latter may be because of a problem with System.Directory.Tree.
    
  [2011.03.08]
    Switching from System.Directory.Tree to System.FilePath.Find

  

 -}

import Control.Monad 
import Control.Exception

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
import Data.Traversable
import Data.Maybe
import Data.Char
import Data.Either
import Data.IORef
import Numeric (readDec)

-- import Text.Regex
import Text.Regex.Posix
import Prelude as P
import System.Directory
-- import System.Directory.Tree
import System.FilePath.Find as FP

import System.FilePath
import System.Mem 
import System.Environment
import System.Console.GetOpt
import System.Console.ANSI
import System.Exit
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace

-- import UI.HSCurses.Curses
-- import UI.HSCurses.Widgets

#define TEMP

#ifdef TEMP
import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJClass
#endif

-- split file into line array?
-- 

version = "0.0.1.1"
progName = "help"
chatter_tag = " [help] "

----------------------------------------------------------------------


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
--  char == '\n'


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
findHelp :: String -> [String] -> [Partitioner] -> Lines -> MatchTree
findHelp filename terms partitioners lines = 
    case loop "WholeFile" partitioners 0 lines of 
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

readAsLines :: String -> IO (Maybe Lines)
readAsLines path = 
 do 
    isBin <- checkForBinaryFile path 

    if isBin then do
      chatter 3$ " Ignoring binary file: " ++ path
      return Nothing

     else do

      bytes <- B.readFile path
      let lines = B.lines bytes
	  -- Atoms are currently capped at 256 characters.. this should probably be documented...
	  tryAtom x = if B.length x > 256 then Nothing
		      else Just$ toAtom x

	  doline line = (line, S.fromList$ mapMaybe tryAtom$ B.words line)
	  pairs = map doline lines

      return$ Just pairs

#if 0
      -- NEED TO CLOSE FILE HANDLES!
      evaluate isBin
      -- TEMP FIXME: TRYING THIS:
      System.Mem.performGC
      putStrLn$ "Performed GC."
#endif




-- Find help within one file.
findHelpFile :: [String] -> [Partitioner] -> FilePath -> IO MatchTree
findHelpFile terms partitioners file = 
  do x <- readAsLines file
     case x of 
       Just lines -> return$ findHelp file terms partitioners lines
       Nothing    -> return$ Node []

-- | Find help within multiple files.
findHelpFiles :: [String] -> [Partitioner] -> [FilePath] -> IO MatchTree
findHelpFiles terms partitioners files = 
  do allhelp <- P.mapM (findHelpFile terms partitioners) files
     return$ Node allhelp


--------------------------------------------------------------------------------
-- Command line options
--------------------------------------------------------------------------------

-- | Recognized flags
data CmdFlag = 
      NoColor
    | Help
    | HierarchyList String
    | CaseInsensitive
    | PrependDatePart
    | FollowIncludes
    | Root String
    | Verbose (Maybe Int)
    | Version
--    | Verbose (Maybe String)
 deriving (Show,Eq)

-- <boilerplate> Pure boilerplate, would be nice to scrap it:
getRoot (Root x) = Just x
getRoot _        = Nothing
getHierarchy (HierarchyList str) = Just str
getHierarchy _                   = Nothing
getVerbose (Verbose x) = Just x
getVerbose  _          = Nothing
-- </boilerplate>


options :: [OptDescr CmdFlag]
options =
     [ 
       Option ['h']  ["help"]   (NoArg Help)                  "show this help information"
     , Option ['r']  ["root"]   (ReqArg Root "PATH")          "set the root file or directory to search"
     , Option []     ["custom"] (ReqArg HierarchyList "LIST") "use a custom hierarchy of partition methods"
     , Option ['d']  ["date"]        (NoArg PrependDatePart)  "prepend a splitter on date-tags '[2011.02.21]' to the hierarchy list"
     , Option ['i']  ["ignore-case"] (NoArg CaseInsensitive)  "treat file contents and search terms as completely lower-case"
     , Option ['f']  ["follow"]      (NoArg FollowIncludes)   "follow \\include{...} expressions like the original 1988 'help'"
     , Option ['n']  ["nocolor"]     (NoArg NoColor)          "disable ANSI color output"

     , Option ['v']  ["verbose"]     (OptArg (Verbose . fmap safeRead) "LVL")  
		                     "set or increment verbosity level 0-4, default 1"

     , Option ['V']  ["version"] (NoArg Version)              "Show version number." 
     ]

usage = "\nVersion "++version++"\n"++
	 "Usage: "++progName++" [OPTION...] searchterm1 [searchterm2 ...]\n\n"++	

        "The "++progName++" program ____.\n\n"++

        "As output .... \n"++
        "   \n"++
	"   \n"++

	"\nOptions include:\n"
defaultErr errs = error $ "ERROR!\n" ++ (P.concat errs ++ usageInfo usage options)

safeRead :: String -> Int 
safeRead s = 
  case readDec s of
   [(n,"")] -> n
   _ -> error$ "Could not read '"++ s ++"' as an integer."


--------------------------------------------------------------------------------
-- The imperative main function.
--------------------------------------------------------------------------------

-- A mutable global variable:
verbosityRef = unsafePerformIO $ newIORef 1

chatter lvl str = 
  do verbosity <- readIORef verbosityRef
     when (verbosity >= lvl) $
       putStrLn$ chatter_tag ++ str

main = 
 do 
    args <- getArgs
    (opts,terms) <- 
       case getOpt Permute options args of
	 (o,rest,[])  -> return (o,rest)
         (_,_,errs)   -> defaultErr errs

    when (Version `elem` opts)$ do
      putStrLn$ "\nVersion: "++version
      exitSuccess

    when (Help `elem` opts)$ do
      putStrLn$ usageInfo usage options
      exitSuccess

    case mapMaybe getVerbose opts of 
      []        -> return ()
      [Nothing] -> modifyIORef verbosityRef (+1)
      [Just n]  -> writeIORef  verbosityRef n
      _         -> error "More than one -verbose flag not currently allowed."

    when (terms == [])$ defaultErr ["  NO SEARCH TERMS"]

--    chatter 1 "Running help program..."
    chatter 2$ "Searching for terms: " ++ show terms

    let 
        root = case mapMaybe getRoot opts of 
	        []  -> "."
	        [r] -> r
		_ -> error$ progName++": More than one --root option not currently supported"

        default_hierarchy = 
	   if PrependDatePart `elem` opts then
  	        [partition_dateTags, partition_paragraphs]
           else [partition_paragraphs]

        hierarchy = case mapMaybe getHierarchy opts of 
		      []    -> default_hierarchy
		      [str] -> error "Custom hierarchy descriptions not implemented yet."
	

    isdir    <- doesDirectoryExist root
    isfile   <- doesFileExist      root
    txtfiles <- 
      if isdir then do
         chatter 2$ "Reading from root directory: "++show root
#if 0
	 tree <- readDirectoryWithL return root
	 let allfiles = depthFirst tree
#else 
	 allfiles <- fileNames root
#endif
	 chatter 2 $ " Found " ++ show (length allfiles) ++ " regular files (following symlinks)."
	 chatter 4 $ " All files found :" ++ show allfiles

-- TODO/FIXME: This should be OPTIONAL: We might not care about file extensions:
	 return$ filter isTextFile allfiles 

      else if isfile then do
         chatter 2$ "Reading from root file: "++show root
	 return [root]
      else 
	 error$ "Root was not an existing directory or file!: "++ show root

    allhelp <- findHelpFiles terms hierarchy txtfiles

-- Print out the structure of the match tree:
--    putStrLn$ render (pPrint allhelp)

    printMatchTree allhelp

--    BL.-putStrLn "Done."


----------------------------------------------------------------------
