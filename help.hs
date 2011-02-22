{-# LANGUAGE OverloadedStrings, NamedFieldPuns, ScopedTypeVariables, CPP #-}

{- 

  [2011.02.19]



  [2011.02.21]  
    Observing Mac/Linux differences.  First with respect to the
    matching of the "empty_line" pattern against lines containing only
    spaces.  Second, with the failure (or lack of) on broken symlinks.

    The latter may be because of a problem with System.Directory.Tree.
    
 -}

import Control.Monad 

--import Data.ByteString.Char8  as B
import qualified Data.ByteString.Lazy.Char8    as B
--import qualified Data.ByteString.Lazy.Internal as BI
import Data.ByteString.Lazy.Internal
import qualified Data.ByteString.Char8         as BS
import Data.List.Split 
--import qualified Data.Set as S 
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
import System.Directory.Tree
import System.FilePath
import System.Environment
import System.Console.GetOpt
import System.Console.ANSI
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

version = "0.0.1"
progName = "help"
chatter_tag = " [help] "

----------------------------------------------------------------------


-- Our main datatype for text includes both a wordset and the raw text:
type Lines = [(B.ByteString, S.AtomSet)]

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
date_regex :: B.ByteString
date_regex = -- mkRegex $ 
   "^ *\\[[0123456789][0123456789][0123456789][0123456789]\\.[0123456789][0123456789]\\.[0123456789][0123456789].*\\]"

empty_line :: B.ByteString
-- empty_line = "^\\s*$"
-- empty_line = "^[ ]*$"
empty_line = "^[ \t]*$"
-- empty_line = "^\\t*$"

-- Partition by lines with a leading date tag, e.g. "[2011.02.19]".

partition_dateTags ::  Partitioner
-- TODO: Since we are only matching the beginning of the line can this be made more efficient?
partition_dateTags = 
  Partitioner ("Date-tagged entries")
	      (split (whenElt ((=~ date_regex) . fst)))

partition_paragraphs ::  Partitioner
partition_paragraphs = 
  Partitioner ("Paragraphs")
	      (split (whenElt ((=~ empty_line) . fst)))

----------------------------------------------------------------------
-- Quick test for a binary bytestream.  

-- We could run "file" or some other program, but hopefully this is
-- quicker and will only draw only the first chunk of the file
-- (probably 32K based on lazy bytestring chunk size).

isBinaryFile :: BS.ByteString -> Bool
isBinaryFile bs = 
   if bool 
   then trace ("File disqualified based on: "++ show (BS.take 20$ BS.filter (not . isValidASCII) bs)) True
   else False
  where 
   bool = BS.any (not . isValidASCII) bs

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
data Loc = Loc { file::String, method::String, line::Int } 
  deriving Show

data MatchTree = Match (String,Int,Lines) | Node [MatchTree]
  deriving Show

#ifdef TEMP
instance Pretty MatchTree where 
  pPrint (Match (str,n,lines)) = text$ show (str,n)
  pPrint (Node ls) = text "(" <> sep (map pPrint ls) <> text ")"
#endif

type PartialMatch = S.AtomSet -- Just the subset of terms that were matched.

-- | @findHelp@ is a pure function that searches through a stream of
--   text using a hierarchical scheme for subdividing its extent.
findHelp :: [String] -> [Partitioner] -> Lines -> MatchTree
findHelp terms partitioners lines = 
    case loop partitioners 0 lines of 
      Left _  -> Node []
      Right x -> x
 where 
   termset    = S.fromList (map toAtom terms)
   targetsize = S.size termset

   checkComplete partials possible = 
      if S.size partials == targetsize
      then Right$ Match possible
      else Left$ partials

   -- This loop returns a pair of (termMatches,MatchTree)
   loop :: [Partitioner] -> Int -> Lines -> Either PartialMatch MatchTree
   loop [] lineOffset text = 
      -- Here we are at the "leaves" of the partitioning hierarchy.
      -- This is where we look for matches.
      -- let allwords = S.unions (map snd text) 
      -- 	  matches  = S.intersection termset allwords in 
      -- This version may be more efficient:
      let matches = S.unions$ map ((S.intersection termset) . snd) text in
      checkComplete matches ("",lineOffset,text)


   loop (Partitioner {pname,fun} : rest) lineOffset text = 
      let 
          -- Here we recursively partition the text according to the NEXT partitioning scheme in the list.
          blocks     = fun text 
	  -- We keep track of the line offset of all the blocks:
	  indices    = scanl (+) lineOffset $ map length blocks
	  submatches = zipWith (loop rest) indices blocks
	  completes  = rights submatches
	  allpartials = S.unions (lefts  submatches)
      in
       if not (null completes) 
       -- If there are complete matches, we're not interested in sibling partial matches.
       then Right$ Node completes
       else checkComplete allpartials (pname,lineOffset,text)


-- | Simplest method for presenting results:
printMatchTree :: MatchTree -> IO ()
printMatchTree (Match (name,int,lines)) =
  do -- putStrLn$ chatter_tag ++ "Match in "++show name++" starting at line "++show int
     putStrLn$ "------------------------------------------------------------"
     putStrLn$  " Match in "++show name++" starting at line "++show int
     putStrLn$ "------------------------------------------------------------"
     B.putStrLn (delines lines)
     putStrLn$ "--------------------------------------------------------------------------------"
printMatchTree (Node ls) = mapM_ printMatchTree ls 
			   

--------------------------------------------------------------------------------


depthFirst :: AnchoredDirTree String -> [String]
depthFirst (root :/ tree) = loop tree
 where 
  loop (File name file)    = [root ++ file]
  loop (Dir name contents) = concatMap loop contents
  loop (Failed name err)   = 
     unsafePerformIO$
       do chatter 1$ " ERROR reading file "++ name++ ": "++ show err
	  return []


isTextFile path = 
   case takeExtension path of 
     -- NOTE: Assuming that files with no extension are text files:
     ""     -> True
     ".txt" -> True
     _      -> False

-- An instance for lazy bytestrings:
instance ToAtom B.ByteString where
  toAtom Empty               = toAtom (""::String)
  -- A small optimization for one-chunk lazy bytestrings:
  toAtom (Chunk chunk Empty) = toAtom chunk
  -- Otherwise we have to copy it:
  toAtom other = toAtom$ BS.concat (B.toChunks other)


readAsLines :: String -> IO (Maybe Lines)
readAsLines path = 
 do bytes <- B.readFile path
    let lines = B.lines bytes

        isBin = case B.toChunks bytes of 
	          []  -> False -- Empty file.
		  h:_ -> isBinaryFile h

        -- Atoms are currently capped at 256 characters.. this should probably be documented...
        tryAtom x = if B.length x > 256 then Nothing
		    else Just$ toAtom x
	doline line = (line, S.fromList$ mapMaybe tryAtom$ B.words line)
	pairs = map doline lines
    when isBin$ putStrLn$ " Ignoring binary file: " ++ path
    return (if isBin then Nothing else Just pairs)

-- Find help within one file.
findHelpFile :: [String] -> [Partitioner] -> FilePath -> IO MatchTree
findHelpFile terms partitioners file = 
  do x <- readAsLines file
     case x of 
       Just lines -> return$ findHelp terms partitioners lines
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
    | HierarchyList String
    | CaseInsensitive
    | PrependDatePart
    | FollowIncludes
    | Root String
 deriving (Show,Eq)

-- Pure boilerplate, would be nice to scrap it:
getRoot (Root x) = Just x
getRoot _        = Nothing
getHierarchy (HierarchyList str) = Just str
getHierarchy _                   = Nothing


options :: [OptDescr CmdFlag]
options =
     [ 
       Option ['r']  ["root"]   (ReqArg Root "PATH")          "set the root file or directory to search"
     , Option []     ["custom"] (ReqArg HierarchyList "LIST") "use a custom hierarchy of partition methods"
     , Option ['d']  ["date"]        (NoArg PrependDatePart)  "prepend a splitter on date-tags '[2011.02.21]' to the hierarchy list"
     , Option ['i']  ["ignore-case"] (NoArg CaseInsensitive)  "treat file contents and search terms as completely lower-case"
     , Option ['f']  ["follow"]      (NoArg FollowIncludes)   "follow \\include{...} expressions like the original 1988 'help'"
     , Option ['n']  ["nocolor"]     (NoArg NoColor)          "disable ANSI color output"
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

    putStrLn$ "Terms : " ++ show terms
    putStrLn$ "OPTS: " ++ show opts

    -- TEMPTOGGLE
    writeIORef verbosityRef 2

    chatter 1 "Running help program..."

    let readWName file = 
	  do contents <- B.readFile file
	     return (file,contents)

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
		      []    -> undefined
		      [str] -> undefined
	

    isdir    <- doesDirectoryExist root
    isfile   <- doesFileExist      root
    txtfiles <- 
      if isdir then do
         chatter 2$ "Reading from root directory: "++show root
	 tree <- readDirectoryWithL return root
	 let allfiles = depthFirst tree
	 B.putStrLn "\n All files found :"
         print allfiles
	 return$ filter isTextFile allfiles 
      else if isfile then do
         chatter 2$ "Reading from root file: "++show root
	 return [root]
      else 
	 error$ "Root was not an existing directory or file!: "++ show root

    B.putStrLn "\n All text files found :"
    print txtfiles
    B.putStrLn "\n All help results:"

    allhelp <- findHelpFiles terms hierarchy txtfiles
    putStrLn$ render (pPrint allhelp)
    printMatchTree allhelp

    B.putStrLn "Done."


----------------------------------------------------------------------
