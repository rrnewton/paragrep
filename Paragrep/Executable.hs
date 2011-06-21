{-# LANGUAGE OverloadedStrings, NamedFieldPuns, 
             ScopedTypeVariables, CPP, RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

-- This module implements the commandline driver for the paragrep program.

{- 

  [2011.02.21]  
    Observing Mac/Linux differences.  First with respect to the
    matching of the "empty_line" pattern against lines containing only
    spaces.  Second, with the failure (or lack of) on broken symlinks.

    The latter may be because of a problem with System.Directory.Tree.
    
  [2011.03.08]
    Switching from System.Directory.Tree to System.FilePath.Find

  [2011.04.06] 
    Removing the WholeFile partition method from the default hierarchy.

 -}

module Paragrep.Executable (paragrepMain) where 

import Control.Monad 

import Data.Maybe
import Data.Char
import Data.IORef
import Numeric (readDec)

import Prelude as P
import System.Directory
import System.Environment
import System.Console.GetOpt
import System.Exit

import Paragrep.Globals
import Paragrep.Lib
import Paragrep.Server

-- Considering possible pager type functionality for navigating results....
-- import UI.HSCurses.Curses
-- import UI.HSCurses.Widgets



--------------------------------------------------------------------------------
-- Command line options
--------------------------------------------------------------------------------

-- | Recognized flags
data CmdFlag = 
      NoColor
    | Help
    | HierarchyList String
    | CaseInsensitive
    | FollowIncludes
    | Root String
    | Verbose (Maybe Int)
    | Version

    | PrependDatePart
    | WholeFile
    | RunServer Int
 deriving (Show,Eq)

-- <boilerplate> Pure boilerplate, would be nice to scrap it:
getRoot (Root x) = Just x
getRoot _        = Nothing
getHierarchy (HierarchyList str) = Just str
getHierarchy _                   = Nothing
getVerbose (Verbose x) = Just x
getVerbose  _          = Nothing
getRunServer (RunServer x) = Just x
getRunServer _             = Nothing
-- </boilerplate>


options :: [OptDescr CmdFlag]
options =
     [ 
       Option ['h']  ["help"]   (NoArg Help)                  "show this help information"
     , Option ['r']  ["root"]   (ReqArg Root "PATH")          "set the root file or directory to search"
     , Option ['i']  ["ignore-case"] (NoArg CaseInsensitive)  "treat file contents and search terms as completely lower-case"
     , Option ['v']  ["verbose"]     (OptArg (Verbose . fmap safeRead) "LVL")  
		                     "set or increment verbosity level 0-4, default 1"

     , Option ['V']  ["version"] (NoArg Version)              "Show version number." 


     , Option []  []  (NoArg undefined)  ""

     , Option ['d']  ["date"]       (NoArg PrependDatePart)  "prepend a splitter on date-tags '[2011.02.21]' to the hierarchy list"
     , Option ['w']  ["wholefile"]  (NoArg WholeFile)  "append the WholeFile granularity to the list of splitters"


-- TODO / FIXME -- these still need to be implemented:
--     , Option []     ["custom"] (ReqArg HierarchyList "LIST") "use a custom hierarchy of partition methods"
--     , Option ['f']  ["follow"]      (NoArg FollowIncludes)   "follow \\include{...} expressions like the original 1988 'help'"

--     , Option ['n']  ["nocolor"]     (NoArg NoColor)          "disable ANSI color output"

     , Option ['s']  ["server"]  (ReqArg (RunServer . read) "PORT")  "run as HTTP server listening on PORT"

     ]

usage = "\nVersion "++version++"\n"++
	 "Usage: "++progName++" [OPTION...] searchterm1 [searchterm2 ...]\n\n"++	

        "The "++progName++" program provides flexible search at paragraph\n"++
	"granularity, or other custom granularities.  A list of partitioning\n"++
	"methods splits files into progressively smaller pieces.  The program\n"++
	"prints matching spans of text as output, printing the smallest delimited\n" ++
	"span that matches.\n"++
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

paragrepMain = 
 do 
    args <- getArgs
    (opts,terms_) <- 
       case getOpt Permute options args of
	 (o,rest,[])  -> return (o,rest)
         (_,_,errs)   -> defaultErr errs

    ------------------------------------------------------------
    -- First handle execution modes that do something completely
    -- different and exit through a separate control path:

    when (Version `elem` opts)$ do
      putStrLn$ "\nVersion: "++version
      exitSuccess

    when (Help `elem` opts)$ do
      putStrLn$ usageInfo usage options
      exitSuccess

    case (mapMaybe getRunServer opts) of 
      [port] -> do chatter 1$ "Running server on port " ++ show port
		   runServer port defaultHandler
		   chatter 1$ "Server exited."
		   exitSuccess
      [] -> return ()
      ls -> error$ "Cannot run server on multiple ports: "++ show ls

    ------------------------------------------------------------

    let caseinsensitive = CaseInsensitive `elem` opts

    case mapMaybe getVerbose opts of 
      []        -> return ()
      [Nothing] -> modifyIORef verbosityRef (+1)
      [Just n]  -> writeIORef  verbosityRef n
      _         -> error "More than one -verbose flag not currently allowed."

    let terms = if caseinsensitive then map (map toLower) terms_ else terms_
    when (terms == [])$ defaultErr ["  NO SEARCH TERMS"]

    chatter 2$ "Searching for terms: " ++ show terms

    let 
        root = case mapMaybe getRoot opts of 
	        []  -> "."
	        [r] -> r
		_ -> error$ progName++": More than one --root option not currently supported"

        hier1 = [partition_paragraphs]
	hier2 = if PrependDatePart `elem` opts 
		then partition_dateTags : hier1
		else hier1 
        default_hierarchy = hier2
        initmethod = 
	        if WholeFile `elem` opts 
		then "WholeFile"
		else ""
                
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

    allhelp <- findHelpFiles caseinsensitive initmethod terms hierarchy txtfiles

-- Print out the structure of the match tree:
--    putStrLn$ render (pPrint allhelp)

    printMatchTree allhelp

--    BL.-putStrLn "Done."


----------------------------------------------------------------------
