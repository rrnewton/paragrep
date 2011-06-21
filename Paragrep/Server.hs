{-# LANGUAGE OverloadedStrings, NamedFieldPuns, CPP #-} 
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Paragrep.Server ( runServer, defaultHandler ) where

import Paragrep.Lib

import Data.List.Split
import qualified Data.Map as M
import Network.Shed.Httpd
import Network.URI
import Text.JSON

#ifdef LAZYMODE
import qualified Data.ByteString.Lazy.Char8 as B
#else 
import qualified Data.ByteString.Char8      as B
#endif 

-- data MatchHit = MatchHit { file::String, method::String, line::Int, matchtext::Lines }
--   deriving Show
-- --data MatchTree = Match (String,Int,Lines) | Node [MatchTree]
-- data MatchTree = Match MatchHit | Node [MatchTree]
-- type Lines = [(B.ByteString, S.AtomSet)]

matchToJSON :: MatchHit -> JSValue
matchToJSON MatchHit{file,method,line,matchtext} = 
    JSObject$ toJSObject $
	   [("header", showJSON header ),
	    ("year", showJSON (2000::Int)),
	    ("month", showJSON (11::Int)),
	    ("day", showJSON (11::Int)),
	    ("method", showJSON method),
	    ("line", showJSON line),
	    ("body", showJSON body)]
 where 
   lns    = map fst matchtext
   body   = B.concat ["<pre>", B.unlines lns, "</pre>"]
--   body   = B.unlines$  map (`B.append` "<br>") lns
   header = head (dropWhile B.null lns)

-- caseinsensitive, initmethod, hierarchy

defaultHandler root (Request {reqMethod, reqURI, reqHeaders, reqBody}) = 

--  do putStrLn$ "Handling request " ++ reqMethod ++" "++ show (queryToArguments$ uriQuery reqURI)
  do 

     putStrLn$ "Handling request " ++ reqMethod ++" uritostring "++ (uriToString id reqURI "")     
#if 0
     putStrLn$ " queryToArguments " ++ show (args)
     putStrLn$ " Search terms " ++ show terms
     putStrLn$ "Headers: " 
     mapM_ print reqHeaders
     putStrLn$ "Body: " ++ show reqBody
#endif

     ----------------------------------------------------------------------
     -- Do the actual work:
     txtfiles <- listAllFiles root
     allhelp <- findHelpFiles False "" terms [partition_dateTags, partition_paragraphs] txtfiles
     -- Print out the structure of the match tree:
     --    putStrLn$ render (pPrint allhelp)
     printMatchTree allhelp
     ----------------------------------------------------------------------

     -- Create a JSON representation of the results:
     let 
         -- json = toJSObject $
         --        [("header", showJSON "[2000.00.00] {blah blah}"),
	 -- 	 ("date", showJSON (3::Int)),
	 -- 	 ("year", showJSON (2000::Int)),
	 -- 	 ("month", showJSON (11::Int)),
	 -- 	 ("day", showJSON (11::Int)),
	 -- 	 ("body", showJSON "bodbodbod")]

         jsarr = showJSON$ map matchToJSON (matchTreeToList allhelp)
	 jsobj = toJSObject $ 
	    [("resultArray", jsarr)
	    ]
	 resBody = encode jsarr

     putStrLn$ "  Done handling request...\n"
     return (Response { resCode, resHeaders, resBody })     

  where
   args = M.fromList$ queryToArguments$ uriQuery reqURI
   name = args M.! "name"
   terms = filter (not . null) $ 
	   splitOn "+" $ 
	   args M.! "terms"

   resCode    = 0
   resHeaders = [("Content-Type", "application/json")]




-- runServer :: (Request -> IO Response) -> IO ()
runServer port root = 
  do 
     initServer port (defaultHandler root)
     
-- Source

-- reqMethod :: String 
-- reqURI :: URI 
-- reqHeaders :: [(String, String)] 
-- reqBody :: String

-- Response	 
-- resCode :: Int
-- resHeaders :: [(String, String)]  
-- resBody :: String
