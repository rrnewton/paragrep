{-# LANGUAGE OverloadedStrings, NamedFieldPuns, CPP #-} 
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Paragrep.Server ( runServer, defaultHandler ) where

import Paragrep.Lib

import Data.List.Split
import Data.Maybe
import qualified Data.Map as M
import Network.Shed.Httpd
import Network.URI
import qualified Network.HTTP as N
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
     let uristr = uriToString id reqURI ""
		  -- uriQuery reqURI
     putStrLn$ "Handling request, method " ++ reqMethod ++", args "++ show args ++ " : "++ uristr
#if 0
     putStrLn$ " queryToArguments " ++ show (args)
     putStrLn$ " Search terms " ++ show terms
     putStrLn$ "Headers: " 
     mapM_ print reqHeaders
     putStrLn$ "Body: " ++ show reqBody
#endif

     case M.lookup "command" args of 

      ------------------------------------------------------------
      -- Heuristic: If it is a normal http request we send it through to apache to serve.
      -- TOFIX: We could do this ourselves and serve up the webapp directly.
      Nothing -> do 
	  putStrLn$ "   => REDIRECTING query to apache... "
	  response <- N.simpleHTTP $ N.Request 
	       -- Redirect to localhost on the normal port:
	       { N.rqURI     = fromJust$ parseURI$ "http://localhost:80/" ++ uristr
               , N.rqMethod  = N.GET
	       , N.rqHeaders = []
	       , N.rqBody    = reqBody
	       }
	  case response of 
	    Left connErr -> error$ "simpleHTTP: connection ERROR: " ++ show connErr
	    Right (N.Response{N.rspCode,N.rspHeaders,N.rspBody}) -> return$
	       Response { resCode = 0   -- TODO: translate the code
			, resHeaders= map (\ (N.Header k s) -> (show k,s)) rspHeaders
			, resBody = rspBody }

      -- Otherwise  we've got a request for the web app itself:
      ------------------------------------------------------------
      Just "search" -> do          

	  ----------------------------------------------------------------------
	  -- Do the actual work:
	  txtfiles <- listAllFiles root
	  allhelp <- findHelpFiles False "" terms [partition_dateTags, partition_paragraphs] txtfiles
	  -- Print out the structure of the match tree:
	  --    putStrLn$ render (pPrint allhelp)
	  printMatchTree allhelp
	  ----------------------------------------------------------------------
	  -- Create a JSON representation of the results:
	  let jsarr = showJSON$ map matchToJSON (matchTreeToList allhelp)
	      -- TODO: include some extra info at the top level:
	      jsobj = toJSObject $ 
		 [ ("resultArray", jsarr)
--		 , ("filesSearched", showJSON$ txtfiles)
		 , ("numfiles", showJSON (length txtfiles))
		 ]
	      resBody = encode (JSObject jsobj)
	      response = Response { resCode, resHeaders, resBody }

	  putStrLn$ "  Done handling request.\n  Response: "++ show response ++ "\n"
	  return response     

      ------------------------------------------------------------
      Just "create" -> do
           let file = "./webapp_created_notes.txt"
	       -- body = reqBody -- Couldn't figure out how to populate this.
	       body = args M.! "body"

           putStrLn$ "  Writing new entry to file "++file++ 
		     ": \n================================================================================\n" 
		     ++ body
           appendFile file ("\n\n" ++ body ++ "\n")
	   return$ Response { resCode, resHeaders, 
			      resBody= encode ("New note written successfully, "++ 
					       show (length body) ++" characters"::String) }

      ------------------------------------------------------------
      Just command -> error$ "Unknown command received: "++ show command


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
