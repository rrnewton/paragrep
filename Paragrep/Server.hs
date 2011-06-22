{-# LANGUAGE OverloadedStrings, NamedFieldPuns, CPP #-} 
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Paragrep.Server ( runServer, defaultHandler ) where

import Paragrep.Lib

import Data.List.Split
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Network.Shed.Httpd
import Network.URI
import qualified Network.HTTP as N
import Text.JSON
import Debug.Trace

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
-- Word wrap is not working for me on iphone [2011.06.21]:
--   body   = B.concat ["<pre style=\"word-wrap:break:word\">", B.unlines lns, "</pre>"]
   -- Eek, should probably go to more trouble to ESCAPE characters here:
   -- TODO!  Can also get fancy here and:
   --   (1) Highlight search terms
   --   (2) Highlight dates and do other rrn-text-mode style highlighting.
   body   = B.unlines$  map (`B.append` "<br>") lns
   header = head (dropWhile B.null lns)

stripHeadersHack str | "HTTP/" `isPrefixOf` str = 
  -- Here we scroll forward until there is an empty line that separates the headers from the body.
  unlines $ tail $ 
  dropWhile (not . null) $ 
  lines str

stripHeadersHack str = str


-- There must be a library routine for this but I can't find it.
-- In the http request I get things like "foo+bar" as well as the percent characters.
-- unEscapeString works for the percent characters but not the spaces.
decodeQueryString :: String -> [String]
decodeQueryString str =
   map unEscapeString $   -- from Network.URI
   filter (not . null) $ 
   splitOn "+" $ 
   str


----------------------------------------------------------------------------------------------------
defaultHandler root (Request {reqMethod, reqURI, reqHeaders, reqBody}) = 

  do 
     let uristr = uriToString id reqURI ""
		  -- uriQuery reqURIerms
     putStrLn$ "Handling request, method " ++ reqMethod ++", args "++ show args ++ " : "++ uristr
#if 0
     putStrLn$ " queryToArguments " ++ show (args)
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
	       -- let stripped = stripHeadersHack rspBody in 
	       -- trace ("STRIPPED "++ stripped) $
	       -- Strip out some of the headers to avoid duplicates:
	       let stripHeaders = [N.HdrConnection, N.HdrContentLength] in
	       trace ("  Apache Response Code: " ++ show rspCode) $
	       -- trace ("HEADERS " ++ show rspHeaders)
	       Response { resCode 
			, resHeaders= map (\ (k,s) -> (show k,s)) $ 
			              filter (not . (`elem` stripHeaders) . fst) $ 
			              map (\ (N.Header k s) -> (k,s)) $ 
			              rspHeaders
			-- Annoyingly, we can end up duplicating the headers here... ick:
			-- , resBody = stripped -- The headers didn't actually appear here to be stripped.
			, resBody = rspBody
			}

      -- Otherwise  we've got a request for the web app itself:
      ------------------------------------------------------------
      Just "search" -> do          
	  putStrLn$ " Search terms " ++ show terms

	  ----------------------------------------------------------------------
	  -- Do the actual work:
	  txtfiles <- listAllFiles root
	  -- Setting case insensitive:
	  allhelp <- findHelpFiles True "" terms [partition_dateTags, partition_paragraphs] txtfiles
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

	  putStrLn$ "  Done handling request."
	  -- putStrLn$ "  Response: "++ show response ++ "\n"
	  return response     

      ------------------------------------------------------------
      Just "create" -> do
           let file = "./webapp_created_notes.txt"
	       -- body = reqBody -- Couldn't figure out how to populate this.
	       body = concat$ intersperse " " $
		      decodeQueryString$ args M.! "body"

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
   terms = decodeQueryString $ args M.! "terms"

   resCode    = 200 -- 'OK' in HTTP speak.
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
