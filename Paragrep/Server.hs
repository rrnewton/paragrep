{-# LANGUAGE NamedFieldPuns #-} 
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Paragrep.Server ( runServer, defaultHandler ) where

import Data.List.Split
import qualified Data.Map as M
import Network.Shed.Httpd
import Network.URI
import Text.JSON


defaultHandler (Request {reqMethod, reqURI, reqHeaders, reqBody}) = 

--  do putStrLn$ "Handling request " ++ reqMethod ++" "++ show (queryToArguments$ uriQuery reqURI)
  do putStrLn$ "Handling request " ++ reqMethod ++" uritostring "++ (uriToString id reqURI "")     
     putStrLn$ " queryToArguments " ++ show (args)
     putStrLn$ " Search terms " ++ show terms
     putStrLn$ "Headers: " 
     mapM_ print reqHeaders
     putStrLn$ "Body: " ++ show reqBody

     -- let searchResults = ["a", "b", "c"]

     -- Create a JSON representation of the results:
     let json = toJSObject $
                [("header", showJSON "[2000.00.00] {blah blah}"),
		 ("date", showJSON (3::Int)),
		 ("year", showJSON (2000::Int)),
		 ("month", showJSON (11::Int)),
		 ("day", showJSON (11::Int)),
		 ("body", showJSON "bodbodbod")]

	 resBody = encode [json,json,json]

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
runServer port handler = 
  do 
     initServer port handler
     
-- Source

-- reqMethod :: String 
-- reqURI :: URI 
-- reqHeaders :: [(String, String)] 
-- reqBody :: String

-- Response	 
-- resCode :: Int
-- resHeaders :: [(String, String)]  
-- resBody :: String
