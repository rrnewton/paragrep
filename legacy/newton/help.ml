(* HELP - help database accessing program
** Rewritten and expanded by Ryan Newton 5/20/2004
** Original C version: Ronald L. Rivest -- 10/14/88
o.....**
** [2009.10.07] Adding new functionality to recognize my date syntax.
** Basically, the policy is that if a file has more than one date in
** it, we use that as the delimeter instead of blank lines.  
**)

open Printf
open Unix 
open Rutils

(* CONSTANTS *)

let version = "0.9"

(* SETTINGS *)

let homedir = String.concat "" [getenv "HOME"; "/"]

(* Hardcoded default, should be set by the "-f" flag. *)
let repository = ref (String.concat ""
			[homedir; "help/helpfile/HELP"])

let includepat = "\\include{.*}"
let includeregexp = Str.regexp includepat

(*let date_pat = "\\[[0123456789]{4}\\.[0123456789]{2}\\.[0123456789]{2}.*\\]"*)
let date_pat = "\\[[0123456789][0123456789][0123456789][0123456789]\\.[0123456789][0123456789]\\.[0123456789][0123456789].*\\]"
let date_regexp = Str.regexp date_pat

(* Global variables, switches *)

let print_names = ref false
let page_screen = ref false
let search_terms = ref ([] : string list)
let regexps = ref []

(*let all_pars = ref []*)

(********************************************************************)

let print_version () : unit = 
  print_string "help - search text database: ver ";
  print_endline version;
  exit 0;;

let print_help () : unit = 
  print_endline "Usage: help <options> <search-terms>";
  print_endline "options are:";
  print_endline "  -h       Show this help message.";
  print_endline "  -v       Show the version.";
  print_endline "  -f FILE  Use FILE as help database"; 
  print_endline "  -n       Print file name info with each entry"; 
  print_endline "  -m       \"More\", page screen one entry at a time.";
  print_endline "  -s       Suppress ERRORs due to missing files.";
(*  print_endline "  -w       Dammit I forgot what this does.";  *)

(*  print_endline "  -c   Use ANSI color.  Green and red theme.";
  print_endline "  -b   Use ANSI color.  Blue and yellow theme.";
  print_endline "  -nc  Don't use color.  Plain ASCII.";
  print_endline "  -s   Sort output by filesize, increasing.";*)
  exit 0;;

let clear_screen () = ignore (Unix.system "clear")

let process_filename fn =
  (* Right now this just gets rid of the tilde syntax.*)
  Str.global_replace 
    (Str.regexp_string "~/")
    homedir fn

(* Tells if a character is whitespace.*)
let is_whitespace = function
  | ' ' | '\n' | '\t' -> true
  | _ -> false


let do_paragraph fn par =
(*   printf "DOING PAR: \n"; *)
(*   printf "================================================================================\n"; *)
(*   printf "%s" par; *)
(*   printf "================================================================================\n"; *)
  if List.for_all (fun rexp ->
		     try 
		       ignore (Str.search_forward rexp par 0);
		       true
		     with Not_found -> false)
    !regexps
  then ((*print_newline (); *)
	if !print_names
	then printf "[%s]\n" fn;
	print_endline par;
	print_newline ();
	if !page_screen
	then (ignore (input_byte Pervasives.stdin);
	      clear_screen ())
  );;
		
let errors_enabled = ref true

let input_all_lines chan = 
  let rec loop() = 
    try let l = input_line chan in
        l :: loop()
    with End_of_file -> []
  in loop()

(* If there are more than this many date-entry lines in a file, we
   automatically assume it is delimited in that fashion *)
let num_dates_to_trigger = 2

let rec do_file fn =
  let pipe = Unix.open_process_in ("grep -sE \""^ date_pat ^"\" \""^ fn ^"\"") in
  let lines = input_all_lines pipe in 
  let _ = close_process_in pipe in
  let by_date = List.length lines >= num_dates_to_trigger in
   (  (*if by_date then printf "By date %s\n" fn;*)
    try
    (let f = open_in fn in    
     let thispar = ref [] in
     let start = String.length "\\include{" - 1 in
     let dopar () = do_paragraph fn
		      (String.lowercase 
			 (String.concat "\n" 
			    (List.rev !thispar)))
     in
       try
         (* This is a nasty piece of imperative code *)
	 while true do
	   let line = input_line f in
	     if 
	      (try (let n = Str.search_forward includeregexp line 0
		    in n < 2)
	       with Not_found -> false)
	     then ( (* Pull in the included file *) 
		   let s = Str.matched_string line in
		   let len  =  String.length s - 1 - start in
		   let file =  String.sub s start len in
		     do_file (process_filename file))
	     else if (not by_date && 
	              List.for_all is_whitespace
			(RString.charlist_of_string line)) ||
		     (by_date && 
	              (try let _ = Str.search_forward date_regexp line 0 in true
		       with Not_found -> false))
	     then ((*if by_date then printf "Breaker line: %s\n" line;*)
	           dopar ();
		   thispar := [line])
	     else (thispar := line :: !thispar)
	 done
       with End_of_file -> 
	 dopar ();
	 close_in f)
    with Sys_error _ -> 
     if !errors_enabled
     then printf "ERROR!!!: Could not open file: \"%s\"\n" fn);;


(********************************************************************)

let main () =
  let paths = (* Here are all those filepaths. *)    
    List.map process_filename
      (List.filter
	 (* Process flags *)
	 (function
	    | "-h"  -> print_help (); false
	    | "-v"  -> print_version (); false
	    | "-n"  -> print_names := true; false
	    | "-m"  -> page_screen := true; false
	    | "-s"  -> errors_enabled := false; false
	    | "-f"  -> false (* TODO *)
	    | other -> true)
	 (List.tl (Array.to_list Sys.argv))) in
    
    if paths = [] 
    then (print_endline "help needs an argument!"; exit 0);
    search_terms := List.map String.lowercase paths;
    regexps := List.map Str.regexp !search_terms;

    if !page_screen then clear_screen();
    do_file !repository;
;;

(*******************************)
main ();;
