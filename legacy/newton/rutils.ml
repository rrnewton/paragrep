
(** rutils.ml 

[2009.10.07] This file is old and probably dead.  I duplicated it from my "camlutils" repository.
  
  These are generic utilities, some tree/list/array stuff mostly. 

  -Ryan Newton
*)

open Printf;;

(****************************************************************************)

(* module RUnix =  *)
(* struct *)
(*   open Unix *)
(*   let system_to_string cmd =  *)
(*     let _ = Unix.system cmd in *)
(*     "done" *)
(* end *)

module RString =
struct
  open String
    let string_of_charlist ls = 
    let len = List.length ls in
    let str = create len in
    let rec loop i ls = 
      match ls with
	  [] -> str
	| h::t -> 
	    (set str i h; loop (i+1) t)
    in loop 0 ls
	 
  let charlist_of_string s = 
    let acc = ref [] in
      for i = 0 to length s -1 do
	acc := s.[i] :: !acc
      done;
      List.rev !acc 

(*  let string_reverse s = 
    string_of_charlist (List.rev (charlist_of_string s))*)
  let rev s = 
    let l = length s in
    let newstr = create l in
      for i = 0 to length s - 1 do
	newstr.[i] <- s.[l - i - 1];
      done;
      s;;

  let index_string super sub = 
    let rec loop i j =
      if j = length sub
      then i - j
      else if i = length super
      then -1 
      else if get super i = get sub j
      then loop (i+1) (j+1)
      else loop (i+1) 0
    in loop 0 0

  let map f str = 
    let rec loop i acc = 
      if i < 0
      then acc
      else loop (i-1) ((f str.[i]) :: acc)
    in loop (String.length str - 1) []
	
  let head str n = 
    sub str 0 n
  let tail str n =
    sub str n (length str - n)
end

(****************************************************************************)

module RList = 
struct 
  open List

  (** {6Construction of lists} *)
	
  let init (n:int) f =
    if n<0 then raise 
      (Failure 
	 (Printf.sprintf 
	    "List.init can't handle a negative argument! %d" n));
    let rec loop i = 
      if i = n
      then []
      else (f i)::(loop (i+1))
    in loop 0

  (* Same but for 64 bit ints *)
  let biginit (n:int64) f =
    if n < Int64.zero then raise 
      (Failure 
	 (Printf.sprintf 
	    "List.init can't handle a negative argument! %Ld" n));
    let rec loop i = 
      if i = n
      then []
      else (f i)::(loop (Int64.add i Int64.one))
    in loop Int64.zero;;

  let make n x =
    if n < 0 then raise 
      (Failure 
	 (Printf.sprintf 
	    "List.make can't handle a negative argument! %d" n));
    let rec loop n acc =
      if n = 0
      then acc
      else loop (n-1) (x::acc) 
    in loop n []

  (** {6Accessing and Splitting Lists} *)

  let last ls = 
    let rec loop ls = 
      match ls with
	  [a] -> a
	| _::t -> loop t
	| _ -> raise (Failure "this will never happen")
    in 	if ls=[] 
      then raise (Failure "List.last: given null list.")
      else loop ls

  (*  let head ind lst =
    if ind < 0  
    then failwith
      (Printf.sprintf 
	 "Utils.RList.head can't handle a negative argument! %d" ind)
    else
      let rec loop n ls = 	
	match n,ls with
	    0,  ls    -> []
	  | n,  h::t  -> h :: loop (n-1) t
	  | n,  []    -> raise End_of_file
	      (*failwith
	      (Printf.sprintf "Utils.RList.head of %d hit end of list." ind)*)
      in loop ind lst

  let tail ind lst =
    if ind < 0  
    then failwith
      (Printf.sprintf 
	 "Utils.RList.tail can't handle a negative argument! %d" ind)
    else
      let rec loop n ls = 	
	match n,ls with
	    0,  ls    -> ls
	  | n,  _::t  -> loop (n-1) t
	  | _,  []    -> raise End_of_file
	      (*failwith
	      (Printf.sprintf "Utils.RList.tail of %d hit end of list." ind)*)
      in loop ind lst
  *)
  (*(** This splits a list at a numerical index. *)
  let rec partition n ls = 
    if ls = [] 
    then []
    else
      let frnt = try list_head n ls with End_of_file -> ls 
      and back = try list_tail n ls with End_of_file -> []
      in 
	frnt :: partition n back *)

  let fissure n l = 
    if n < 0 
    then failwith 
      (Printf.sprintf 
	 "Utils.RList.fissure can't take negative number: %d" n)
    else
      let rec loop n l acc =         
	if n = 0 then (rev acc,l)
	else if l = []
	then (*(raise (Failure "Utils.RList.fissure: hit null"))*)
	  raise End_of_file
	else loop (n-1) (tl l) (hd l::acc) 
      in loop n l [];;  

  let rec prefix  n l = fst (fissure n l);;
  let rec postfix n l = snd (fissure n l);;    

  let rec chunk n l =
    if l = [] then []
    else (try
	    let a,b = fissure n l in
	      a :: chunk n b
	  with End_of_file -> [l])

  (** {6Iterating over lists, and searching within them.} *)

  let iteri f ls = 
    let rec loop ls n =
      if ls=[]
      then ()      
      else (f n (hd ls);
	    loop (tl ls) (n+1))
    in loop ls 0

  let mapi f ls = 
    let rec loop ls n =
      if ls=[]
      then []   
      else (f n (hd ls) ::
	    loop (tl ls) (n+1))
    in loop ls 0

  let rec filter_some ls =
    let rec loop ls =
      match ls with 
	|  []         -> []
	|  None::t    -> loop t
	| (Some x)::t -> x :: loop t
    in loop ls;;

  (** Pulls out the first Something *)
  let rec find_some f =
    let rec loop = function
      	[]   -> None 
      | h::t -> 
	  match f h with 
	      None   -> loop t
	    | Some x -> Some x
    in loop;;

  let index x ls =
    let rec loop i ls = 
      if ls=[] 
      then -1 
      else if x = hd ls
      then i 
      else loop (i+1) (tl ls) 
    in
      loop 0 ls;;

  let indexq x ls =
    let rec loop i ls = 
      if ls=[] 
      then -1 
      else if x == hd ls
      then i 
      else loop (i+1) (tl ls) 
    in
      loop 0 ls;;

  let indices x ls =
    let rec loop i ls acc = 
      if ls=[] 
      then acc 
      else if x = hd ls
      then loop (i+1) (tl ls) (i::acc)
      else loop (i+1) (tl ls) acc 
    in
      loop 0 ls [];;

  let index_all pred ls =
    let rec loop i ls =
      if ls=[]
      then []
      else if pred (hd ls)
      then i :: loop (i+1) (tl ls)
      else loop (i+1) (tl ls)
    in
      loop 0 ls;;

  (** {6Removing/replacing stuff in lists.} *)

  (** Removes all instances of an item from a list, using structural
    equality. *)
  (*let rec remove x ls =
    match ls with
	[] -> []
      | h::t when h = x -> remove x t
      | h::t -> h :: remove x t*)

  let remove x ls = 
    let rec loop = function
	[] -> [],false
      | h::t when h = x -> (fst (loop t)),true
      | (h::t) as ls ->
	  let res,b = loop t in
	    if b 
	    then (h::res,true)
	    else (ls,false)
    in fst (loop ls)    

  (** Removes all instances of an item from a list, using physical
    equality. *)
  (*let rec remq x ls =
    match ls with
	[] -> []
      | h::t when h == x -> remq x t
      | h::t -> h :: remq x t*)

  let remq x ls = 
    let rec loop = function
	[] -> [],false
      | h::t when h == x -> (fst (loop t)),true
      | (h::t) as ls ->
	  let res,b = loop t in
	    if b 
	    then (h::res,true)
	    else (ls,false)
    in fst (loop ls)    

  let rec rem1q x ls =
    match ls with
	[] -> []
      | h::t when h == x -> t
      | h::t -> h :: rem1q x t

  let rec removei i = 
    let rec loop acc i = 
      function
	| []              ->  failwith "removei"
	| h::t when i==0  ->  (h,rev_append acc t)
	| h::t            ->  loop (h::acc) (i-1) t
    in loop [] i 

  let rec replacei ind x ls = 
    let rec loop i = function
	[] -> []
      | h::t when i=0 -> x::t
      | h::t -> (h :: loop (i-1) t)
    in loop ind ls


  let rec replace old nu = 
    let rec loop = function
	[] -> []
      | h::t when h = old -> nu :: loop t
      | h::t -> (h :: loop t)
    in loop 

  let rec replace1 old nu = 
    let rec loop = function
	[] -> raise Not_found
      | h::t when h = old -> nu::t
      | h::t -> (h :: loop t)
    in loop 

  let rec replaceq old nu = 
    let rec loop = function
	[] -> []
      | h::t when h == old -> nu :: loop t
      | h::t -> (h :: loop t)
    in loop 

  let rec replace1q old nu = 
    let rec loop = function
	[] -> raise Not_found
      | h::t when h == old -> nu::t
      | h::t -> (h :: loop t)
    in loop 

  (** {6Randomized operations over lists.} *)

  let get_random ls =
    if ls=[] 
    then raise (Failure "List.get_random : list is null")
    else let i = Random.int (length ls) in
      nth ls i

  let random_insert x ls =
    let len = List.length ls in
    let pos = Random.int (len+1) in
    let rec loop ls n =
      if n = 0
      then x::ls
      else (List.hd ls)::(loop (List.tl ls) (n-1))
    in loop ls pos;;

  let random_replace x ls = 
    if ls = []
    then failwith "random_replace"
    else let i = Random.int (List.length ls) in       
      replacei i x ls

  let random_remove ls =
    if ls = []
    then failwith "random_remove"
    else let i = Random.int (List.length ls) in
      removei i ls

  let randomize ls = 
    let arr = Array.of_list ls in
    let len = Array.length arr
    and swap i j = 
      let temp = arr.(i) in
	arr.(i) <- arr.(j);
	arr.(j) <- temp in
      for i = 0 to len - 2 do
	swap i (Random.int (len-i) + i)
      done;
      Array.to_list arr;;   

  (** {6 Printing and conversion } *)

  let to_string tostr ls =
    sprintf "[%s]" (String.concat "; " (map tostr ls));;
  let to_string_nopunct tostr ls =
    sprintf "%s" (String.concat " " (map tostr ls));;  
  let print tostr ls =  printf "%s\n" (to_string tostr ls);; 

  (** {6Lists emulating sets} *)

  let to_set cmp ls =
    let rec loop prev ls = 
      if ls=[]
      then [] 
      else if prev = (hd ls)
      then loop prev (tl ls)
      else hd ls :: loop (hd ls) (tl ls)
    in
      if ls=[]
      then [] 
      else (let ls = fast_sort cmp ls in
	      hd ls :: loop (hd ls) (tl ls))    

  let rec is_set =
    function 
	[] -> true
      | h::l -> 
	  if List.memq h l 
	  then false
	  else is_set l ;;

  let is_subset ls set = 
    List.for_all 
      (fun x -> List.memq x set)
      ls;;

  (** {6 More complex utilities, and utilities specific to certain kinds of lists } *)

  let compact ls cmpr squish = 
    if ls = [] then [] else
      let sorted = List.sort cmpr ls in
      let rec loop l head sqsh dup acc = match l with
	  [] -> (if dup
		 then (head,sqsh) :: acc
		 else acc)
	| h::t ->
	    if 0 = cmpr h head
	    then loop t head (squish sqsh h) true acc
	    else if dup
	    then loop t h h false ((head,sqsh) :: acc) 
	    else loop t h h false acc in 
      let assoc = loop (List.tl sorted) (List.hd sorted) (List.hd sorted) false [] in
	(*printf "Size %d\n" (List.length assoc);*)

	if assoc = []
	then ls
	else
	  let rec loop = function
	      [] -> []
	    | h::t when List.mem_assq h assoc -> 
		let squished = List.assq h assoc in
		  squished ::
		    loop (List.filter (fun x -> 0 <> cmpr x h) t)
	    | h::t -> h :: loop t
	  in loop ls  

  (** Operations on lists of numbers (specifically, lists of integers) **)
  module Lon = 
  struct 
    let find_gap delta lon =
      let rec loop next = function
	| [] -> None
	| h::t when h = next ->
	    loop (next + delta) t
	| h::t when h = (next - delta) ->
	    (* There's a duplication in the list. *)
	    loop next t
	| h::t -> 
	    if delta > 0
	    then (if next < h
		  then Some next
		  else failwith 
		    (sprintf "find_gap: number %d falls off delta %d expected %d"
		       h delta next))
	    else (if next > h
		  then Some next
		  else failwith
		    (sprintf "find_gap: number %d falls off delta %d expected %d"
		       h delta next))
      in if lon = [] 
	then None
	else loop (hd lon) lon


    (** Here's a more general version that works on lists of any
      comparable type.  I don't think it's working yet.  And anyway, I
      decided it's too much obfuscation for the extra abstraction. (Also,
      I figured it would be much slower. *)

    (*and find_gap delta ls =
      let rec loop upwards last next = function
      | [] -> None
      | h::t when h = next ->
      loop upwards next (delta next) t
      | h::t when h = last ->
    (* There's a duplication in the list. *)
      loop upwards last next t
      | h::t -> 
      if upwards
      then (if next < h
      then Some next
      else raise (Invalid_argument "find_gap: list doesn't follow deltas"))
      else (if next > h
      then Some next
      else raise (Invalid_argument "find_gap: list doesn't follow deltas"))
      in match ls with
      | [] 
      | [_] -> None
      | a::b::r -> loop (a < b) a b r
    *)

    (** Having a weird segfault problem when using the to_string
      print-statement from the repl. *)
    let find_gaps delta lon =
      let rec loop next ls = 
	(*printf "Hmmm... looping %d %s\n" next (to_string string_of_int ls);*)
	(*printf "Hmmm... looping %d %d\n" next (if ls=[] then -1 else hd ls);*)
	match ls with 
	  | [] -> []
	  | h::t when h = next ->
	      loop (next + delta) t
	  | h::t when h = (next - delta) ->
	      (* There's a duplication in the list. *)
	      loop next t
	  | h::t -> 
	      if delta > 0
	      then (if next < h
		    then next :: loop (next + delta) ls 
		    else failwith 
		      (sprintf "find_gaps: number %d falls off delta %d expected %d"
			 h delta next))
	      else (if next > h
		    then next :: loop (next + delta) ls
		    else failwith
		      (sprintf "find_gaps: number %d falls off delta %d expected %d"
			 h delta next))
      in if lon = [] then []
	else loop (hd lon) lon
  end 
    
  module Sorted = 
  struct 

    let equality_partition (cmpr : 'a -> 'a -> int)  (sortedls : 'a list) = 
      let eq a b = (cmpr a b == 0) in
      let rec loop thischunk acc ls = 
	match thischunk, ls with
	    [],[] -> map rev (rev acc)
	  | tc,[] -> map rev (rev (tc :: acc))
	  | [],(h::t) -> loop [h] acc t
	  | (a::b),(h::t) when eq a h -> 
	      loop (h::thischunk) acc t 
	  | (a::b),(h::t) ->
	      loop [h] (thischunk::acc) t
      in loop [] [] sortedls

    let separate_dups cmpr sortedls =
      let rec loop dups nondups = function
	| [] -> rev dups, rev nondups
	| [a] -> rev dups, rev (a::nondups)
	| a::b::r when cmpr a b == 0 -> loop (b::dups) nondups (a::r)
	| a::r ->                loop dups (a::nondups) r
      in loop [] [] sortedls

    let rec insert cmpr x ls = 
      let loop = insert cmpr x in
	match ls with
	  | [] -> [x]
	  | h::_ when cmpr x h < 0 -> x :: ls
	  | h::t -> h :: loop t

    let is_set cmpr ls = ([] = (fst (separate_dups cmpr ls)))
    let to_set cmpr ls = snd (separate_dups cmpr ls)      

    let rec is_subset cmpr alst blst =
      match alst,blst with
	  [],[] -> true
	| [],_  -> true
	|  _,[] -> false
	| a::at,b::bt -> 
	    let dir = cmpr a b in
	      if dir = 0
	      then is_subset cmpr at blst
	      else if dir > 0 
	      then is_subset cmpr alst bt
	      else false
	  
  end  


  (** {6 Simple aliases to other operations } *)
  let random_get = get_random
  let shuffle = randomize

  let separate_dups cmpr ls = (Sorted.separate_dups cmpr (sort cmpr ls))  

end;;

(****************************************************************************)

module RArray = 
struct
  open Array;;
  (** This rotates rows into columns in a matrix.  When assertions are
    turned on it checks tha all the rows are the same length *)
  let rotate a =
    let rows = Array.length a 
    and cols = Array.length a.(0) in
      assert (Array.iter 
		(fun r->assert (Array.length r = cols)) a;
	     true);      
      Array.init cols
	(fun j ->
	   Array.init rows 
	   (fun i -> a.(i).(j)));;

  (** Unlike the List version, this destructively randomizes ordering: *)
  let randomize arr = 
    let len = Array.length arr
    and swap i j = 
      let temp = arr.(i) in
	arr.(i) <- arr.(j);
	arr.(j) <- temp in
      for i = 0 to len - 2 do
	swap i (Random.int (len-i) + i)
      done

  let get_random arr = 
    arr.(Random.int (Array.length arr))

  let memq x arr = 
    let rec loop i = 
      if i = Array.length arr
      then false
      else if x == arr.(i)
      then true
      else  loop (i+1)
    in loop 0 

  let index x arr = 
    let rec loop i = 
      if i = Array.length arr
      then -1
      else if x = arr.(i)
      then i 
      else  loop (i+1)
    in loop 0 

  let indexq x arr = 
    let rec loop i = 
      if i = Array.length arr
      then -1
      else if x == arr.(i)
      then i 
      else  loop (i+1)
    in loop 0 

  let to_string tostr ar =
    sprintf "[|%s|]" (String.concat "; " 
			 (Array.to_list (map tostr ar)));;
  
  let print tostr ar =  printf "%s\n" (to_string tostr ar);;

  module Sorted = 
  struct 

    let binsearch_helper succ fail comp x arr =           
      let rec loop a b =
	if a == b then fail a else
	  let i = (a + b) / 2 in
	  let c = comp x arr.(i) in	    	    
	    if c == 0
	    then succ i 
	    else if i == a (* This means a + 1 = b *) 
	    then (if comp x arr.(b) == 0
		  then succ b else fail b)
	    else if c < 0 
	    then loop a i 
	    else loop i b 
      in loop 0 (Array.length arr - 1)

    (** GRRR.  Why doesn't currying typecheck properly here! *)
    let binsearch c x a = binsearch_helper (fun x-> Some x) (fun _-> None) c x a
    let nearest c x a = binsearch_helper (fun x->x) (fun x->x) c x a 

  (*
    let nearest comp x arr =     
    let rec loop a b =
    if a == b then a else
    let i = (a + b) / 2 in
    let c = comp x arr.(i) in
    if c = 0
    then i 
    else if c < 0 
    then loop a i 
    else loop i b 
    in loop 0 (Array.length arr - 1) 
  *)   
  end	 

  (** Arrays of numbers (integers) *)
  module Aon =
  struct

    let nearest n arr =
      if 0 == Array.length arr 
      then failwith "nearest: empty array";      
      let closest = ref 0 in
      let diff = ref (abs (n - arr.(0))) in
	for i = 1 to Array.length arr - 1 do
	  let d = abs (n - arr.(i)) in
	    if d < !diff
	    then (diff := d; closest := i)
	done;
	!closest
		      
    (*let sorted_nearest x arr = *)
    
  end

end;;

(****************************************************************************)

module Misc =
struct 
  let id x = x;;
  let pair a b = (a,b)
  let (@@) f g = (fun x -> f (g x))
  let (@<) f g = (fun x y -> f (g x) (g y))
  let (+=) r v = r := !r + v
  let (+=.) r v = r := !r +. v

  (** This returns a function which just keeps on taking input and
    maintaining a randomized selection.  Use this to get a random
    element of a stream of items of unknown size. *)
  let stream_random () = 
    let weight = ref 0
    and selection = ref None in
      fun x -> 
	incr weight;
	if Random.int !weight == 0
	then selection := Some x;
	match !selection with
	  | Some y -> y
	  | None -> failwith "stream_random: This should not be able to happen."
    
    let flip f =  fun x y -> f y x 

  (*let rtree_helper full ~make_branch:mb ~depth:max_depth = *)
  let rtree_helper full mb max_depth = 
    let rec loop depth = 
      if depth = 0 || (not full && Random.int 2 = 0)
      then 
	mb 0 (fun n -> [])
      else 
	mb (-1)
	  (fun n -> RList.init n (fun _ -> loop (depth-1))) in
      loop max_depth;;

  let random_tree m d = rtree_helper true m d;;
  let random_complete_tree m d = rtree_helper false m d;;

  (** Here's an example of valid input to random_tree *)
  type 'a tree = Tree of 'a * ('a tree list);;    
  let int_branch arity make_children = 
    Tree (Random.bits(), make_children arity);;    

  let random_int64 () =
    let x = Int64.of_int (Random.bits()) 
    and y = Int64.of_int (Random.bits()) 
    and z = Int64.of_int (Random.bits()) in            
    let r = Int64.add (Int64.shift_left x 30) y in
    let r = Int64.add (Int64.shift_left r 30) z in
      r;;	

  let unSome def = function
    | None -> def
    | Some x -> x
  let unoption = unSome
  let is_none = function
    | None   -> true
    | Some _ -> false
  let is_some = function
    | None   -> false
    | Some _ -> true
  let list_of_option = function
    | None   -> []
    | Some x -> [x]

  (** I just do this so often... *)
  let string_of_loi = RList.to_string string_of_int

  let applyn n f x = 
    let rec loop acc i = 
      if i == 0 
      then acc
      else loop (f acc) (i - 1)
    in loop x n

end

(****************************************************************************)

module RHashtbl =
struct
  open Hashtbl

  module HshOrd = 
  struct 
    type t = int 
    let compare a b = compare (hash a) (hash b) 
  end;;
  module HshSet = Set.Make(HshOrd);;

  let length hsh = fold (fun _ _ i -> i+1) hsh 0 
  let length_nodups hsh = 
    let seen = ref HshSet.empty in
      fold (fun a _ i -> 
	      if HshSet.mem a !seen
	      then i
	      else (seen := HshSet.add a !seen;
		    i + 1))
	hsh 0

  let get_random hsh = 
    let f = Misc.stream_random () in
    let result = ref None in
      Hashtbl.iter (fun a b -> result := Some (f (a,b))) hsh;
      match !result with
	| Some x -> x 
	| None -> failwith "RHashtbl.get_random"

  (** This removes any duplicate entries for a given key, leaving only
    the "current" bindings. AKA this returns a "flattened" hashtable. *)
  let flatten hsh = 
    let seen = ref HshSet.empty 
    and newhsh = create (length hsh) in
      iter (fun a b -> 
	      if not (HshSet.mem a !seen)
	      then (seen := HshSet.add a !seen;
		    add newhsh a (find hsh a)))		    
	hsh;
      newhsh

  let to_list hsh = fold (fun a b ls -> (a,b)::ls) hsh []
  let to_list_nodups hsh = 
    let seen = ref HshSet.empty in
      fold (fun a b ls -> 
	      if HshSet.mem a !seen
	      then ls
	      else (seen := HshSet.add a !seen;
		    (a,(find hsh a))::ls))
	hsh []

  (** This does NOT respect the order duplicate keys were entered into
    the hash table *)
  let map_bang f hsh =
    let ls = fold (fun a b ls -> (a,b)::ls) hsh [] in
      clear hsh;
      List.iter
	(fun (a,b) -> add hsh a (f a b))
	ls
end

(*
module StringStream =
struct
  exception Failure

  type t = int ref * string

  let make s : t = (ref 0,s)
		   
  let next ((c,s) : t) = 
    if !c < String.length s
    then Some (let v = s.[!c] in incr c; v)
    else (incr c; None)

  let empty ((c,s) : t) = 
    if !c >= String.length s
    then ()
    else raise Failure

  let copy ((c,s) : t) : t = 
    (ref !c, s)
end*)

(****************************************************************************)

module Graphviz =
struct 
let dotty_graphs expand print g_list = 
  let print_distinct =
    let counter = ref 0 
    and memory = ref [] in
      fun x ->
	if List.mem_assq x !memory 
	then List.assq x !memory
	else (let str = print x ^ string_of_int !counter in
		incr counter;
		memory := (x,str)::!memory;
		str) in
    (**************************)
  let rec dotty_graph g = 
    let str = print g
    and id  = print_distinct g
    and children = expand g in 
    let thisnode = 
      "\n\t\"" ^ id ^ "\" [label=\"" ^ str  ^ "\"];\n" ^
      String.concat ""
	(List.map (fun c -> "\t\"" ^ id ^ "\" -> \"" 
		     ^ (print_distinct c) ^ "\";\n") 
	   children) in
      thisnode ^ 
      String.concat ""
	(List.map dotty_graph  children) in
    (**************************)
  let contents = 
    String.concat "\n"
      (Array.to_list
	 (Array.map dotty_graph g_list)) in 
  let graph = sprintf 
		"digraph WalkerGenotype {\n %s \n}\n" contents in
  let fn = "___temp.dot" in
  let out = open_out fn in
    (*print_string graph; flush stdout;*)
    output_string out graph;
    close_out out;
    ignore (Unix.system (sprintf "dotty %s" fn));
    Unix.unlink fn;;
end 


(****************************************************************************)

module Ansi =
struct
  type color = 
      Reset  
    | Bold_on  | Italics_on  | Underline_on  | Inverse_on 
    | Bold_off | Italics_off | Underline_off | Inverse_off 
    | Black  | Lightgray | Darkgray 
    | Blue   | Lightblue 
    | Green  | Lightgreen
    | Cyan   | Lightcyan 
    | Red    | Lightred 
    | Purple | Lightpurple
    | Brown  | Yellow    | White

  let color_code = 
    (function 
	 Reset          -> "\\033[0;32m"

       | Bold_on        -> "\\033[0;32m"
       | Bold_off       -> "\\033[0;32m"

       | Italics_on     -> "\\033[0;32m"
       | Italics_off    -> "\\033[0;32m"

       | Underline_on   -> "\\033[0;32m"
       | Underline_off  -> "\\033[0;32m"

       | Inverse_on     -> "\\033[0;32m"
       | Inverse_off    -> "\\033[0;32m"
	   
       | Black          -> "\\033[0;30m"
       | Darkgray       -> "\\033[1;30m"
       | White          -> "\\033[1;37m"
       | Lightgray      -> "\\033[0;37m"
       | Blue           -> "\\033[0;34m"
       | Lightblue      -> "\\033[1;34m"
       | Green          -> "\\033[0;32m"
       | Lightgreen     -> "\\033[1;32m"
       | Cyan           -> "\\033[0;36m"
       | Lightcyan      -> "\\033[1;36m"
       | Red            -> "\\033[0;31m"
       | Lightred       -> "\\033[1;31m" 
       | Purple         -> "\\033[0;35m"
       | Lightpurple    -> "\\033[1;35m"
       | Brown          -> "\\033[0;33m"
       | Yellow         -> "\\033[1;33m"
    )

  let color_of_string str = 
    match String.lowercase str with
	"reset"               -> Reset         
      | "bold_on"             -> Bold_on     
      | "italics_on"          -> Italics_on  
      | "underline_on"        -> Underline_on
      | "inverse_on"          -> Inverse_on  
      | "bold_off"            -> Bold_off    
      | "italics_off"         -> Italics_off 
      | "underline_off"       -> Underline_off
      | "inverse_off"         -> Inverse_off  
      | "black"               -> Black        
      | "darkgray"            -> Darkgray     
      | "white"               -> White        
      | "lightgray"           -> Lightgray    
      | "blue"                -> Blue         
      | "lightblue"           -> Lightblue    
      | "green"               -> Green        
      | "lightgreen"          -> Lightgreen   
      | "cyan"                -> Cyan         
      | "lightcyan"           -> Lightcyan    
      | "red"                 -> Red          
      | "lightred"            -> Lightred     
      | "purple"              -> Purple       
      | "lightpurple"         -> Lightpurple  
      | "brown"               -> Brown        
      | "yellow"              -> Yellow  
      | s -> failwith "color_of_string: undefined color" 


  let is_color_str str =
    try ignore (color_of_string str); true
    with Failure "color_of_string: undefined color" -> false

  let print_colored prls =
    let command = "echo -n -e '" ^
		  (String.concat
		     ""
		     (List.map
			(fun (c,s) -> color_code c ^ s)
			prls))
		  ^ "'"
    in 
      flush stdout;
      ignore (Unix.system command);
      flush stdout

  let print_colored_strs strls =
    let command = "echo -n -e '" ^
		  (String.concat
		     ""
		     (List.map
			(fun str ->
			   if is_color_str str
			   then color_code (color_of_string str)
			   else str)
			strls))
		  ^ "'"
    in 
      flush stdout;
      ignore (Unix.system command);
      flush stdout
	      
  let switch_color color = 
    let doit s = ignore (Unix.system ("echo -n -e '" ^ s ^ "'"))
    in flush stdout;
      doit (color_code color);
      flush stdout;;

  let test_colors () =  
    printf "\n\n  Going to test ANSI colors...\n";
    switch_color Green;
    printf "Testing switch_color (Green)\n";
    switch_color Blue;
    printf "Done with that (Blue)\n";
    switch_color White;
    printf "Back to white, testing print_colored... (White)\n";
    print_colored_strs
      ["Red";    " Red";     "LightRed";   "  LightRed\n";
       "Green";  " Green";   "LightGreen"; "  LightGreen\n";
       "Blue";   " Blue";    "LightBlue";  "  LightBlue\n";
       "Cyan";   " Cyan";    "LightCyan";  "  LightCyan\n";
       "Brown";  " Brown";   "Yellow";     "  Yellow\n";
       "Black";  " Black";   "DarkGray";   "  DarkGray\n";
       "LightGray"; " LightGrey"; "White"; "  White\n"
      ]
end


(****************************************************************************)

(****************************************************************************)

(*
(* TEMPORARY *)
  type tree = Leaf of int 
	    | Mono of tree
	    | Bi of tree * tree;;
let test arity f =
  let arity = if arity=(-1) then 1+Random.int 2 else arity in
  let children = f arity in 
    match arity with 
	0 -> Leaf 3 
      | 1 -> Mono (List.hd children)
      | 2 -> Bi (List.hd children,List.hd (List.tl children))
      | _ -> raise (Failure "hmm...");;    

let test2 arity f =
  let arity = if arity=(-1) then 1+Random.int 2 else arity in
  let children = f arity in 
    match arity with 
	0 -> Leaf 3 
      | 1 -> Mono (List.hd children)
      | 2 -> Bi (List.hd children,List.hd (List.tl children))
      | _ -> raise (Failure "hmm...");;    

random_tree test 5 false;;

let round fl = int_of_float (fl +. 0.5);;
*)
