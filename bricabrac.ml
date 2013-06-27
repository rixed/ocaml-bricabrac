let unopt = function
	| Some x -> x
	| None -> failwith "Cannot unopt None"

let may o f = match o with
	| Some x -> f x
	| None -> ()

let optdef o def = match o with
	| Some x -> x
	| None -> def

let optmap o f = match o with
    | Some x -> Some (f x)
    | None -> None

let optbind o f = match o with
    | Some x -> f x
    | None -> None

let compose f g = fun x -> f (g x)

let forever f x = ignore (while true do f x done)

let nop () = ()

let eta f x = fun () -> f x

let try_finalize f x finally y =
  let res =
    try f x with exn ->
      finally y;
      raise exn in
    finally y;
    res

let with_mutex m f x =
  Mutex.lock m ;
  try_finalize f x Mutex.unlock m

let with_fd fd f =
  try_finalize f fd Unix.close fd

let with_file_in fname f =
    let ic = open_in fname in
    try_finalize f ic close_in ic

let with_file_out ?(mode=[Open_wronly;Open_trunc;Open_creat]) ?(perm=0o644) fname f =
    let oc = open_out_gen mode perm fname in
    try_finalize f oc close_out oc

let apply x f = f x

let pi = 4. *. atan 1.

let ignore_exceptions f x =
  try ignore (f x) with _ -> ()

let none_if_exception f x =
  try Some (f x) with _ -> None

let rec restart_on_EINTR f x =
  try f x with Unix.Unix_error (Unix.EINTR, _, _) ->
    restart_on_EINTR f x

let string_split line delim =
  let rec aux strs i =
    if i >= String.length line then List.rev strs
    else let next_i =
      try String.index_from line i delim with Not_found -> String.length line in
    let substr = String.sub line i (next_i - i) in
      aux (substr :: strs) (next_i + 1) in
    aux [] 0

let string_translate str a b =
	let len = String.length str in
	let ret = String.create len in
	let char_at c =
		try b.[ String.index a str.[c] ]
		with Not_found -> str.[c] in
	for c = 0 to len-1 do ret.[c] <- char_at c done ;
	ret

let rec join elt_to_string str list =
  match list with
    | [] -> ""
    | [head] -> elt_to_string head
    | head :: tail ->
        (elt_to_string head)^str^(join elt_to_string str tail)

let list_to_string elt_to_string list =
  join elt_to_string "\n" list

let list_intersect l1 l2 =
  List.filter (fun e -> List.mem e l2) l1

let list_sub l1 l2 =
  List.filter (fun e -> not (List.mem e l2)) l1

let list_init n f =
  let rec aux prevs n =
    if n = 0 then prevs else
      aux (f () :: prevs) (n - 1) in
    aux [] n

let rec list_last = function
	| [e] -> e
	| _ :: l -> list_last l
	| [] -> raise (Failure "list_last")

let delay s =
	ignore_exceptions (Unix.select [] [] []) s

let id x = x

let ( |> ) x f = f x
let ( |- ) f g = fun x -> g (f x)
let ( ||- ) f g = fun x y -> g (f x y)

let pair a b = a, b
let triplet a b c = a, b, c
let tuple3 a b c = a, b, c
let tuple4 a b c d = a, b, c, d
let tuple5 a b c d e = a, b, c, d, e
let tuple6 a b c d e f = a, b, c, d, e, f
let tuple7 a b c d e f g = a, b, c, d, e, f, g
let tuple8 a b c d e f g h = a, b, c, d, e, f, g, h

(* combine two arrays, with a1 length the shorter *)
let array_zip f a1 a2 =
	Array.init (Array.length a1) (fun i -> f a1.(i) a2.(i))

(* Iter over a directory *)
let foreach_file path f =
	let dir = Unix.opendir path in
	try
		let next () =
			let entry = Unix.readdir dir in
			let fname = path ^ "/" ^ entry in
			let stats = Unix.stat fname in
			f fname stats in
		forever next ()
	with End_of_file -> ()

let hashtbl_keys h = Hashtbl.fold (fun k _ l -> k::l) h []
let hashtbl_values h = Hashtbl.fold (fun _ v l -> v::l) h []

let read_file filename = 
	let lines = ref []
	and ic = open_in filename in 
	(try
		forever (fun () -> lines := input_line ic :: !lines) ()
	with End_of_file -> close_in ic) ;
	List.rev !lines

let mkdir_all ?(is_file=false) dir =
    let dir_exist d =
        try Sys.is_directory d with Sys_error _ -> false in
    let dir = if is_file then Filename.dirname dir else dir in
    let rec ensure_exist d =
        if String.length d > 0 && not (dir_exist d) then (
            ensure_exist (Filename.dirname d) ;
            try Unix.mkdir d 0o755
            with Unix.Unix_error (Unix.EEXIST, "mkdir", _) ->
                (* Happen when we have "somepath//someother" (dirname should handle this IMHO *)
                ()
        ) in
    ensure_exist dir

