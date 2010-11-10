let unopt = function
	| Some x -> x
	| None -> failwith "Cannot unopt None"

let may o f = match o with
	| Some x -> f x
	| None -> ()

let compose f g = fun x -> f (g x)

let forever f x = while true do f x done

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

let apply x f = f x

let pi = 4. *. atan 1.

let ignore_exceptions f x =
  try ignore (f x) with _ -> ()

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

let delay s =
	ignore_exceptions (Unix.select [] [] []) s

(* Taken from Jane Street's Lib *)
let ( |! ) x f = f x
