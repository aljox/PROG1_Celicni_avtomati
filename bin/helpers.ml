(*
  Initialize random number generator.   
*)
Random.self_init ();;

(* INT keyed map. *)
module IntMap = Map.Make(struct
  type t = int
  let compare = compare
end);;

(* Self implementation of a map function. *)
let rec map f xs =
  match xs with
    | [] -> []
    | hd::tl -> f hd :: map f tl;;

(* 
   Helper functions for cell neighbours calculation. 
   They are used in pair-combinations, one for row and one for col.
*)
let xm1 a = a - 1 ;; 
let xp1 a = a + 1 ;;
let xid a = a;;