open Graphics;;
open Helpers;;

(* 
  Each cell holds its data in cell record.
    - neighbour -> access keys to cell neighbours
    - state -> state of the cell 
    - row -> x axis coordinate
    - col -> y axis coordinate
    - live_neighbour_count -> number of live neighbours to change cell state
    - time_alive -> number of render loops the cell survived
*)  
type cell_record = {neighbour: int list;
                    state: int; 
                    row: int; 
                    col: int;
                    live_neighbour_count: int list;
                    time_alive : int};;

(*
  Each cell across different CA must satisfy this interface.   
*)
module type CELL = sig

  (* 
    List of functions to determine cell neighbourhood. 
  *)
  val neighbours_fn : ((int -> int) * (int -> int)) list

  (* 
    Function to initialize starting state of a cell. Used in grid assembler. 
  *)
  val initialize_state : unit -> int

  (* 
    Based on cell records, it determines new state of current cell. 
  *)
  val get_new_state : cell_record IntMap.t -> int list -> int -> int list -> int

  (* 
    Logic to determine cell drawing color based on its state. 
  *)
  val draw : int -> color
end;;


module CELL_Moore : CELL = struct 

  let neighbours_fn = 
    [(xm1, xp1);
     (xid, xp1);
     (xp1, xp1);
     (xm1, xid);
     (xm1, xm1);
     (xid, xm1);
     (xp1, xm1);
     (xp1, xid)];;

  let initialize_state () = (Random.int 2);;

  (* 
    Determine live cells in neighbourhood
  *)
  let get_live_count (cell_grid: cell_record IntMap.t) (seq_list: int list) =
    let rec seqscan xs result =
      match xs with
      | [] -> result
      | hd::tl -> if ((IntMap.find hd cell_grid).state = 1)  
                  then seqscan tl (hd :: result) 
                  else seqscan tl result;
    in List.length (seqscan seq_list []);;    

  (* 
    Based on the number of living neighbours, it determines cell next state. 
  *)
  let get_new_state cell_grid neighbors current_state live_neighbour_count = 
    let live_count = ((get_live_count cell_grid neighbors) : int) + current_state in
    let looking_for x = (x = live_count) in
      
    match List.exists looking_for live_neighbour_count with
    | true -> 1
    | false -> 0;;
      
  let draw current_state = 
    match current_state with
    | 0 -> (rgb 30 30 30)
    | 1 -> (rgb 255 255 20)
    | _ -> (rgb 0 0 0);; 
end;;

module CELL_Neumann : CELL = struct 

  let neighbours_fn = 
    [(xm1, xid);
     (xp1, xid);
     (xid, xm1);
     (xid, xp1);];;

  let initialize_state () = (Random.int 2);;

  (* 
    Determine live cells in neighbourhood
  *)
  let get_live_count (cell_grid: cell_record IntMap.t) (seq_list: int list) =
    let rec seqscan xs result =
      match xs with
      | [] -> result
      | hd::tl -> if ((IntMap.find hd cell_grid).state = 1)  
                  then seqscan tl (hd :: result) 
                  else seqscan tl result;
    in List.length (seqscan seq_list []);;    

  (* 
    Based on the number of living neighbours, it determines cell next state. 
  *)
  let get_new_state cell_grid neighbors current_state live_neighbour_count = 
    let live_count = ((get_live_count cell_grid neighbors) : int) + current_state in
    let looking_for x = (x = live_count) in
      
    match List.exists looking_for live_neighbour_count with
    | true -> 1
    | false -> 0;;
      
  let draw current_state = 
    match current_state with
    | 0 -> (rgb 30 30 30)
    | 1 -> (rgb 255 255 20)
    | _ -> (rgb 0 0 0);;  
end;;

module CELL_Game_Of_Life: CELL = struct 

  let neighbours_fn = 
    [(xm1, xp1);
     (xid, xp1);
     (xp1, xp1);
     (xm1, xid);
     (xm1, xm1);
     (xid, xm1);
     (xp1, xm1);
     (xp1, xid)];;

  let initialize_state () = (Random.int 2);;

  (* 
    Determine live cells in neighbourhood
  *)
  let get_live_count (cell_grid: cell_record IntMap.t) (seq_list: int list) =
    let rec seqscan xs result =
      match xs with
      | [] -> result
      | hd::tl -> if ((IntMap.find hd cell_grid).state = 1)  
                  then seqscan tl (hd :: result) 
                  else seqscan tl result;
    in List.length (seqscan seq_list []);;    

  (* 
    Based on the number of living neighbours, it determines cell next state. 
    Special implementation of game of life logic. 
  *)
  let get_new_state cell_grid neighbors current_state _ = 
    let live_count = ((get_live_count cell_grid neighbors) : int)  in

    match current_state with
    |  0 -> begin
                  match live_count with
                    |  3 -> 1
                    | _  -> 0
                end 
    
    |  1 -> begin
                 match live_count with
                   |  3 -> 1
                   |  2 -> 1
                   |  1 -> 0
                   |  0 -> 0
                   | _  -> 0
               end
    | _ -> 0;; 
      
  let draw current_state = 
    match current_state with
    | 0 -> (rgb 30 30 30)
    | 1 -> (rgb 255 255 20)
    | _ -> (rgb 0 0 0);; 
end;;