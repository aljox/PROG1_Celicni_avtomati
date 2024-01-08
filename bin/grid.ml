open Cell;;
open Helpers;;

(* 
  Check if value is in bounds
*)
let inBounds (a : int) (low_bound : int) (high_bound : int) = 
  (a > low_bound) && (a < high_bound) ;;

(* 
  Check which neighbour cells are valid. For valid cells their index is returned, otherwise it is set to 0.
*) 
let  getSN n_cells row col fa fb =
  (* Select neighbour cell *)
  let new_row = fa row in
  let new_col = fb col in
  
  (* Check if neighbour is in bounds *)
  if (inBounds new_row 0 (n_cells + 1)) && (inBounds new_col 0 (n_cells + 1)) 
  then new_row + (new_col - 1) * n_cells 
  else 0;;

(* 
  Extract valid neighbours of a cell based on neighbour functions 
*)
let get_neighbors row col n_cells neighbour_fn =
  (List.filter (fun a -> a > 0) (* Select only valid cells *)
               (map (fun k -> getSN n_cells row col (fst k) (snd k)) neighbour_fn));;

(* 
  Function to assamble initial grid of cells.
  Cells are set based on cell module that is used.
*)
let get_default_grid_assembler n_cells live_count ca_module = 
  let module CA_module_unpacked = (val ca_module : CELL) in
  let rec assembler box nrow ncol ticks =
    if nrow = n_cells then
      begin
        if ncol = n_cells 
          then box
          else assembler 
                (IntMap.add ticks 
                            {neighbour = (get_neighbors 1 (ncol + 1) n_cells CA_module_unpacked.neighbours_fn);
                              state = (CA_module_unpacked.initialize_state ()); 
                              row = 1; 
                              col = (ncol + 1);
                              live_neighbour_count = live_count;
                              time_alive = 0} 
                            box) 
                  1 
                  (ncol + 1) 
                  (ticks + 1)
        end
    else assembler 
          (IntMap.add ticks 
                      {neighbour = (get_neighbors (nrow + 1) ncol n_cells CA_module_unpacked.neighbours_fn); 
                        state = (CA_module_unpacked.initialize_state ());
                        row = (nrow + 1);
                        col = ncol;
                        live_neighbour_count = live_count;
                        time_alive = 0}
                      box)
          (nrow+1) 
          ncol 
          (ticks+1) in 
  assembler;;

(* 
  Function to assamble initial grid of cells.
  Cells are initialized in 0 state.
*)
let get_empty_grid_assembler n_cells live_count ca_module =
  let module CA_module_unpacked = (val ca_module : CELL) in
  let rec assembler box nrow ncol ticks =
    if nrow = n_cells then
      begin
        if ncol = n_cells 
          then box
          else assembler 
                (IntMap.add ticks 
                            {neighbour = (get_neighbors 1 (ncol + 1) n_cells CA_module_unpacked.neighbours_fn);
                              state = 0; 
                              row = 1; 
                              col = (ncol + 1);
                              live_neighbour_count = live_count;
                              time_alive = 0} 
                            box) 
                  1 
                  (ncol + 1) 
                  (ticks + 1)
        end
    else assembler 
          (IntMap.add ticks 
                      {neighbour = (get_neighbors (nrow + 1) ncol n_cells CA_module_unpacked.neighbours_fn); 
                        state = 0;
                        row = (nrow + 1);
                        col = ncol;
                        live_neighbour_count = live_count;
                        time_alive = 0}
                      box)
          (nrow+1) 
          ncol 
          (ticks+1) in 
  assembler;;

(* 
  Fill center row of a grid with seed value.
*)
let get_fill_seed n_cells seed_size seed_value live_count ca_module =
  let module CA_module_unpacked = (val ca_module : CELL) in
  let half_cells = n_cells / 2 in
  let half_seed_size = seed_size / 2 in

  let rec seed_assembler box seed_size ticks =
    if ticks < seed_size 
      then seed_assembler 
            (IntMap.add ((half_cells - half_seed_size + ticks) + (half_cells - 1) * n_cells)
                        {neighbour = (get_neighbors (half_cells - half_seed_size + ticks) half_cells n_cells CA_module_unpacked.neighbours_fn);
                          state = seed_value; 
                          row= (half_cells - half_seed_size + ticks); 
                          col= half_cells;
                          live_neighbour_count = live_count;
                          time_alive = 0} 
                        box) 
              seed_size 
              (ticks + 1)
      else box in
    seed_assembler;; 

(* 
  Update cell state and its time alive.
*)
let update_grid_state xs ca_module = 
  let module CA_module_unpacked = (val ca_module : CELL) in
  (fun c -> 
    let new_state = (CA_module_unpacked.get_new_state xs c.neighbour c.state c.live_neighbour_count) in 
    if new_state = 0 
      then {neighbour = c.neighbour; 
            state = new_state; 
            row = c.row; 
            col = c.col;
            live_neighbour_count = c.live_neighbour_count;
            time_alive = 0}
      else {neighbour = c.neighbour; 
            state = new_state; 
            row = c.row; 
            col = c.col;
            live_neighbour_count = c.live_neighbour_count;
            time_alive = c.time_alive + 1});;