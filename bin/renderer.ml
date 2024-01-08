open Graphics;;
open Cell;;

(* 
  Display settings. Final screen resolution is (n_cells * zoom)^2.
    - zoom -> individual cell edge size in pixels
    - n_cells -> number of cells per axis. Results in (n_cells * n_cells) in simulation
    - padding -> boundary padding in simulation
    - bkcolor -> background color
    - ca_module -> cell module to be used during rendering
    - seed_size -> seed_size parameter for seeded grid. 0 uses default grid generation
    - live_command -> number of neighbours needed to be alive for cell to live 
*)
type renderer_desc = {zoom : int; 
                      n_cells : int; 
                      padding : int; 
                      bkcolor : color;
                      ca_module : (module CELL);
                      seed_size : int;
                      live_command : int list};;

(*
  Mutable default renderer description.                         
*)
let run_env = ref {zoom = 3; 
                   n_cells=200; 
                   padding = 5; 
                   bkcolor = (rgb 30 30 30);
                   ca_module = (module CELL_Game_Of_Life : CELL); 
                   seed_size = 0;
                   live_command = [3; 4]};;

let set_span n =
  print_string("\nspan: " ^ (string_of_int n));
  run_env := {zoom = !run_env.zoom; 
              n_cells = n; 
              padding = !run_env.padding; 
              bkcolor = !run_env.bkcolor;
              ca_module = !run_env.ca_module;
              seed_size = !run_env.seed_size;
              live_command = !run_env.live_command};;

let set_zoom n =
  print_string(", zoom: " ^ (string_of_int n));
  run_env := {zoom = n; 
              n_cells = !run_env.n_cells; 
              padding = !run_env.padding;
              bkcolor = !run_env.bkcolor;
              ca_module = !run_env.ca_module;
              seed_size = !run_env.seed_size;
              live_command = !run_env.live_command};;

let set_ca_module ca_module_str =
  match ca_module_str with
  | "Moore" -> run_env := {zoom = !run_env.zoom;
                           n_cells = !run_env.n_cells; 
                           padding = !run_env.padding;
                           bkcolor = !run_env.bkcolor;
                           ca_module = (module CELL_Moore : CELL);
                           seed_size = !run_env.seed_size;
                           live_command = !run_env.live_command}
  | "Neumann" -> run_env := {zoom = !run_env.zoom;
                             n_cells = !run_env.n_cells; 
                             padding = !run_env.padding;
                             bkcolor = !run_env.bkcolor;
                             ca_module = (module CELL_Neumann : CELL);
                             seed_size = !run_env.seed_size;
                             live_command = !run_env.live_command}
  | "Game_Of_Life" -> run_env := {zoom = !run_env.zoom;
                             n_cells = !run_env.n_cells; 
                             padding = !run_env.padding;
                             bkcolor = !run_env.bkcolor;
                             ca_module = (module CELL_Game_Of_Life : CELL);
                             seed_size = !run_env.seed_size;
                             live_command = !run_env.live_command}
  | _ -> print_string "Invalid module. Running default module CELL_Moore!";;

let set_seed_size n =
  print_string(", seed_size: " ^ (string_of_int n));
  run_env := {zoom = !run_env.zoom;
              n_cells = !run_env.n_cells; 
              padding = !run_env.padding;
              bkcolor = !run_env.bkcolor;
              ca_module = !run_env.ca_module;
              seed_size = n;
              live_command = !run_env.live_command};;

let set_mode_fn z m =
  (fun () ->  run_env := {zoom = z; 
                          n_cells = m; 
                          padding = !run_env.padding; 
                          bkcolor = !run_env.bkcolor;
                          ca_module = !run_env.ca_module;
                          seed_size = !run_env.seed_size;
                          live_command = !run_env.live_command});;

let add_live_command command =
  match command with
  | 0 -> run_env := {zoom = !run_env.zoom;
                     n_cells = !run_env.n_cells; 
                     padding = !run_env.padding;
                     bkcolor = !run_env.bkcolor;
                     ca_module = !run_env.ca_module;
                     seed_size = !run_env.seed_size;
                     live_command = !run_env.live_command}
  | _ -> run_env := {zoom = !run_env.zoom;
                    n_cells = !run_env.n_cells; 
                     padding = !run_env.padding;
                     bkcolor = !run_env.bkcolor;
                     ca_module = !run_env.ca_module;
                     seed_size = !run_env.seed_size;
                     live_command = !run_env.live_command @ [command]};;

(*
  ---------------Rendering---------------
*)
let clear_screen env = 
  set_color env.bkcolor;
  fill_rect 0 0 ((2 * env.padding) + (env.zoom * env.n_cells)) 
                ((2 * env.padding) + (env.zoom * env.n_cells));;

let init_graphics env =
  let gridspan = ((2 * env.padding) + (env.zoom * env.n_cells)) in

  open_graph(" " ^ (string_of_int gridspan) ^ "x" ^ (string_of_int gridspan));
  auto_synchronize false;
  set_window_title "Celluar automata";
  clear_screen env ;
  at_exit (fun () -> close_graph ());;

(* 
  Returns a cell rendering function with the current game.
*)
let get_fill_plotter env ca_module =
  let module CA_module_unpacked = (val ca_module : CELL) in
  (fun _ cell_rec -> begin
                     set_color (CA_module_unpacked.draw cell_rec.state);
                      
                     fill_rect (env.padding + (env.zoom * (cell_rec.row - 1))) 
                               (env.padding + (env.zoom * (cell_rec.col - 1))) 
                               env.zoom 
                               env.zoom  
                     end);;