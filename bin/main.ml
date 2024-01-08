open Graphics;;
open Renderer;;
open Grid;;
open Helpers;;

let get_help_text  =
    let helpframe_top =
    "\n********************************************************************************" in
    let helpinfo = "\n2D cellular automata. \n\n Cells in a grid live or die depending upon the states of surrounding cells. End program by any key press." in
    let helpinfo2 = "\n\nYou may choose your own values for the size of the grid (span) \nand the size of individual cells (zoom), or just use one of the provided presets." in
    let helpinfo3 = "\n\nYou can also choos seed size which determines how many cells in the middle of the screen will be alive at the start of the simulation. If 0, random initialization of cells will follow.\n" in
    let helpinfo4 = "\n\n You can set variation of cell neighbourhood by setting -module to Moore, Neumann or Game_Of_Life. Number of living neighbours for cell to live, is determined by -live_count command.\n" in
    
    let help_example1 =
    "\n-------\n Example 1: use the 'fast' configuration with live count 1 and 2(if there is one or two living neighbours, cell will survive)\n dune exec -- CA -fast -live_count 1 2 0 0 0 0 0 0 \n " in
    let help_example2 =
    "\n Example 2: use a custom span and zoom, with the default color scheme \n dune exec -- CA -s 180 -z 2 \n" in
    let help_example3 =
    "\n Example 3: use seed grid \n dune exec -- CA -seed_size 10 \n" in

    helpframe_top ^ helpinfo  ^ helpinfo2 ^ helpinfo3 ^ helpinfo4 ^ help_example1 ^ help_example2 ^ help_example3 ^ "\n-------\n Usage and Options: "
;;

let speclist = [( "-regular", Arg.Unit (set_mode_fn 3 200), "(Preset, with span: 200 and zoom: 3)\n");
                ( "-fast", Arg.Unit  (set_mode_fn 6 120), "(Preset, with span: 120 and zoom: 6)\n");
                ( "-large", Arg.Unit (set_mode_fn 2 300), "(Preset, with span: 300 and zoom: 2)\n");
                ( "-larger", Arg.Unit (set_mode_fn 2 600), "(Preset, with span: 600 and zoom: 2)\n");
                ( "-ultimate", Arg.Unit (set_mode_fn 1 1000), "(Preset, with span: 1000 and zoom: 1)\n");
                ( "-s", Arg.Int (set_span), "Set custom span size.\n");
                ( "-z", Arg.Int (set_zoom), "Set custom zoom value.\n");
                ( "-seed_size", Arg.Int (set_seed_size), "Set seed size. If 0, random initialization of cells will follow.\n");
                ( "-module", Arg.String (set_ca_module), "Set module type. Currently possible options are Moore, Neumann or Game_Of_Life. Default is Game_Of_Life.\n");
                ( "-live_count", Arg.Tuple [Arg.Int add_live_command; (* Ugly solution! *)
                                            Arg.Int add_live_command;
                                            Arg.Int add_live_command; 
                                            Arg.Int add_live_command;
                                            Arg.Int add_live_command; 
                                            Arg.Int add_live_command;
                                            Arg.Int add_live_command; 
                                            Arg.Int add_live_command], "Set number of neighbours for cell to live. There must be eight values. Set to 0 for value to skip. Default is game of life (3, 4).\n")];;

(* Global definition of a cell grid. Dirty way of handling starting grids. *)
let cell_grid = ref IntMap.empty;;

let default_grid () = 
  let grid_assembler = get_default_grid_assembler !run_env.n_cells !run_env.live_command !run_env.ca_module in
  cell_grid := grid_assembler IntMap.empty 0 1 1;;

let seed_grid () = 
  let grid_assembler = get_empty_grid_assembler !run_env.n_cells !run_env.live_command !run_env.ca_module in
  let cell_grid_aux = grid_assembler IntMap.empty 0 1 1 in
  
  let fill_seed = get_fill_seed !run_env.n_cells !run_env.seed_size 1 !run_env.live_command !run_env.ca_module in
  cell_grid := fill_seed cell_grid_aux !run_env.seed_size 0;;

let simulation_loop reps curr_cell_grid ca_module =
  let env = !run_env in
  let renderer = (get_fill_plotter env ca_module) in 
  
  let rec simulation_loop_aux ticks cell_grid =
    IntMap.iter renderer cell_grid;  
    synchronize ();

    if (key_pressed () = false) && (ticks < reps) 
      then simulation_loop_aux (ticks + 1) (IntMap.map (update_grid_state cell_grid ca_module) cell_grid)
      else ticks in 

  simulation_loop_aux 0 curr_cell_grid;;

let () =
  Arg.parse speclist (fun _ -> ()) (get_help_text) ;
  init_graphics !run_env;

  if !run_env.seed_size = 0 
    then default_grid ()
    else seed_grid ();;

  let completed_reps = (simulation_loop max_int !cell_grid !run_env.ca_module) in
  print_string("\n\n generations: " ^ string_of_int completed_reps);
  read_line ();;