open OUnit2
open State
open Action

let two_player = init_state 2
let three_player = init_state 3
let four_player = init_state 4
let five_player = init_state 5
let six_player = init_state 6

let tests =
[
  (* Tests on two-player game *)
  "two_player_init" >:: (fun _ -> assert_equal (player_id (current_player two_player)) "Red");

  (* Tests on two-player game *)
  "two_player_init" >:: (fun _ -> assert_equal (player_id (current_player two_player)) "Red");

]

let suite = "State tests" >::: tests

let _ = run_test_tt_main suite
