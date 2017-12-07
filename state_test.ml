open OUnit2
open State
open Action

let tests =
[
  "format" >:: (fun _ -> assert_equal "jej" ("jej"));

  (** Initialize state *)
  "init_state" >:: (fun _ -> assert_equal
                       (player_id (current_player (init_state 2)))
                       "Red"
                   );
]

let suite = "State tests" >::: tests

let _ = run_test_tt_main suite
