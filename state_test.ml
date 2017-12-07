open OUnit2
open State
open Action

let tests = 
[
	"format" >:: (fun _ -> assert_equal "jej" ("jej"));
]

let suite = "State tests" >::: tests

let _ = run_test_tt_main suite