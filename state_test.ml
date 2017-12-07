open OUnit2
open State
open Action

let two_player = init_state 2
let three_player = init_state 3
let four_player = init_state 4


let regions = get_regions two_player (* list of all regions in game *)

let test_map2 = update test_map (ADeployment "Alaska")
let () = rep_ok test_map2
let test_map3 = update test_map2 (ADeployment "Ukraine")
let () = rep_ok test_map3
let test_map4 = update test_map3 (AReinforcement ("Greenland", 19))
let () = rep_ok test_map4



let tests =
[
  (* Init tests on two-player game *)
  "tp_init" >:: (fun _ -> assert_equal (rep_ok two_player) ());
  "tp_p1" >:: (fun _ -> assert_equal (player_id (current_player two_player)) "Red");
  "tp_cards1" >::  (fun _ -> assert_equal ((two_player |> current_player |> num_inf) + (two_player |> current_player |> num_cav) + (two_player |> current_player |> num_art) + (two_player |> current_player |> num_wild)) 0);
  "tp_cards2" >::  (fun _ -> assert_equal (
                      let p = player_of_id two_player "Blue" in (num_inf p) + (num_cav p) + (num_art p) + (num_wild p)) 0);
  "tp_troops" >:: (fun _ -> assert_equal (avail_troops two_player) 19);
  "tp_regions" >:: (fun _ -> assert_equal
                      (List.fold_left
                        (fun (p1, p2) r ->
                          if ctrl_of_reg two_player r = "Red"
                          then (p1 + 1, p2)
                          else (p1, p2 + 1))
                        (0, 0) regions) (21, 21));
  "tp_troops_in_r" >:: (fun _ -> assert_equal
                         (List.fold_left
                          (fun b r ->
                            b && troops_in two_player r = 1)
                          true regions) true);
  (* Init tests on three-player game *)
  "trp_init" >:: (fun _ -> assert_equal (rep_ok three_player) ());
  "trp_p1" >:: (fun _ -> assert_equal (player_id (current_player three_player)) "Red");
  "trp_cards1" >::  (fun _ -> assert_equal (
                       let p = player_of_id three_player "Red" in (num_inf p) + (num_cav p) + (num_art p) + (num_wild p)) 0);
  "trp_cards2" >::  (fun _ -> assert_equal (
                       let p = player_of_id three_player "Blue" in (num_inf p) + (num_cav p) + (num_art p) + (num_wild p)) 0);
  "trp_cards3" >::  (fun _ -> assert_equal (
                        let p = player_of_id three_player "Green" in (num_inf p) + (num_cav p) + (num_art p) + (num_wild p)) 0);
  "trp_troops" >:: (fun _ -> assert_equal (avail_troops three_player) 21);
  "trp_regions" >:: (fun _ -> assert_equal
                      (List.fold_left
                        (fun (p1, p2, p3) r ->
                          if ctrl_of_reg three_player r = "Red"
                          then (p1 + 1, p2, p3)
                          else if ctrl_of_reg three_player r = "Blue"
                          then (p1, p2 + 1, p3)
                          else (p1, p2, p3 + 1)) (0, 0, 0)
                        regions) (14, 14, 14));
  "trp_troops_in_r" >:: (fun _ -> assert_equal
                         (List.fold_left
                          (fun b r ->
                            b && troops_in three_player r = 1)
                          true regions) true);
  (* Init tests on four-player game *)
  "fp_init" >:: (fun _ -> assert_equal (rep_ok four_player) ());
  "fp_p1" >:: (fun _ -> assert_equal (player_id (current_player four_player)) "Green");
  "fp_cards1" >::  (fun _ -> assert_equal (
                       let p = player_of_id four_player "Red" in (num_inf p) + (num_cav p) + (num_art p) + (num_wild p)) 0);
  "fp_cards2" >::  (fun _ -> assert_equal (
                       let p = player_of_id four_player "Blue" in (num_inf p) + (num_cav p) + (num_art p) + (num_wild p)) 0);
  "fp_cards3" >::  (fun _ -> assert_equal (
                        let p = player_of_id four_player "Green" in (num_inf p) + (num_cav p) + (num_art p) + (num_wild p)) 0);
  "fp_cards4" >::  (fun _ -> assert_equal (
                        let p = player_of_id four_player "Yellow" in (num_inf p) + (num_cav p) + (num_art p) + (num_wild p)) 0);
  "fp_troops" >:: (fun _ -> assert_equal (avail_troops four_player) 20);
  "fp_regions" >:: (fun _ -> assert_equal
                      (List.fold_left
                        (fun (p1, p2, p3, p4) r ->
                          if ctrl_of_reg four_player r = "Red"
                          then (p1 + 1, p2, p3, p4)
                          else if ctrl_of_reg four_player r = "Blue"
                          then (p1, p2 + 1, p3, p4)
                          else if ctrl_of_reg four_player r = "Green"
                          then (p1, p2, p3 + 1, p4)
                          else(p1, p2, p3, p4 + 1)) (0, 0, 0, 0)
                        regions) (11, 11, 10, 10));
  "fp_troops_in_r" >:: (fun _ -> assert_equal
                         (List.fold_left
                          (fun b r ->
                            b && troops_in four_player r = 1)
                          true regions) true);

  (* Tests with test_map *)

  (* first turn: deployment *)
  (* Red's turn to deploy *)
  "deploy_test_red" >:: (fun _ -> assert_equal ((troops_in test_map "Alaska") + 1) (troops_in test_map2 "Alaska"));
  "num_controlled_test" >:: (fun _ -> assert_equal (current_player test_map2 |> num_controlled) (current_player test_map |> num_controlled));
  "owner_of_cont_test" >:: (fun _ -> assert_equal "Red" (owner_of_cont test_map "Asia"));
  (* ensuring deployment works to all of red's territories *)
  "deploy_test_red_alberta" >:: (fun _ -> assert_equal ((troops_in test_map "Alberta") + 1) (troops_in (update test_map (ADeployment "Alberta")) "Alberta"));
  "deploy_test_red_centamerica" >:: (fun _ -> assert_equal ((troops_in test_map "Central America") + 1) (troops_in (update test_map (ADeployment "Central America")) "Central America"));
  "deploy_test_red_eastus" >:: (fun _ -> assert_equal ((troops_in test_map "Eastern United States") + 1) (troops_in (update test_map (ADeployment "Eastern United States")) "Eastern United States"));
  "deploy_test_red_greenland" >:: (fun _ -> assert_equal ((troops_in test_map "Greenland") + 1) (troops_in (update test_map (ADeployment "Greenland")) "Greenland"));
  "deploy_test_red_nwterr" >:: (fun _ -> assert_equal ((troops_in test_map "Northwest Territory") + 1) (troops_in (update test_map (ADeployment "Northwest Territory")) "Northwest Territory"));
  "deploy_test_red_ontario" >:: (fun _ -> assert_equal ((troops_in test_map "Ontario") + 1) (troops_in (update test_map (ADeployment "Ontario")) "Ontario"));
  "deploy_test_red_quebec" >:: (fun _ -> assert_equal ((troops_in test_map "Quebec") + 1) (troops_in (update test_map (ADeployment "Quebec")) "Quebec"));
  "deploy_test_red_westus" >:: (fun _ -> assert_equal ((troops_in test_map "Western United States") + 1) (troops_in (update test_map (ADeployment "Western United States")) "Western United States"));
  "deploy_test_red_afgh" >:: (fun _ -> assert_equal ((troops_in test_map "Afghanistan") + 1) (troops_in (update test_map (ADeployment "Afghanistan")) "Afghanistan"));
  "deploy_test_red_china" >:: (fun _ -> assert_equal ((troops_in test_map "China") + 1) (troops_in (update test_map (ADeployment "China")) "China"));
  "deploy_test_red_india" >:: (fun _ -> assert_equal ((troops_in test_map "India") + 1) (troops_in (update test_map (ADeployment "India")) "India"));
  "deploy_test_red_irkutsk" >:: (fun _ -> assert_equal ((troops_in test_map "Irkutsk") + 1) (troops_in (update test_map (ADeployment "Irkutsk")) "Irkutsk"));
  "deploy_test_red_japan" >:: (fun _ -> assert_equal ((troops_in test_map "Japan") + 1) (troops_in (update test_map (ADeployment "Japan")) "Japan"));
  "deploy_test_red_kamchatka" >:: (fun _ -> assert_equal ((troops_in test_map "Kamchatka") + 1) (troops_in (update test_map (ADeployment "Kamchatka")) "Kamchatka"));
  "deploy_test_red_mideast" >:: (fun _ -> assert_equal ((troops_in test_map "Middle East") + 1) (troops_in (update test_map (ADeployment "Middle East")) "Middle East"));
  "deploy_test_red_mongolia" >:: (fun _ -> assert_equal ((troops_in test_map "Mongolia") + 1) (troops_in (update test_map (ADeployment "Mongolia")) "Mongolia"));
  "deploy_test_red_siam" >:: (fun _ -> assert_equal ((troops_in test_map "Siam") + 1) (troops_in (update test_map (ADeployment "Siam")) "Siam"));
  "deploy_test_red_siberia" >:: (fun _ -> assert_equal ((troops_in test_map "Siberia") + 1) (troops_in (update test_map (ADeployment "Siberia")) "Siberia"));
  "deploy_test_red_ural" >:: (fun _ -> assert_equal ((troops_in test_map "Ural") + 1) (troops_in (update test_map (ADeployment "Ural")) "Ural"));
  "deploy_test_red_yakutsk" >:: (fun _ -> assert_equal ((troops_in test_map "Yakutsk") + 1) (troops_in (update test_map (ADeployment "Yakutsk")) "Yakutsk"));
  (* Blue's turn to deploy *)
  "deploy_test_blue" >:: (fun _ -> assert_equal ((troops_in test_map2 "Ukraine") + 1) (troops_in test_map3 "Ukraine"));
  "num_controlled_test2" >:: (fun _ -> assert_equal (current_player test_map3 |> num_controlled) (current_player test_map2 |> num_controlled));
  "owner_of_cont_test2" >:: (fun _ -> assert_equal "Blue" (owner_of_cont test_map2 "Europe"));
  (* ensring deployment works to all of blue's territories *)
  "deploy_test_blue_gb" >:: (fun _ -> assert_equal ((troops_in test_map2 "Great Britain") + 1) (troops_in (update test_map2 (ADeployment "Great Britain")) "Great Britain"));
  "deploy_test_blue_iceland" >:: (fun _ -> assert_equal ((troops_in test_map2 "Iceland") + 1) (troops_in (update test_map2 (ADeployment "Iceland")) "Iceland"));
  "deploy_test_blue_noreurope" >:: (fun _ -> assert_equal ((troops_in test_map2 "Northern Europe") + 1) (troops_in (update test_map2 (ADeployment "Northern Europe")) "Northern Europe"));
  "deploy_test_blue_scandi" >:: (fun _ -> assert_equal ((troops_in test_map2 "Scandinavia") + 1) (troops_in (update test_map2 (ADeployment "Scandinavia")) "Scandinavia"));
  "deploy_test_blue_soueurope" >:: (fun _ -> assert_equal ((troops_in test_map2 "Southern Europe") + 1) (troops_in (update test_map2 (ADeployment "Southern Europe")) "Southern Europe"));
  "deploy_test_blue_weseurope" >:: (fun _ -> assert_equal ((troops_in test_map2 "Western Europe") + 1) (troops_in (update test_map2 (ADeployment "Western Europe")) "Western Europe"));
  "deploy_test_blue_congo" >:: (fun _ -> assert_equal ((troops_in test_map2 "Congo") + 1) (troops_in (update test_map2 (ADeployment "Congo")) "Congo"));
  "deploy_test_blue_easafrica" >:: (fun _ -> assert_equal ((troops_in test_map2 "East Africa") + 1) (troops_in (update test_map2 (ADeployment "East Africa")) "East Africa"));
 	"deploy_test_blue_egypt" >:: (fun _ -> assert_equal ((troops_in test_map2 "Egypt") + 1) (troops_in (update test_map2 (ADeployment "Egypt")) "Egypt"));
	"deploy_test_blue_madagas" >:: (fun _ -> assert_equal ((troops_in test_map2 "Madagascar") + 1) (troops_in (update test_map2 (ADeployment "Madagascar")) "Madagascar"));
	"deploy_test_blue_norafrica" >:: (fun _ -> assert_equal ((troops_in test_map2 "North Africa") + 1) (troops_in (update test_map2 (ADeployment "North Africa")) "North Africa"));
	"deploy_test_blue_souafrica" >:: (fun _ -> assert_equal ((troops_in test_map2 "South Africa") + 1) (troops_in (update test_map2 (ADeployment "South Africa")) "South Africa"));
	"deploy_test_blue_easaust" >:: (fun _ -> assert_equal ((troops_in test_map2 "Eastern Australia") + 1) (troops_in (update test_map2 (ADeployment "Eastern Australia")) "Eastern Australia"));
	"deploy_test_blue_indonesia" >:: (fun _ -> assert_equal ((troops_in test_map2 "Indonesia") + 1) (troops_in (update test_map2 (ADeployment "Indonesia")) "Indonesia"));
	"deploy_test_blue_newguin" >:: (fun _ -> assert_equal ((troops_in test_map2 "New Guinea") + 1) (troops_in (update test_map2 (ADeployment "New Guinea")) "New Guinea"));
  "deploy_test_blue_wesaust" >:: (fun _ -> assert_equal ((troops_in test_map2 "Western Australia") + 1) (troops_in (update test_map2 (ADeployment "Western Australia")) "Western Australia"));
  "deploy_test_blue_argentina" >:: (fun _ -> assert_equal ((troops_in test_map2 "Argentina") + 1) (troops_in (update test_map2 (ADeployment "Argentina")) "Argentina"));
  "deploy_test_blue_brazil" >:: (fun _ -> assert_equal ((troops_in test_map2 "Brazil") + 1) (troops_in (update test_map2 (ADeployment "Brazil")) "Brazil"));
  "deploy_test_blue_peru" >:: (fun _ -> assert_equal ((troops_in test_map2 "Peru") + 1) (troops_in (update test_map2 (ADeployment "Peru")) "Peru"));
  "deploy_test_blue_venez" >:: (fun _ -> assert_equal ((troops_in test_map2 "Venezuela") + 1) (troops_in (update test_map2 (ADeployment "Venezuela")) "Venezuela"));
  (* ensuring deployment to unowned territory fails *)
  "deploy_test_red_gb" >:: (fun _ -> assert_equal (troops_in test_map "Great Britain") (troops_in (update test_map (ADeployment "Great Britain")) "Great Britain"));
  "deploy_test_blue_alaska" >:: (fun _ -> assert_equal (troops_in test_map2 "Alaska") (troops_in (update test_map2 (ADeployment "Alaska")) "Alaska"));
  (* ensuring deployment to a territory does not change a different territory's # of troops *)
  "deploy_unchanged_peru" >:: (fun _ -> assert_equal (troops_in test_map "Peru") (troops_in (update test_map (ADeployment "Greenland")) "Peru"));
  "deploy_unchanged_alberta" >:: (fun _ -> assert_equal (troops_in test_map2 "Alberta") (troops_in (update test_map2 (ADeployment "Brazil")) "Alberta"));

  (* reinforcement *)
  (* Red reinforcing *)
  "reinforce_all_greenland" >:: (fun _ -> assert_equal ((troops_in test_map3 "Greenland") + 19) (troops_in test_map4 "Greenland"));
  "reinforce_one_greenland" >:: (fun _ -> assert_equal ((troops_in test_map3 "Greenland") + 1) (troops_in (update test_map3 (AReinforcement ("Greenland", 1))) "Greenland"));
  (* ensure reinforce works for all territories *)
  "reinforce_one_alaska" >:: (fun _ -> assert_equal ((troops_in test_map3 "Alaska") + 1) (troops_in (update test_map3 (AReinforcement ("Alaska", 1))) "Alaska"));
  "reinforce_one_alberta" >:: (fun _ -> assert_equal ((troops_in test_map3 "Alberta") + 1) (troops_in (update test_map3 (AReinforcement ("Alberta", 1))) "Alberta"));
  "reinforce_one_centamer" >:: (fun _ -> assert_equal ((troops_in test_map3 "Central America") + 1) (troops_in (update test_map3 (AReinforcement ("Central America", 1))) "Central America"));
  (* ensure reinforcement fails with more than # of troops allowed *)
  "reinforce_fail" >:: (fun _ -> assert_equal (troops_in test_map3 "Ontario") (troops_in (update test_map3 (AReinforcement ("Ontario", 25))) "Ontario"));
  (* ensure reinforcement of a territory does not change the # of troops on a different territory *)
  "reinforce_unchanged_quebec" >:: (fun _ -> assert_equal (troops_in test_map3 "Quebec") (troops_in (update test_map3 (AReinforcement ("Ontario", 1))) "Quebec"));
  (* ensure reinforcement to unowned territory fails *)
  "reinforce_blue_egypt" >:: (fun _ -> assert_equal (troops_in test_map3 "Egypt") (troops_in (update test_map3 (AReinforcement ("Egypt", 1))) "Egypt"));

  (* trading in cards *)
  "trade_all_inf" >:: (fun _ -> assert_equal 0 (update test_map3 (APlayCards (Infantry, Infantry, Infantry)) |> current_player |> num_inf));
  "trade_first_wild" >:: (fun _ -> assert_equal 1 (update test_map3 (APlayCards (Wild, Infantry, Infantry)) |> current_player |> num_inf));
  "trade_fail_iaa" >:: (fun _ -> assert_equal 3 (update test_map3 (APlayCards (Infantry, Artillery, Artillery)) |> current_player |> num_inf));
  "trade_fail_www" >:: (fun _ -> assert_equal 1 (update test_map3 (APlayCards (Wild, Wild, Wild)) |> current_player |> num_wild));

  (* note: due to the randomized nature of attacking, we decide to not run unit tests for it *)

  (* movement *)
  "movement_centameri_mideast1" >:: (fun _ -> assert_equal ((troops_in test_map4 "Middle East") + 1) (troops_in (update test_map4 (AMovement (("Central America", "Middle East"), 1))) "Middle East"));
  "movement_centameri_mideast2" >:: (fun _ -> assert_equal ((troops_in test_map4 "Central America") - 1) (troops_in (update test_map4 (AMovement (("Central America", "Middle East"), 1))) "Central America"));
  (* ensure unsuccessful movement does not change # of troops *)
  "movement_fail_japan_iceland1" >:: (fun _ -> assert_equal (troops_in test_map4 "Iceland") (troops_in (update test_map4 (AMovement (("Japan", "Iceland"), 1))) "Iceland"));
  "movement_fail_japan_iceland2" >:: (fun _ -> assert_equal (troops_in test_map4 "Japan") (troops_in (update test_map4 (AMovement (("Japan", "Iceland"), 1))) "Japan"));
  "movement_fail_brazil_china" >:: (fun _ -> assert_equal (troops_in test_map4 "China") (troops_in (update test_map4 (AMovement (("Brazil", "China"), 1))) "China"));
  "movement_fail_brazil_china" >:: (fun _ -> assert_equal (troops_in test_map4 "Brazil") (troops_in (update test_map4 (AMovement (("Brazil", "China"), 1))) "Brazil"));

  (* end turn *)
  "next_turn_blue" >:: (fun _ -> assert_equal "Blue" (update (update test_map4 ANextTurn) ANextTurn |> current_player |> player_id));
]

let suite = "State tests" >::: tests

let _ = run_test_tt_main suite
