type action =
| Deploy of string
| Reinforce of string * int
| Move of (string * string) * int
| Attack of string * string
| Trade_cards_different
| Trade_cards_same 
| End_turn
