type action =
| Deploy of string
| Reinforce of string * int
| Move of (string * string) * int
| Attack of string * string
| TradeSame
| TradeDiff
| EndTurn
