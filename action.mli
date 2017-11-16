type action =
| Reinforce of region * int
| Attack of region * region
| Fortify of (region * region) * int
