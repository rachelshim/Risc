# Risc :earth_africa:
Let's get riscy.

Authors: Avani Bhargava (ab2387), Haram Kim (hk592), Sam Ringel (sjr254), Rachel Shim (cs899)

## System description:
A fun, multi-player implementation of the popular board game Risk with a graphical user interface.

### Description:
An OCaml desktop app implementation of online/board game Risk supporting multiplayer play on one computer. Our UI displays an interactive map, game details, controls, and log for the user.

### Key features:
- Fully implemented logic for Risk
- GUI
  - Interactive map
  - User controls
  - Logging
- Multiplayer (single computer) game play

## System design:
We used a Model-View-Controller framework. There are three core modules:
- Model - state.mli
  - Encapsulates the current state of the game, maintains information about the map, user turns, scores and other information.
  - Checks the validity of user actions passed to it from the controller, handling errors by catching them and passing an unmodified state and log messages to the controller
  - Applies changes to produce a new state structure to return to the controller
-  Controller - controller.mli
  - An intermediary module between the state and the GUI.
  - Interprets user actions generated by the GUI, calls appropriate update methods in the state, updates the GUI to reflect state changes, and propagates error information if an action fails.
  - Enforces separation between GUI operations and the State, ensuring that neither directly interact or share internal information.
- View - gui.mli
  - Displays game state (for example, number of troops and owner of each territory) on the map and in the informational panel.
  - Provides user access to gameplay data (available cards, troop counts, etc.).
  - Accepts user input and does some logic before passing to the controller.

## Module design
Our .mli files contain interfaces describing our module design.

## Data
Our program maintains game state data, user action data, and GUI data.

- Game state: as in A2, we used record types to represent the game state cleanly, with a state type, player type, and region type. This includes a variant entitled curr_move, which expresses the current game phase and information relevant to that phase.
- User action data: we defined an Action variant type exposed to all modules. When the GUI receives a user event, its handlers package it into an Action constructor, which the controller passes to the State, and upon receiving an updated state, updates the GUI (also dependent on the Action variant).
- GUI data: The GUI stored several mutable variables that were required to track user input to the GUI in the GTK event-driven context. In addition, it kept track of several GUI data structures, which were necessary to allow the Controller to modify the GUI state. Only setter methods related to these GTK widgets were exposed in the MLI.

An interesting question we faced was how to maintain state information between the separate components we implemented while following an MVC framework. While this would be easy in an imperative language, in a functional language we needed some way to preserve information about updated states as the modules operated during game play. Our approach was: keep our State module purely functional, with functions that operated on and returned states; store a state ref in the GUI (without access to internals, because the state type is not exposed to the GUI) so that even when imperative changes are made, the state can be picked up and passed to the backend to make changes upon user action; use the controller to pass user action information so State can update the state, and pass state change information to update the GUI.

### More GUI implementation details
The GUI was developed in GTK+ 2 using the lablgtk C bindings.
Lablgtk uses OCaml’s imperative and object-oriented components to connect to the underlying GTK library. This required the GUI to be developed using those features. Unfortunately, although an almost purely functional-sytled GUI is theoretically possible in GTK (by re-creating and re-drawing the GUI after every event), the overhead for re-generation would be very high and also out of line with lablgtk’s idioms.
To avoid circular dependencies, imperative functions for setting GUI state are passed to Controller, which uses them to modify the displayed information. We made sure our Model was purely functional.


### External dependencies:
- Lablgtk: https://opam.ocaml.org/packages/lablgtk/
- GTK+2.0 runtime environment (via Apt or Homebrew) - required for  GUI to operate

## Testing:
- Interactive play-testing: we tested interactively by play-testing the game in both a non-malicious average user use case, and deliberately attempting to break the game through illegal actions. A detailed rep_ok function was also run on each state during play-testing.
- Unit testing: used to demonstrate the correctness of our State implementation, as it was the only segment of the program isolated from GUI components and imperative features.
- Utop: we used during development to test State operations during programming.
- Ocamldebug: for specific error cases, we used Ocamldebug to identify the locations of problems and to verify that they were corrected after corrections were made.

### Known problems:
- App is only guaranteed to work on GNOME desktop environments, but may work  on some non-GNOME environments. This is because other desktop systems require GTK to be implemented differently, and can cause display problems for our program.
- Low resolution displays may have difficulty displaying the program GUI in full, due to restrictions on resizing the gameplay widget frame.
- In some earlier design choices, we chose to separate Deployment (in the beginning of the game) and Reinforcement (occurs during each player's turn). If we were to revise our implementation, we would combine the two to avoid user confusion.

## Division of labor:
- Avani worked in a full-stack capacity, implementing the Controller, State functions, improving UX and user-facing logging, and helping to coordinate functionality between the backend and frontend. ~x hours
- Haram worked largely on frontend and testing, implementing the entire GUI, in addition to development environment setup and debugging via ocamldebug. ~x hours
- Sam developed the backend, implementing game initialization, troop deployment, attacking (including all results of an attack), turn ending, logging, and the rep_ok function. He also wrote state test cases. ~x hours
- Rachel developed the backend, implementing x y z. ~x hours

## Screenshots:
Some sample screenshots from GTK on GNOME (Ubuntu 16.04 LTS)

The game window at the start of a 6-player game.
![Six player game start screenshot](screenshots/6_player.png?raw=true)

The game window at the end of a 2-player game.
![Two player game over screenshot](screenshots/game_over.png?raw=true)


The card selection dialog box.
![Card selection screenshot](screenshots/card_sel.png?raw=true)
