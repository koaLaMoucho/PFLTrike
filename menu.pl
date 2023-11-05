% Main menu

:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).
:- use_module(library(system)).

:- consult(gameLoops).
:- consult(movesAndHelpers).
:- consult(trikeBoard).


cool_menu_art :-
    write('___________      .__ __         '), nl,
    write('\\__    ___/______|__|  | __ ____ '), nl,
    write('  |    |  \\_  __ \\  |  |/ // __ \\ '), nl,
    write('  |    |   |  | \\/  |    <\\  ___/ '), nl,
    write('  |____|   |__|  |__|__|_ \\\\___  >'), nl,
    write('                         \\/    \\/ '), nl,
    nl.

% Main Menu
play :-
    cool_menu_art,
    nl,
    write('|---------------------------------|'), nl,
    write('|        WELCOME TO TRIKE         |'), nl,
    write('|---------------------------------|'), nl,
    write('           ------------          '), nl,
    write('            1. Play Game'), nl,
    write('           ------------          '), nl,
    write('              2. Quit'), nl,
    write('           ------------          '), nl,
    nl,
    read_menu_option(Option),
    handle_menu_option(Option).

% Read user input
read_menu_option(Option) :-
    write('Enter your choice: '),
    read(NewOption),
    skip_line,
    (   NewOption = 1
    ;   NewOption = 2
    ),
    !, % Prevent backtracking once a valid option has been read
    Option = NewOption.

read_menu_option(Option) :-
    nl,
    write('Invalid option, please try again.'), nl,
    read_menu_option(Option).

handle_menu_option(1) :-
    play_game_options.
handle_menu_option(2) :-
    nl,
    write('Goodbye, Thank you for playing!'), nl.

% Options after choosing to play the game
play_game_options :-
    nl,
    write('|---------------------------------|'), nl,
    write('|        Choose a GameMode        |'), nl,
    write('|---------------------------------|'), nl,
    write('           ------------          '), nl,
    write('       1. Person vs Person'), nl,
    write('           ------------          '), nl,
    write('      2. Person vs Computer'), nl,
    write('           ------------          '), nl,
    write('     3. Computer vs Computer'), nl,
    write('           ------------          '), nl,
    nl,
    read_gamemode_menu_option(GameOption),
    handle_play_game_option(GameOption).

% Read user input
read_gamemode_menu_option(Option) :-
    write('Enter your choice: '),
    read(NewOption),
    skip_line,
    (   NewOption = 1
    ;   NewOption = 2
    ;   NewOption = 3
    ),
    !, % Cut to prevent backtracking after a valid choice
    Option = NewOption.

read_gamemode_menu_option(Option) :-
    nl,
    write('Invalid option, please try again.'), nl,
    read_gamemode_menu_option(Option).

% Handle options after choosing to play the game
handle_play_game_option(1) :-
    % Person vs Person
    player_vs_player_game, % Call the example_update predicate
    nl.

handle_play_game_option(2) :-
    % Person vs Computer
    nl,
    write('|---------------------------------|'), nl,
    write('|       Choose Computer Level     |'), nl,
    write('|---------------------------------|'), nl,
    write('           ------------          '), nl,
    write('       1. Easy (Random)'), nl,
    write('           ------------          '), nl,
    write('      2. Hard (Greedy)'), nl,
    write('           ------------          '), nl,
    nl,
    read_menu_option(DifficultyOption),

    nl,
    write('|---------------------------------|'), nl,
    write('|    Choose Who Starts First      |'), nl,
    write('|---------------------------------|'), nl,
    write('           ------------          '), nl,
    write('            1. Player'), nl,
    write('           ------------          '), nl,
    write('           2. Computer'), nl,
    write('           ------------          '), nl,
    nl,
    read_menu_option(StartOption),

    (StartOption == 1 ->
        player_vs_computer_game(DifficultyOption)
    ;   computer_vs_player_game(DifficultyOption)
    ),
    nl.


handle_play_game_option(3) :-
    nl,
    write('|---------------------------------|'), nl,
    write('|   Choose Computer Levels (1-2)  |'), nl,
    write('|---------------------------------|'), nl,
    write('           ------------          '), nl,
    write('   1. White Computer (Random)'), nl,
    write('           ------------          '), nl,
    write('   2. White Computer (Greedy)'), nl,
    write('           ------------          '), nl,
    nl,
    read_menu_option(DifficultyOption1),
    nl,
    write('|---------------------------------|'), nl,
    write('|   Choose Computer Levels (1-2)  |'), nl,
    write('|---------------------------------|'), nl,
    write('           ------------          '), nl,
    write('   1. Black Computer (Random)'), nl,
    write('           ------------          '), nl,
    write('   2. Black Computer (Greedy)'), nl,
    write('           ------------          '), nl,
    nl,
    read_menu_option(DifficultyOption2),
    computer_vs_computer_game(DifficultyOption1, DifficultyOption2),
    
    nl.