% Main menu

cool_menu_art :-
    write('___________      .__ __         '), nl,
    write('\\__    ___/______|__|  | __ ____ '), nl,
    write('  |    |  \\_  __ \\  |  |/ // __ \\ '), nl,
    write('  |    |   |  | \\/  |    <\\  ___/ '), nl,
    write('  |____|   |__|  |__|__|_ \\\\___  >'), nl,
    write('                         \\/    \\/ '), nl,
    nl.

main_menu :-
    cool_menu_art,
    nl,
    write('WELCOME TO TRIKE'), nl,
    write('-----------------'), nl,
    write('1. Play Game'), nl,
    write('2. Quit'), nl,
    nl,
    read_menu_option(Option),
    handle_menu_option(Option).

% Read user's menu choice
read_menu_option(Option) :-
    write('Enter your choice (1-2): '),
    read(Option).

% Handle user's menu choice
handle_menu_option(1) :-
    play_game_options.
handle_menu_option(2) :-
    nl,
    write('Goodbye!'), nl.

% Options after choosing to play the game
play_game_options :-
    nl,
    write('Choose Game Mode:'), nl,
    write('1. Person vs Person'), nl,
    write('2. Person vs Computer'), nl,
    write('3. Computer vs Computer'), nl,
    nl,
    read_menu_option(GameOption),
    handle_play_game_option(GameOption).

% Handle options after choosing to play the game
handle_play_game_option(1) :-
    % Person vs Person
    initial_state(7, GameState),
    display_game(GameState),
    % Add logic for Person vs Person here.
    nl.
handle_play_game_option(2) :-
    % Person vs Computer
    initial_state(7, GameState),
    display_game(GameState),
    % Add logic for Person vs Computer here.
    nl.
handle_play_game_option(3) :-
    % Computer vs Computer
    initial_state(7, GameState),
    display_game(GameState),
    % Add logic for Computer vs Computer here.
    nl.
