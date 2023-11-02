:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).
:- use_module(library(system)).

% Main game loop for computer vs computer
computer_vs_computer_game(DifficultyOption1,DifficultyOption2) :-
    create_matrix(7, Matrix),
    
    random_initial_move(Row, Column),
    update_matrix(Matrix, Row, Column, 'B', UpdatedMatrix),

    % Start the game loop
    computer_vs_computer_game_loop(UpdatedMatrix, white,DifficultyOption1,DifficultyOption2).




% Main game loop for computer vs computer
computer_vs_computer_game_loop(Matrix, CurrentPlayer, DifficultyOption1, DifficultyOption2) :-
    (
        CurrentPlayer = black 
    -> 
        (
            % Check the difficulty level and call the appropriate computer move predicate for the first computer
            (DifficultyOption2 == 1 -> computer_make_move(Matrix, CurrentPlayer, UpdatedMatrix, NextPlayer);
             DifficultyOption2 == 2 -> computer_make_move2(Matrix, CurrentPlayer, UpdatedMatrix, NextPlayer)),
            write('Black Computer''s Turn'),
            sleep(2),
            computer_vs_computer_game_loop(UpdatedMatrix, NextPlayer, DifficultyOption1, DifficultyOption2);
            game_over(Matrix, CurrentPlayer), !
        )
    ;   
        (
            % Check the difficulty level and call the appropriate computer move predicate for the second computer
            (DifficultyOption1 == 1 -> computer_make_move(Matrix, CurrentPlayer, UpdatedMatrix, NextPlayer);
             DifficultyOption1 == 2 -> computer_make_move2(Matrix, CurrentPlayer, UpdatedMatrix, NextPlayer)),
            write('White Computer''s Turn'),
            sleep(2),
            computer_vs_computer_game_loop(UpdatedMatrix, NextPlayer, DifficultyOption1, DifficultyOption2);
            game_over(Matrix, CurrentPlayer), !
        )
    ).


    % Main game loop for player vs player
player_vs_player_game :-
    create_matrix(7, Matrix),
    
    % Prompt black player for initial move
    write('Black, enter the row for your initial move: '),
    read(BlackRow),
    write('Enter the column for your initial move: '),
    read(BlackColumn),
    update_matrix(Matrix, BlackRow, BlackColumn, 'B', UpdatedMatrix),

    % Start the game loop
    player_vs_player_game_loop(UpdatedMatrix, white).

player_vs_player_game_loop(Matrix, CurrentPlayer) :-
    (   make_move(Matrix, CurrentPlayer, UpdatedMatrix, NextPlayer) ->
        player_vs_player_game_loop(UpdatedMatrix, NextPlayer)  % If make_move succeeds, recurse with updated state.
    ;   game_over(Matrix, CurrentPlayer), !  % If make_move fails, end the game.
    ).

% Main game loop for computer vs player
computer_vs_player_game(DifficultyOption) :-
    create_matrix(7, Matrix),
    
    random_initial_move(Row, Column),
    update_matrix(Matrix, Row, Column, 'B', UpdatedMatrix),

    % Start the game loop
    computer_vs_player_game_loop(UpdatedMatrix, white,DifficultyOption).

% Game loop for computer vs player
computer_vs_player_game_loop(Matrix, CurrentPlayer, DifficultyOption) :-
    (   CurrentPlayer = black 
    ->  (
            % Check the difficulty level and call the appropriate computer move predicate
            (DifficultyOption == 1 -> computer_make_move(Matrix, CurrentPlayer, UpdatedMatrix, NextPlayer);
             DifficultyOption == 2 -> computer_make_move2(Matrix, CurrentPlayer, UpdatedMatrix, NextPlayer)),
            write('Computer''s Turn'),
            sleep(2),
            computer_vs_player_game_loop(UpdatedMatrix, NextPlayer, DifficultyOption);
            game_over(Matrix, CurrentPlayer), !
        )
    ;   (
            make_move(Matrix, CurrentPlayer, UpdatedMatrix, NextPlayer) ->
            computer_vs_player_game_loop(UpdatedMatrix, NextPlayer, DifficultyOption)
        ;   game_over(Matrix, CurrentPlayer), !
    )
    ).




% Main game loop for player v computer
player_vs_computer_game(DifficultyOption) :-
    create_matrix(7, Matrix),
    
    % Prompt black player for initial move
    write('Black, enter the row for your initial move: '),
    read(BlackRow),
    write('Enter the column for your initial move: '),
    read(BlackColumn),
    update_matrix(Matrix, BlackRow, BlackColumn, 'B', UpdatedMatrix),

    % Start the game loop
    player_vs_computer_game_loop(UpdatedMatrix, white,DifficultyOption).

% Game loop for computer vs player
player_vs_computer_game_loop(Matrix, CurrentPlayer, DifficultyOption) :-
    (   CurrentPlayer = black 
    ->  (
         make_move(Matrix, CurrentPlayer, UpdatedMatrix, NextPlayer) ->
            player_vs_computer_game_loop(UpdatedMatrix, NextPlayer, DifficultyOption)
        ;   game_over(Matrix, CurrentPlayer), !
            
        )
    ;   (
           % Check the difficulty level and call the appropriate computer move predicate
            (DifficultyOption == 1 -> computer_make_move(Matrix, CurrentPlayer, UpdatedMatrix, NextPlayer);
             DifficultyOption == 2 -> computer_make_move2(Matrix, CurrentPlayer, UpdatedMatrix, NextPlayer)),
            write('Computer''s Turn'),
            sleep(2),
            player_vs_computer_game_loop(UpdatedMatrix, NextPlayer, DifficultyOption);
            game_over(Matrix, CurrentPlayer), !
    )
    ).