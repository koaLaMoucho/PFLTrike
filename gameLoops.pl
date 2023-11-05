

% Main game loop for computer vs computer
computer_vs_computer_game(DifficultyOption1,DifficultyOption2) :-
    create_matrix(7, Matrix),
    
    random_initial_move(Row, Column),
    update_matrix(Matrix, Row, Column, 'B', UpdatedMatrix),

    % Start the game loop
    computer_vs_computer_game_loop(UpdatedMatrix, white,DifficultyOption1,DifficultyOption2).




% Game loop for computer vs computer based on CurrentPlayer
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
    
    % Prompt first player for color
    write('First player, do you want to be white or black? (white/black): '),
    read(FirstPlayerColor),
    skip_line,
    (   FirstPlayerColor = white ->  % If first player is white, second player is black
        SecondPlayerColor = black
    ;   SecondPlayerColor = white  % If first player is black, second player is white
    ),

    % Prompt first player for initial move
    write('First player, enter the row for your initial move: '),
    read(Row),
    skip_line,
    write('Enter the column for your initial move: '),
    read(Column),
    skip_line,

    % Prompt second player for initial move
    write('Second Player, do you want to switch sides? (y/n): '),
    read(SwitchSides),
    skip_line,
    write('SwitchSides: '), write(SwitchSides), nl,

    % Start the game loop
    (   SwitchSides = y ->  % If second player wants to switch sides, switch the colors
        current_player_symbol(SecondPlayerColor, Symbol),
        update_matrix(Matrix, Row, Column, Symbol, UpdatedMatrix),
        player_vs_player_game_loop(UpdatedMatrix, FirstPlayerColor)
    ;   current_player_symbol(FirstPlayerColor, Symbol),
        update_matrix(Matrix, Row, Column, Symbol, UpdatedMatrix),
        player_vs_player_game_loop(UpdatedMatrix, SecondPlayerColor)
    ).
    

% Game loop for player vs player based on CurrentPlayer
player_vs_player_game_loop(Matrix, CurrentPlayer) :-
    (   make_move(Matrix, CurrentPlayer, UpdatedMatrix, NextPlayer) ->
        player_vs_player_game_loop(UpdatedMatrix, NextPlayer)  % If make_move succeeds, recurse with updated state.
    ;   game_over(Matrix, CurrentPlayer), !  % If make_move fails, end the game.
    ).

% Main game loop for computer vs player
computer_vs_player_game(DifficultyOption) :-
    create_matrix(7, Matrix),
    
    random_initial_move(Row, Column),

    % Computer is black and player is white
    FirstPlayerColor = black,
    SecondPlayerColor = white,

    % Prompt second player for initial move
    write('Second Player, do you want to switch sides? (y/n): '),
    read(SwitchSides),
    skip_line,
    write('SwitchSides: '), write(SwitchSides), nl,

    % Start the game loop
    (   SwitchSides = y ->  % If second player wants to switch sides, switch the colors
        current_player_symbol(SecondPlayerColor, Symbol),
        update_matrix(Matrix, Row, Column, Symbol, UpdatedMatrix),
        computer_vs_player_game_loop(UpdatedMatrix, FirstPlayerColor, DifficultyOption)
    ;   current_player_symbol(FirstPlayerColor, Symbol),
        update_matrix(Matrix, Row, Column, Symbol, UpdatedMatrix),
        computer_vs_player_game_loop(UpdatedMatrix, SecondPlayerColor, DifficultyOption)
    ).

    /*
    update_matrix(Matrix, Row, Column, 'B', UpdatedMatrix),

    % Start the game loop
    computer_vs_player_game_loop(UpdatedMatrix, white,DifficultyOption).
    */

% Game loop for computer vs player based on CurrentPlayer
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
    skip_line,
    write('Enter the column for your initial move: '),
    read(BlackColumn),
    skip_line,
    update_matrix(Matrix, BlackRow, BlackColumn, 'B', UpdatedMatrix),

    % Start the game loop
    player_vs_computer_game_loop(UpdatedMatrix, white,DifficultyOption).



% Game loop for computer vs player based on CurrentPlayer
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