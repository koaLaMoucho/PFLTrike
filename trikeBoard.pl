:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).
:- use_module(library(system)).

% Updated create_matrix/2 to create a matrix with variables
create_matrix(Size, Matrix) :-
    create_matrix(Size, 1, Matrix).

create_matrix(Size, Size, [Row]) :-
    row_of_vars(Size, Row).

create_matrix(Size, N, [Row | Rest]) :-
    N < Size,
    row_of_vars(N, Row),
    NextN is N + 1,
    create_matrix(Size, NextN, Rest).

row_of_vars(0, []).
row_of_vars(N, [Var | Rest]) :-
    N > 0,
    NextN is N - 1,
    row_of_vars(NextN, Rest),
    Var = 'X'. % You can use any variable here

% Update a value at a specific position in the matrix with move restrictions
update_matrix(Matrix, Row, Col, NewValue, UpdatedMatrix) :-
    % Keep track of the last row and last column
    last_move(LastRow, LastCol),

    % Add move restrictions here
    (valid_move(Row, Col, LastRow, LastCol, Matrix) ; throw(error('Invalid move'))),

    update_row(Matrix, Row, Col, NewValue, UpdatedMatrix),

    % Update the last row and last column
    retractall(last_move(_, _)),
    asserta(last_move(Row, Col)).

current_player_symbol(white, 'W').
current_player_symbol(black, 'B').

update_row([CurrentRow | Rest], 1, Col, NewValue, [UpdatedRow | Rest]) :-
    update_column(CurrentRow, Col, NewValue, UpdatedRow).
update_row([CurrentRow | Rest], Row, Col, NewValue, [CurrentRow | UpdatedRest]) :-
    Row > 1,
    NextRow is Row - 1,
    update_row(Rest, NextRow, Col, NewValue, UpdatedRest).

update_column([_ | Rest], 1, NewValue, [NewValue | Rest]).
update_column([Current | Rest], Col, NewValue, [Current | UpdatedRest]) :-
    Col > 1,
    NextCol is Col - 1,
    update_column(Rest, NextCol, NewValue, UpdatedRest).

% Example of a simple display function.
display_game([Board, CurrentPlayer]) :-
    nl,
    display_matrix(Board),
    nl,
    write('Current Player: '), write(CurrentPlayer),
    nl.

display_matrix(Matrix) :-
    length(Matrix, BoardSize),
    display_matrix(Matrix, 1),
    nl,
    write('  '),
    display_column_numbers(1, BoardSize),
    nl.

display_matrix([], _).
display_matrix([Row | Rest], RowNum) :-
    display_row_number(RowNum),
    display_row(Row),
    nl,
    NextRowNum is RowNum + 1,
    display_matrix(Rest, NextRowNum).

display_column_numbers(CurrentCol, MaxCol) :-
    CurrentCol > MaxCol,
    !.
display_column_numbers(CurrentCol, MaxCol) :-
    write('  '),
    write(CurrentCol),
    write(' '),
    NextCol is CurrentCol + 1,
    display_column_numbers(NextCol, MaxCol).

display_row_number(RowNum) :-
    nl,
    write(RowNum),
    write(' ').

display_row([]).
display_row([Cell | Rest]) :-
    write('| '),
    write(Cell),
    write(' '),
    display_row(Rest).
/*
% Example of the initial game state for Trike with a 7x7 board.
initial_state(Size, [Board, CurrentPlayer]) :-
    create_matrix(Size, Board),
    CurrentPlayer = white.
*/
% Display the last move
display_last_move :-
    last_move(LastRow, LastCol),
    format('Last Move: Row ~d, Column ~d~n', [LastRow, LastCol]).

% Generate a list of integers between L and H
generate_range(L, H, [L | Rest]) :-
    L < H,
    NextL is L + 1,
    generate_range(NextL, H, Rest).
generate_range(H, H, [H]).

available_moves(Matrix, AvailableMoves) :-
    last_move(LastRow, LastCol),
    length(Matrix, Size),
    generate_range(1, Size, Rows),
    generate_range(1, Size, Cols),
    findall([Row, Col], (
        member(Row, Rows),
        member(Col, Cols),
        valid_move(Row, Col, LastRow, LastCol, Matrix)
    ), MoveList),
    sort(MoveList, AvailableMoves).

% Define the initial last move
:- dynamic last_move/2.
last_move(7, 1).

valid_move(Row, Col, LastRow, LastCol, Matrix) :-
    Row > 0,
    Col > 0,
    Row =< 7, % Adjust this based on your matrix size
    Col =< Row, % Adjust this based on your specific condition
    (Row =\= LastRow ; Col =\= LastCol), % Ensure that Row and Col are not both the same as LastRow and LastCol
    (Row =:= LastRow ; Col =:= LastCol ; % Diagonal moves are allowed
     abs(Row - LastRow) =:= abs(Col - LastCol)), % Check for straight diagonal movement
    get_value(Matrix, Row, Col, Cell),
    Cell == 'X'.

get_value(Matrix, Row, Col, Value) :-
    nth1(Row, Matrix, CurrentRow), % Get the specified row from the matrix
    nth1(Col, CurrentRow, Value). % Get the value at the specified column in the row

% Predicate to switch players (white to black, black to white)
switch_player(white, black).
switch_player(black, white).


computer_make_move(Matrix, UpdatedMatrix, NextPlayer) :-
    display_game([Matrix, black]), % Display the current game state for the computer
    display_last_move, % Display the last move
    nl,

    % Generate and display a list of available moves
    available_moves(Matrix, AvailableMoves),
    format('Available Moves: ~w~n', [AvailableMoves]),

    % Choose a random move for the computer
    random_member([Row, Column], AvailableMoves),
    update_matrix(Matrix, Row, Column, 'B', UpdatedMatrix),

    % Switch to the next player
    switch_player(black, NextPlayer).

random_initial_move(Row, Column) :-
    between(1, 7, Row),
    between(1, Row, Column).

% Main game loop for computer vs player
computer_vs_player_game :-
    create_matrix(7, Matrix),
    
    random_initial_move(Row, Column),
    update_matrix(Matrix, Row, Column, 'B', UpdatedMatrix),

    % Start the game loop
    computer_vs_player_game_loop(UpdatedMatrix, white).

% Game loop for computer vs player
computer_vs_player_game_loop(Matrix, CurrentPlayer) :-
    (CurrentPlayer = black -> % 
        computer_make_move(Matrix, UpdatedMatrix, NextPlayer),
        write('Computer''s Turn'),
        sleep(2)
    ;
        make_move(Matrix, CurrentPlayer, UpdatedMatrix, NextPlayer)
    ),
    
    computer_vs_player_game_loop(UpdatedMatrix, NextPlayer).

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

make_move(Matrix, CurrentPlayer, UpdatedMatrix, NextPlayer) :-
    display_game([Matrix, CurrentPlayer]), % Display the current game state
    display_last_move, % Display the last move
    nl,

    % Generate and display a list of available moves
    available_moves(Matrix, AvailableMoves),
    format('Available Moves: ~w~n', [AvailableMoves]),

    write('Enter the row to update: '),
    read(Row),
    write('Enter the column to update: '),
    read(Column),

    current_player_symbol(CurrentPlayer, Symbol),
    update_matrix(Matrix, Row, Column, Symbol, UpdatedMatrix),

    switch_player(CurrentPlayer, NextPlayer).

% Game loop for player vs player
player_vs_player_game_loop(Matrix, CurrentPlayer) :-
    make_move(Matrix, CurrentPlayer, UpdatedMatrix, NextPlayer),
    player_vs_player_game_loop(UpdatedMatrix, NextPlayer).

% Main game loop for computer vs computer
computer_vs_computer_game :-
    create_matrix(7, Matrix),
    
    random_initial_move(Row, Column),
    update_matrix(Matrix, Row, Column, 'B', UpdatedMatrix),

    % Start the game loop
    computer_vs_computer_game_loop(UpdatedMatrix, white).

% Game loop for computer vs computer
computer_vs_computer_game_loop(Matrix, CurrentPlayer) :-
    (CurrentPlayer = black -> % 
        computer_make_move(Matrix, UpdatedMatrix, NextPlayer),
        write('Black Computer''s Turn'),
        sleep(2)
    ;
        computer_make_move(Matrix, UpdatedMatrix, NextPlayer),
        write('White Computer''s Turn'),
        sleep(2)
    ),
    
    computer_vs_computer_game_loop(UpdatedMatrix, NextPlayer).