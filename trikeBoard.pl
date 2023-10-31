:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).
:- use_module(library(system)).
:- use_module(library(not)).

is_empty_list([]). % to easily see if a list is empty

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
    valid_move(Row, Col, LastRow, LastCol, Matrix),

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
    display_matrix(Matrix, 1, BoardSize),
    write('    ---------------------------'),
    nl,
    write('   '),
    display_column_numbers(1, BoardSize),
    nl.

display_matrix([], _, _).
display_matrix([Row | Rest], RowNum, BoardSize) :-
    SpacesBefore is (BoardSize - RowNum) * 2,
    display_row_number(RowNum),
    display_spaces(SpacesBefore),
    display_row(Row),
    nl,
    NextRowNum is RowNum + 1,
    display_matrix(Rest, NextRowNum, BoardSize).

display_spaces(0).
display_spaces(N) :-
    N > 0,
    write(' '),
    NextN is N - 1,
    display_spaces(NextN).

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
    write(' |').

display_row([]).
display_row([Cell | Rest]) :-
    write('  '),
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

/*
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
*/

% Check if the move from LastRow/LastCol to Row/Col is valid on the Matrix
valid_move(Row, Col, LastRow, LastCol, Matrix) :-
    within_board(Row, Col, Matrix),                  % The destination is within the bounds of the board.
    \+ occupied(Row, Col, Matrix),                   % The destination is not occupied.
    is_straight_line(LastRow, LastCol, Row, Col),    % The move is in a straight line.
    path_clear(LastRow, LastCol, Row, Col, Matrix).  % The path from the pawns current to the destination is clear.

% Check if a position is within the bounds of the board.
within_board(Row, Col, Matrix) :-
    length(Matrix, NumRows),
    nth1(Row, Matrix, RowList),
    length(RowList, NumCols),
    between(1, NumRows, Row),
    between(1, NumCols, Col).

% Check if a position is occupied.
occupied(Row, Col, Matrix) :-
    nth1(Row, Matrix, RowList),
    nth1(Col, RowList, Cell),
    Cell \= 'X'. % assuming 'X' is the representation of an unoccupied cell

% Check if a move is in a straight line (this predicate should be implemented based on the games geometry).
is_straight_line(Row, _LastCol, Row, _Col). % Vertical movement
is_straight_line(_LastRow, Col, _Row, Col). % Horizontal movement
is_straight_line(LastRow, LastCol, Row, Col) :- % Diagonal movement
    (Row - LastRow) =:= (Col - LastCol).

% Check if the path is clear of checkers.
/*
path_clear(LastRow, LastCol, Row, Col, Matrix) :-
    % This predicate needs to verify each point on the path between the pawns current position
    % and the destination to make sure none of them are occupied.
    % The implementation depends on the board layout and needs to handle different directions.
*/

% Base case: If the last position is the same as the new position, the path is clear.
path_clear(Row, Col, Row, Col, _Matrix).

% Recursive case: Check if the path is clear by advancing one step towards the destination and then calling path_clear recursively.
path_clear(LastRow, LastCol, Row, Col, Matrix) :-
    next_step(LastRow, LastCol, Row, Col, NextRow, NextCol),  % Determine the next step
    \+ occupied(NextRow, NextCol, Matrix),
    path_clear(NextRow, NextCol, Row, Col, Matrix).  % Recurse to check the rest of the path

% Helper predicate to determine the next step in the path
% Vertical movement
next_step(LastRow, LastCol, Row, Col, NextRow, LastCol) :-
    LastCol == Col,  % Vertical movement, column stays the same
    LastRow \= Row,  % Ensure we are not already at the destination row
    (LastRow < Row -> NextRow is LastRow + 1; NextRow is LastRow - 1).

% Horizontal movement
next_step(LastRow, LastCol, Row, Col, LastRow, NextCol) :-
    LastRow == Row,  % Horizontal movement, row stays the same
    LastCol \= Col,  % Ensure we are not already at the destination column
    (LastCol < Col -> NextCol is LastCol + 1; NextCol is LastCol - 1).

% Diagonal movement
next_step(LastRow, LastCol, Row, Col, NextRow, NextCol) :-
    (Row - LastRow) =:= (Col - LastCol),  % Ensure its a diagonal move
    LastRow \= Row,
    LastCol \= Col,
    (LastRow < Row -> NextRow is LastRow + 1; NextRow is LastRow - 1),
    (LastCol < Col -> NextCol is LastCol + 1; NextCol is LastCol - 1).

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
    \+ is_empty_list(AvailableMoves),
    format('Available Moves: ~w~n', [AvailableMoves]),

    write('Enter the row to update: '),
    read(Row),
    write('Enter the column to update: '),
    read(Column),

    current_player_symbol(CurrentPlayer, Symbol),
    update_matrix(Matrix, Row, Column, Symbol, UpdatedMatrix),

    switch_player(CurrentPlayer, NextPlayer).

% Game loop for player vs player
/*player_vs_player_game_loop(Matrix, CurrentPlayer) :-
    make_move(Matrix, CurrentPlayer, UpdatedMatrix, NextPlayer),
    player_vs_player_game_loop(UpdatedMatrix, NextPlayer).
*/

player_vs_player_game_loop(Matrix, CurrentPlayer) :-
    (   make_move(Matrix, CurrentPlayer, UpdatedMatrix, NextPlayer) ->
        player_vs_player_game_loop(UpdatedMatrix, NextPlayer)  % If make_move succeeds, recurse with updated state.
    ;   game_over(Matrix, CurrentPlayer)  % If make_move fails, end the game.
    ).

/*game_over(Matrix, Player) :-
    % Do something with the final game state, like printing the board or announcing the winner.
    display_board(Matrix),
    format('Player ~w cannot make a move. Game over.', [Player]),
    !, fail.  % Cut and fail to prevent backtracking.
*/

% Define the predicate to call when the game is over.
game_over(Matrix, Player) :-
    last_move(Row, Column),
    % Calculate scores for both players.
    score(Matrix, black, Row-Column, ScoreBlack),
    score(Matrix, white, Row-Column, ScoreWhite),
    get_value(Matrix, Row, Column, Value),
    (Value == 'B' -> ScoreBlack1 = ScoreBlack, ScoreWhite1 is ScoreWhite - 1
                ; ScoreBlack1 is ScoreBlack - 1, ScoreWhite1 = ScoreWhite),
    % Do something with the scores, like print them.
    format('Game over. ~nBlack score: ~w ~nWhite score: ~w~n', [ScoreBlack1, ScoreWhite1]),
    % Determine the winner.
    (ScoreBlack1 > ScoreWhite1 -> format('Black wins!~n', []);
    ScoreWhite1 > ScoreBlack1 -> format('White wins!~n', []);
    format('It is a tie!~n', [])),
    !, fail. % Cut and fail to prevent backtracking.

% Define the score predicate.
% score(Matrix, PlayerColor, PawnPosition, Score)
score(Matrix, Color, PawnPosition, Score) :-
    % Start DFS from the pawns final position to count the number of adjacent checkers.
    dfs(Matrix, Color, [PawnPosition], [], 0, Score).

% Define the DFS predicate.
% dfs(Matrix, Color, Stack, Visited, CurrentScore, FinalScore)
dfs(_, _, [], Visited, Score, Score) :- length(Visited, Score).
dfs(Matrix, Color, [Pos|Rest], Visited, CurrentScore, Score) :-
    % Get all adjacent positions to Pos that have not been visited and contain Colors checker.
    findall(AdjPos, (adjacent(Pos, AdjPos), valid_checker(Matrix, AdjPos, Color), \+ member(AdjPos, Visited)), AdjacentPositions),
    % Merge new positions with the current stack while maintaining DFS order.
    append(AdjacentPositions, Rest, NewStack),
    % Mark the current position as visited.
    NewVisited = [Pos|Visited],
    % Continue DFS with the new stack and visited list.
    CurrentScore1 is CurrentScore + 1,
    dfs(Matrix, Color, NewStack, NewVisited, CurrentScore1, Score).

% Predicate to find adjacent positions on the board.
% adjacent(Position, AdjacentPosition)
adjacent(Row-Col, AdjRow-AdjCol) :-
    adjacent_hex(Row, Col, AdjRow, AdjCol).

% Horizontal adjacent positions (same row)
adjacent_hex(Row, Col, Row, AdjCol) :- AdjCol is Col - 1.
adjacent_hex(Row, Col, Row, AdjCol) :- AdjCol is Col + 1.

% Vertical adjacent positions (same column)
adjacent_hex(Row, Col, AdjRow, Col) :- AdjRow is Row - 1.
adjacent_hex(Row, Col, AdjRow, Col) :- AdjRow is Row + 1.

% Diagonal adjacent positions
adjacent_hex(Row, Col, AdjRow, AdjCol) :-
    AdjRow is Row - 1, AdjCol is Col - 1.
adjacent_hex(Row, Col, AdjRow, AdjCol) :-
    AdjRow is Row + 1, AdjCol is Col + 1.


% Predicate to check if theres a valid checker at a position for the given color.
% valid_checker(Matrix, Position, Color)
valid_checker(Matrix, Row-Col, Color) :-
    get_value(Matrix, Row, Col, Value),
    Value == 'B',
    Color == black.
valid_checker(Matrix, Row-Col, Color) :-
    get_value(Matrix, Row, Col, Value),
    Value == 'W',
    Color == white.

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