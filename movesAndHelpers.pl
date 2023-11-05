


% Generate a list of integers between L and H
generate_range(L, H, [L | Rest]) :-
    L < H,
    NextL is L + 1,
    generate_range(NextL, H, Rest).
generate_range(H, H, [H]).

% List of available moves based on the last row, the matrix and the valid moves
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
last_move(0,0).

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
    Cell \= '0'. 

% Check if a move is in a straight line 
is_straight_line(Row, _LastCol, Row, _Col). % Vertical movement
is_straight_line(_LastRow, Col, _Row, Col). % Horizontal movement
is_straight_line(LastRow, LastCol, Row, Col) :- % Diagonal movement
    (Row - LastRow) =:= (Col - LastCol).

% Check if the path is clear of checkers.

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

% Sequence of steps when a random computer is making a move
computer_make_move(Matrix,CurrentPlayer, UpdatedMatrix, NextPlayer) :-
    display_game([Matrix, CurrentPlayer]), 
    display_last_move, 
    nl,
    
    available_moves(Matrix, AvailableMoves),
    \+ is_empty_list(AvailableMoves),
    format('Available Moves: ~w~n', [AvailableMoves]),

   
    random_member([Row, Column], AvailableMoves),
    current_player_symbol(CurrentPlayer, Symbol),
    update_matrix(Matrix, Row, Column, Symbol, UpdatedMatrix),

  
    switch_player(CurrentPlayer, NextPlayer).

% Initial move when a computer is starting the game
random_initial_move(Row, Column) :-
    random(1, 7, Row),
    
   random(1, Row, Column),
   write('Random Initial Move: Row '), write(Row), write(', Column '), write(Column), nl.
  

% Sequence of steps when a player is making a move
make_move(Matrix, CurrentPlayer, UpdatedMatrix, NextPlayer) :-
    display_game([Matrix, CurrentPlayer]), 
    display_last_move, 
    nl,

    
    available_moves(Matrix, AvailableMoves),
    \+ is_empty_list(AvailableMoves),

    ask_move(Row, Column, AvailableMoves),

    current_player_symbol(CurrentPlayer, Symbol),
    update_matrix(Matrix, Row, Column, Symbol, UpdatedMatrix),

    switch_player(CurrentPlayer, NextPlayer).

% Ask the user for a valid move until is given one
ask_move(Row, Column, AvailableMoves) :-
    format('Available Moves: ~w~n', [AvailableMoves]),

    write('Enter the row to update: '),
    read(NewRow),
    skip_line,
    write('Enter the column to update: '),
    read(NewColumn),
    skip_line,

    member([NewRow, NewColumn], AvailableMoves),
    Row = NewRow,
    Column = NewColumn,
    !.

ask_move(Row, Column, AvailableMoves) :-
    ask_move(Row, Column, AvailableMoves).

% Predicate to call when the game is over
game_over(Matrix, Player) :-
    last_move(Row, Column),
    
    score(Matrix, black, Row-Column, ScoreBlack),
    score(Matrix, white, Row-Column, ScoreWhite),

   
    format('Game over. ~nBlack score: ~w ~nWhite score: ~w~n', [ScoreBlack, ScoreWhite]),
  
    (ScoreBlack > ScoreWhite -> 
        format('Black wins!~n', [])
    ; ScoreWhite > ScoreBlack ->
        format('White wins!~n', [])
    ),
    sleep(3),
    nl, nl,
     retractall(last_move(_, _)),
    asserta(last_move(0, 0)),
    play.


% Calculate the final score of a Player
% score(Matrix, PlayerColor, Pos, Score)
score(Matrix, Color, Pos, Score) :-
    findall(AdjPos, (adjacent(Pos, AdjPos), valid_checker(Matrix, AdjPos, Color)), AdjacentPositions), % Get all adjacent positions to Pos that contain Colors checker.
    length(AdjacentPositions, Score). % The score is the number of adjacent positions.

% Find adjacent positions on the board

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

% Same position
adjacent_hex(Row, Col, Row, Col).

% Check if theres a valid checker at a position for the given color

valid_checker(Matrix, Row-Col, Color) :-
    get_value(Matrix, Row, Col, Value),
    Value == 'B',
    Color == black.
valid_checker(Matrix, Row-Col, Color) :-
    get_value(Matrix, Row, Col, Value),
    Value == 'W',
    Color == white.


% Sequence of steps when a greedy computer is making a move
computer_make_move2(Matrix,CurrentPlayer, UpdatedMatrix, NextPlayer) :-
    display_game([Matrix, CurrentPlayer]), 
    display_last_move, 
    nl,
   
    available_moves(Matrix, AvailableMoves),
    \+ is_empty_list(AvailableMoves),
    format('Available Moves: ~w~n', [AvailableMoves]),

  
  
    greedy_move(Matrix, CurrentPlayer, AvailableMoves, [Row, Column]),    

    current_player_symbol(CurrentPlayer, Symbol),
    update_matrix(Matrix, Row, Column, Symbol, UpdatedMatrix),
    sleep(2),
  
    switch_player(CurrentPlayer, NextPlayer).


% Greedy move for a computer player


greedy_move(Matrix, CurrentPlayer, AvailableMoves, BestMove) :-
   
    maplist(greedy_move_helper(Matrix, CurrentPlayer), AvailableMoves, MoveScores),
  
    maplist(extract_score, MoveScores, ScoresList),
    max_list(ScoresList, MaxScore),

    findall(Move, (member(Score-Move, MoveScores), Score == MaxScore), BestMoves),
   
    maplist(pair_to_list, BestMoves, BestMoves1),

    random_member(BestMove, BestMoves1).
  
    
% Greedy move helper predicate
greedy_move_helper(Matrix, CurrentPlayer, [Row, Column], Score-Move) :-
    Move = Row-Column,
    current_player_symbol(CurrentPlayer, Symbol),
 
    update_matrix2(Matrix, Row, Column, Symbol, UpdatedMatrix),

    % Calculate scores for both players.
    score(UpdatedMatrix, black, Row-Column, ScoreBlack),
    score(UpdatedMatrix, white, Row-Column, ScoreWhite),
    (CurrentPlayer == black -> Score is ScoreBlack - ScoreWhite; Score is ScoreWhite - ScoreBlack).
   

% Second predicate to update board matrix for greedy computer
update_matrix2(Matrix, Row, Col, NewValue, UpdatedMatrix) :-
    
    valid_move(Row, Col, LastRow, LastCol, Matrix),

    update_row(Matrix, Row, Col, NewValue, UpdatedMatrix).

% Find the maximum element in a list
max_list([H|T], Max) :-
    max_list(T, H, Max).

max_list([], Max, Max).
max_list([H|T], TempMax, Max) :-
    H > TempMax,
    !,
    max_list(T, H, Max).
max_list([_|T], TempMax, Max) :-
    max_list(T, TempMax, Max).

% Convert a pair to a list
pair_to_list(Row-Col, [Row, Col]).

% Convert a list to a pair
list_to_pair([Row, Col], Row-Col).

% Extract the score from a move-score pair
extract_score(Score-(Row-Column), Score).