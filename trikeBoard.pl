:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(between)).
:- use_module(library(system)).

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
    Var = '0'. 

% Update a value at a specific position in the matrix with move restrictions
update_matrix(Matrix, Row, Col, NewValue, UpdatedMatrix) :-
    % Keep track of the last row and last column
    last_move(LastRow, LastCol),

     ((LastRow =\= 0, LastCol =\= 0) ->
        valid_move(Row, Col, LastRow, LastCol, Matrix)
    ; true),

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
    write('  '),
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
    display_matrix(Rest, NextRowNum,BoardSize).

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
    write(' '),
    write(Cell),
    write('  '),
    display_row(Rest).

% Display the last move
display_last_move :-
    last_move(LastRow, LastCol),
    format('Last Move: Row ~d, Column ~d~n', [LastRow, LastCol]).
