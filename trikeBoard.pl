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

% Update a value at a specific position in the matrix
update_matrix(Matrix, Row, Col, NewValue, UpdatedMatrix) :-
    update_row(Matrix, Row, Col, NewValue, UpdatedMatrix).

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


% Example of the initial game state for Trike with a 7x7 board.
initial_state(Size, [Board, CurrentPlayer]) :-
    create_matrix(Size, Board),
    CurrentPlayer = white.

% Example of how to use the update_matrix predicate
example_update :-
    create_matrix(7, Matrix),
    display_game([Matrix, white]), % Display the initial game state
    nl,
    write('Enter the row to update'),
    read(Row),
    write('Enter the column to update'),
    read(Column),
    write('Enter the new value'),
    read(NewValue),
    update_matrix(Matrix, Row, Column, NewValue, UpdatedMatrix), % Update a value
    display_game([UpdatedMatrix, black]). % Display the updated game state with black as the current player
