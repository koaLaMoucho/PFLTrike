% Example of a simple display function.
display_game([Board, CurrentPlayer]) :-
    nl,
    display_board(Board),
    nl,
    write('Current Player: '), write(CurrentPlayer),
    nl.

% Updated display_board/1 to create a right-angled triangle with 90 degrees on the bottom left
display_board(Board) :-
    length(Board, BoardSize),
    display_board(Board, BoardSize, BoardSize),
    write('  '),
    display_column_numbers(1, BoardSize).

display_board([], _, _).
display_board([Row | Rest], MaxSize, RowNum) :-
    display_row_number(RowNum),
    display_row(Row, MaxSize),
    nl, % Newline between each row
    display_separator(MaxSize),
    NextRowNum is RowNum - 1,
    display_board(Rest, MaxSize, NextRowNum).

display_separator(0) :- nl.
display_separator(Size) :-
    write(''), % Line between rows
    NewSize is Size - 1,
    display_separator(NewSize).

display_row_number(RowNum) :-
    write(RowNum),
    write(' ').

display_row([], _).
display_row([Cell | Rest], MaxSize) :-
    write('| '),
    write(Cell),
    write(' '),
    display_row(Rest, MaxSize).

display_column_numbers(CurrentCol, MaxCol) :-
    CurrentCol > MaxCol,
    !.
display_column_numbers(CurrentCol, MaxCol) :-
    write('  '), % Added three spaces for better alignment
    write(CurrentCol),
    write(' '),
    NextCol is CurrentCol + 1,
    display_column_numbers(NextCol, MaxCol).

% Updated create_board/2 to create a matrix with variables
create_board(Size, Board) :-
    create_board(Size, 1, Board).

create_board(0, _, []).
create_board(Size, RowNum, [Row | Rest]) :-
    create_empty_spaces(RowNum, Row),
    NewSize is Size - 1,
    NextRowNum is RowNum + 1,
    create_board(NewSize, NextRowNum, Rest).

create_empty_spaces(0, []).
create_empty_spaces(Size, [Var | Rest]) :-
    NewSize is Size - 1,
    create_empty_spaces(NewSize, Rest),
    Var = 'X'. 

% Example of the initial game state for Trike with a 7x7 board.
initial_state(Size, [Board, CurrentPlayer]) :-
    create_board(Size, Board),
    CurrentPlayer = white.

