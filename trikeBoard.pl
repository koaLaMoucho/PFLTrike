% Example of a simple display function.
display_game([Board, CurrentPlayer]) :-
    nl,
    display_board(Board),
    nl,
    write('Current Player: '), write(CurrentPlayer),
    nl.

% Updated display_board/1 to center the triangular board
display_board(Board) :-
    length(Board, BoardSize),
    MaxIndent is BoardSize - 1,
    display_board(Board, 7, MaxIndent).

display_board([], _, _).
display_board([Row | Rest], Indent, MaxIndent) :-
    indent(Indent, MaxIndent),
    display_row(Row),
    nl,
    NewIndent is Indent - 1,
    display_board(Rest, NewIndent, MaxIndent).

indent(0, _).
indent(N, MaxIndent) :-
    print_spaces(MaxIndent),
    Next is N - 1,
    indent(Next, MaxIndent).

print_spaces(0).
print_spaces(N) :-
    write(' '),
    Next is N - 1,
    print_spaces(Next).

display_row([]) :- nl.
display_row([Cell | Rest]) :-
    write(Cell),
    write(' '),
    display_row(Rest).


create_empty_row(0, []).
create_empty_row(Size, ['empty' | Rest]) :-
    NewSize is Size - 1,
    create_empty_row(NewSize, Rest).

create_board(Size, Board) :-
    create_board(Size, 1, Board).

create_board(0, _, []).
create_board(Size, RowNum, [Row | Rest]) :-
    create_empty_spaces(RowNum, Row),
    NewSize is Size - 1,
    NextRowNum is RowNum + 1,
    create_board(NewSize, NextRowNum, Rest).

create_empty_spaces(0, []).
create_empty_spaces(Size, [empty | Rest]) :-
    NewSize is Size - 1,
    create_empty_spaces(NewSize, Rest).

% Example of the initial game state for Trike with a 7x7 board.
initial_state(Size, [Board, CurrentPlayer]) :-
    create_board(Size, Board),
    CurrentPlayer = white.

