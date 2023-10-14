% Define a simple between predicate
my_between(Min, Max, Min) :- Min =< Max.
my_between(Min, Max, Value) :- Min < Max, NewMin is Min + 1, my_between(NewMin, Max, Value).

% Initialize the board with empty points
initialize_board(Height, Board) :-
    findall((X,Y), (my_between(1, Height, X), my_between(1, X, Y)), Board).

% Display the Trike board
display_board(Board) :-
    member((X, Y), Board),
    format('(~d,~d) ', [X, Y]),
    nl,
    fail.
display_board(_).

% Menu to start the game
start_menu :-
    write('Do you want to start the game? (1) Yes, (0) No: '),
    read(Choice),
    (Choice =:= 1 -> start_game ; write('Game not started.'), nl).

% Main predicate to start the game
start_game :-
    initialize_board(9, Board),
    display_board(Board).

% Example usage
:- initialization(main).
main :-
    start_menu.
