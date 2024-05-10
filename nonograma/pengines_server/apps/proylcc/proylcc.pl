:- module(proylcc,
	[  
		put/8
	]).

:-use_module(library(lists)).
:- use_module(library(clpfd)).

search_clues([],[],true).
search_clues([0],[],true).
search_clues(Clues, [L|Ls], Valid):-
    L == "X",
    search_clues(Clues, Ls, Valid),
    !.
search_clues(Clues, [L|Ls], Valid):-
    L == "#",
    check_clue(Clues, ["#"|Ls], Valid),
    !.
search_clues(Clues, [_|Ls], Valid):-
    search_clues(Clues, Ls, Valid),
    !.
search_clues(_, [], false).

check_clue([0],[], true):-!.
check_clue([C|_Cs], [], false):-
    C > 0,
    !.
check_clue([],[L|_Ls], false):-
    L == "#",
    !.
check_clue([], _, true):-!.
check_clue([0|Cs], [L|Ls], Valid):-
    L == "X",
    check_clue(Cs, Ls, Valid),!.
check_clue([C|_], [L|[Ls|_]], false):-
    L == "#",
    C > 0,
    Caux is C - 1,
    (Ls \== "#", Caux \= 0),
    !.
check_clue([C|Cs], [L|Ls], Valid):-
    L == "#",
    C > 0,
    Caux is C - 1,
    check_clue([Caux|Cs], Ls, Valid),
    !.
check_clue([0|_],[L|_Ls], false):-
    L == "#",
    !.
check_clue([0|Cs], [_|Ls], Valid):-
    check_clue(Cs, Ls, Valid),
    !.
check_clue(Clues, [_|Ls], Valid):-
    check_clue(Clues, Ls, Valid),
    !.
check_clue(_, [_|_], false).

search(0, [Element|_], Element).
search(I, [_|Row], Element):-
    Is is I - 1,
    search(Is, Row, Element).

col_to_row(_Index,[],[]).
col_to_row(Index,[Row|Grid],[C|Column]):-
    search(Index,Row,C),
    col_to_row(Index,Grid,Column).

clue(0, [C_head|_C_tail], C_head).
clue(Cursor, [_C_head|C_tail], Return):-
    Cursor > 0,
    CursorAux is Cursor - 1,
    clue(CursorAux, C_tail, Return).
%
replace(X, 0, Y, [X|Xs], [Y|Xs]).
%
replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).
%
% put(+Content, +Pos, +RowsClues, +ColsClues, +Grid, -NewGrid, -RowSat, -ColSat).
%
put(Content, [RowN, ColN], RowsClues, ColsClues, Grid, NewGrid, RowSat, ColSat):-
    replace(Row, RowN, NewRow, Grid, NewGrid),
    (replace(Cell, ColN, _, Row, NewRow),
    Cell == Content
        ;
    replace(_Cell, ColN, Content, Row, NewRow)),
	copy_term(NewGrid, ClonedGrid),
    copy_term(NewRow, ClonedRow),
    clue(RowN, RowsClues, RowClue),
    clue(ColN, ColsClues, ColClue),
    col_to_row(ColN, ClonedGrid, Column),
    search_clues(RowClue, ClonedRow, RowSat),
    search_clues(ColClue, Column, ColSat).

initial_check_rows([],[],[]).
initial_check_rows([Clue|Clues], [Row|Rows], [RowSat|Sat]):-
    search_clues(Clue, Row, RowSat),
	initial_check_rows(Clues, Rows, Sat).
initial_check_cols(ColsClues, Grid, ColsCluesChecked):-
    transpose(Grid, Ts),
	initial_check_rows(ColsClues, Ts, ColsCluesChecked).
initial_check(RowsClues, ColsClues, Grid, RowsCluesChecked, ColsCluesChecked):-
    initial_check_rows(RowsClues, Grid, RowsCluesChecked),
    initial_check_cols(ColsClues, Grid, ColsCluesChecked).

victory_check_rows([],[],[]).
victory_check_rows([Clue|Clues], [Row|Rows], [RowSat|Sat]):-
    search_clues(Clue, Row, RowSat),
	victory_check_rows(Clues, Rows, Sat).
victory_check_cols(ColsClues, Grid, ColsCluesChecked):-
    transpose(Grid, Ts),
	victory_check_rows(ColsClues, Ts, ColsCluesChecked).
victory_check(RowsClues, ColsClues, Grid):-
    victory_check_rows(RowsClues, Grid, RowsCluesChecked),
    victory_check_cols(ColsClues, Grid, ColsCluesChecked),
    is_valid(RowsCluesChecked, ColsCluesChecked).

is_valid([],[]).
is_valid([],[ClueC|CluesC]):-
    ClueC == true,
    is_valid(CluesC, []).
is_valid([ClueR|CluesR],[]):-
    ClueR == true,
    is_valid(CluesR, []).
is_valid([ClueR|CluesR], [ClueC|CluesC]):-
    ClueR == true,
    ClueC == true,
    is_valid(CluesR, CluesC).

combinations(0, 0, []).
combinations(C, I, ["#"|T2]):-
    I =\= 0,
    Idx is I - 1,
    Cs is C-1,
    combinations(Cs, Idx, T2).
combinations(C, I, [_|T2]):-
    I =\= 0,
    Idx is I - 1,
    combinations(C, Idx, T2).

combinations_grid([], [], []).
combinations_grid([Clue|Clues], [Row|Grid], [RowOut|GridOut]):-
    total_clue(Clue, TotalClue),
    length(Row, Length),
    combinations(TotalClue, Length, RowOut),
    search_clues(Clue, RowOut, Valid),
    Valid == true,
    combinations_grid(Clues, Grid, GridOut).

total_clue([], 0).
total_clue([C|Clue], Total):-
    total_clue(Clue, Sum),
    Total is C + Sum.

solve(RowsClues, ColsClues, Grid, GridOut, RowsCluesChecked, ColsCluesChecked):-
    combinations_grid(RowsClues, Grid, GridOut),
    victory_check(RowsClues, ColsClues, GridOut),
    initial_check(RowsClues, ColsClues, GridOut, RowsCluesChecked, ColsCluesChecked).