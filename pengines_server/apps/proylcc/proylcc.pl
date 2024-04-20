:- module(proylcc,
	[  
		put/8
	]).

:-use_module(library(lists)).

search_clues(_, [], false).
search_clues([],[],true).
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

check_clue([0],[], true):-!.
check_clue([C|_Cs], [], false):-
    C > 0,
    !.
check_clue([0], _, true):-!.
check_clue([0],["#"|_Ls], false):-!.
check_clue([],["#"|_Ls], false):-!.
check_clue([], _, true):-!.
check_clue([0|Cs], [L|Ls], Valid):-
    L == "X",
    check_clue(Cs, Ls, Valid),!.
check_clue(_, [L|_], false):-
    L == "X",
    !.
check_clue([C|Cs], [L|Ls], Valid):-
    L == "#",
    C > 0,
    Caux is C - 1,
    check_clue([Caux|Cs], Ls, Valid),
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

clone([],[]).
clone([X|Xs], [X|Ys]):-
    clone(Xs, Ys).

clone_grid([], []).
clone_grid([X|Xs], [Y|Ys]):-
    clone(X, Y),
    clone_grid(Xs, Ys).
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
	clone_grid(NewGrid, ClonedGrid),
    clone(NewRow, ClonedRow),
    clue(RowN, RowsClues, RowClue),
    clue(ColN, ColsClues, ColClue),
    col_to_row(ColN, ClonedGrid, Column),
    search_clues(RowClue, ClonedRow, RowSat),
    search_clues(ColClue, Column, ColSat).