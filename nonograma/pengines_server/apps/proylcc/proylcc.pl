:- module(proylcc,
	[  
		put/9
	]).

:-use_module(library(lists)).
:- use_module(library(clpfd)).

%
% search_clues(+Clues, +Row, -Valid).
%
search_clues([],[],true).
search_clues([0],[],true).
search_clues(_,[],false).
search_clues(Clues, [L|Ls], Valid):-
    L == "#",
    check_clue(Clues, [L|Ls], Valid),!.
search_clues(Clues, [_|Ls], Valid):-
    search_clues(Clues, Ls, Valid),!.
%
% check_clue(+Clues, +Row, -Valid).
%
check_clue([0], [], true).
check_clue([0|_Cs], [L|_Ls], false):-
    L == "#",!.
check_clue([0|Cs], [_L|Ls], Valid):-
    search_clues(Cs, Ls, Valid),!.
check_clue([C|Cs], [L|Ls], Valid):-
    L == "#",
    C > 0,
    Caux is C - 1,
    check_clue([Caux|Cs], Ls, Valid),!.
check_clue(_,_,false).
%
% search(+Position, +List, -Element).
%
search(0, [Element|_], Element).
search(I, [_|Row], Element):-
    Is is I - 1,
    search(Is, Row, Element).
%
% col_to_row(+ColPosition, +Grid, -Col).
%
col_to_row(_Index,[],[]).
col_to_row(Index,[Row|Grid],[C|Column]):-
    search(Index,Row,C),
    col_to_row(Index,Grid,Column).
%
% replace()
%
replace(X, 0, Y, [X|Xs], [Y|Xs]).
replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
    XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).
%
% put(+Content, +Pos, +RowsClues, +ColsClues, +Grid, -NewGrid, -RowSat, -ColSat).
%
put(Content, [RowN, ColN], RowsClues, ColsClues, Grid, NewGrid, RowSat, ColSat, Cant):-
    replace(Row, RowN, NewRow, Grid, NewGrid),
    (replace(Cell, ColN, _, Row, NewRow),
    Cell == Content
        ;
    replace(_Cell, ColN, Content, Row, NewRow)),
    search(RowN, RowsClues, RowClue),
    search(ColN, ColsClues, ColClue),
    col_to_row(ColN, NewGrid, Column),
    search_clues(RowClue, NewRow, RowSat),
    search_clues(ColClue, Column, ColSat),
    count_grid(NewGrid, Cant).
%
% initial_check(+RowsClues, +ColsClues, +Grid, -RowsCluesChecked, -ColsCluesChecked, -Cant).
%
initial_check(RowsClues, ColsClues, Grid, RowsCluesChecked, ColsCluesChecked, Cant):-
    initial_check_rows(RowsClues, Grid, RowsCluesChecked),
    initial_check_cols(ColsClues, Grid, ColsCluesChecked),
    count_grid(Grid, Cant).
initial_check_rows([],[],[]).
initial_check_rows([Clue|Clues], [Row|Rows], [RowSat|Sat]):-
    search_clues(Clue, Row, RowSat),
	initial_check_rows(Clues, Rows, Sat).
initial_check_cols(ColsClues, Grid, ColsCluesChecked):-
    transpose(Grid, Ts),
	initial_check_rows(ColsClues, Ts, ColsCluesChecked).
%
% victory_check(+RowsClues, +ColsClues, +Grid).
%
victory_check(RowsClues, ColsClues, Grid):-
    initial_check(RowsClues, ColsClues, Grid, RowsCluesChecked, ColsCluesChecked, _),
    is_valid(RowsCluesChecked, ColsCluesChecked).
%
% is_valid(+RowsCluesChecked, +ColsCluesChecked).
%
is_valid(RowsCluesChecked, ColsCluesChecked):-
    all_true(RowsCluesChecked),
    all_true(ColsCluesChecked).
%
% all_true(+List).
%
all_true([]).
all_true([L|Ls]):-
    L == true,
    all_true(Ls).
%
% combinations(+Clues, +Row, -RowOut).
%
combinations([], [], []).
combinations(Clues, [L|Ls], Lout):-
    L = "#",
    consecutive(Clues, [L|Ls], Lout).
combinations(Clues, [L|Ls], ["X"|T]):-
    L = "X",
    combinations(Clues, Ls, T).
%
% consecutive(+Clues, +Row, -RowOut).
%
consecutive([0], [], []).
consecutive([0|Cs], [L|Ls], ["X"|T]):-
    L = "X",
    combinations(Cs, Ls, T).
consecutive([C|Cs], [L|Ls], ["#"|T]):-
    L = "#",
    C > 0,
    Caux is C-1,
    consecutive([Caux|Cs], Ls, T).
%
% combinations_grid(+Clues, +Grid, -GridOut).
%
combinations_grid([], [], []).
combinations_grid([Clue|Clues], [Row|Grid], [RowOut|GridOut]):-
    combinations(Clue, Row, RowOut),
    combinations_grid(Clues, Grid, GridOut).
%
% solve(+RowsClues, +ColsClues, +Grid, -GridOut, -Cant).
%
solve(RowsClues, ColsClues, Grid, GridOut, Cant):-
    combinations_grid(RowsClues, Grid, GridOut),
    initial_check(RowsClues, ColsClues, GridOut, RowsCluesChecked, ColsCluesChecked, Cant),
    is_valid(RowsCluesChecked, ColsCluesChecked).
%
% count(+List, -TotalCount).
%
count([], 0).
count([X|Res], Cant) :-
    X == "#",
    count(Res, CantRes),
    Cant is CantRes + 1.
count([X|Res], Cant) :-
    X \== "#",
    count(Res, Cant).
%
% count_grid(+Grid, -TotalCount).
%
count_grid([], 0).
count_grid([Fila|Res], CantTotal) :-
    count(Fila, CantFila),
    count_grid(Res, CantRes),
    CantTotal is CantFila + CantRes.