:- use_module(library(clpfd)).
:- use_module(library(lists)).

notpivot(E) :- var(E); integer(E).

up(M, X-Y, [E|L]) :- Y1 #= Y - 1, nth1(Y1, M, Linha), nth1(X, Linha, E), notpivot(E), up(M, X-Y1, L).
up(_, _, []).

down(M, X-Y, [E|L]) :- Y1 #= Y + 1, nth1(Y1, M, Linha), nth1(X, Linha, E), notpivot(E), down(M, X-Y1, L).
down(_, _, []).

left(M, X-Y, [E|L]) :- X1 #= X - 1, nth1(Y, M, Linha), nth1(X1, Linha, E), notpivot(E), left(M, X1-Y, L).
left(_, _, []).

right(M, X-Y, [E|L]) :- X1 #= X + 1, nth1(Y, M, Linha), nth1(X1, Linha, E), notpivot(E), right(M, X1-Y, L).
right(_, _, []).

constrainpivot(M, X-Y-Pivot) :-
  left(M, X-Y, L),
  right(M, X-Y, R),
  up(M, X-Y, U),
  down(M, X-Y, D), !,
  constraint(L, R, U, D, Pivot).

constseq([], _, 0).
constseq([L|_], A, 0) :- L #\= A.
constseq([A|Ls], A, N) :- N1 #= N - 1, constseq(Ls, A, N1).

constraint(L, R, U, D, N) :-
  NL + NR + NU + ND #= N,
  constseq(L, 0, NL),
  constseq(R, 0, NR),
  constseq(U, 1, NU),
  constseq(D, 1, ND).

findPivots(M, X-Y, P) :-
  nth1(Y, M, R),
  nth1(X, R, E),
  \+ var(E),
  E = p(P).

solve(M) :-
  length(M, Size),
  X in 1..Size,
  Y in 1..Size,
  extractvars(M, Vs),
  domain(Vs,0,1),
  findall(X-Y-P, findPivots(M, X-Y, P), R),
  maplist(constrainpivot(M), R),
  labeling([],Vs).

extractvars(M, Vs) :- append(M, F), include(var, F, Vs).

length_(S, M) :- length(M, S).

printrow([A|Es]) :- var(A), write('.'), write(' '), printrow(Es).
printrow([p(N)|Es]) :- write(N), write(' '), printrow(Es).
printrow([1|Es]) :- write('|'), write(' '), printrow(Es).
printrow([0|Es]) :- write('-'), write(' '), printrow(Es).
printrow([]).

printmatrix([Row|Rs]) :- printrow(Row), nl, printmatrix(Rs).
printmatrix([]):- nl.

matrix([[_, _, p(4), _, _, p(1)],
       [_, p(2), _, _, _, _],
       [p(3), _, _, p(1), _, _],
       [_, _, p(1), _, _, p(2)],
       [_, _, _, _, p(2), _],
       [p(2), _, _, p(3), _, _]]).
  /* M = [[p(1), _, p(1)],
       [_, _, _],
       [_, _, _]], */
solvePuzzle(M):-
  matrix(M),
  solve(M),
  printmatrix(M).
