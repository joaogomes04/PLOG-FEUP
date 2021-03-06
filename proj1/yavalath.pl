:- use_module(library(lists)).
:- use_module(library(random)).
:- include('utilities.pl').

% -----Initialize Board --------

init([['#', '#', ' ', ' ', ' ', ' ', ' ', '#', '#'],
      ['#', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#'],
      ['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
      [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
      [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
      [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
      ['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
      ['#', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#'],
      ['#', '#', ' ', ' ', ' ', ' ', ' ', '#', '#']]).

% --- Game Menu ---
printGameMenu:-
  write('-------------------------------------'), nl,
	write('-            Yavalath               -'), nl,
	write('-------------------------------------'), nl,
	write('-                                   -'), nl,
  write('-   > 1. How to Play                -'), nl,
  write('-   > Game Mode:                    -'), nl,
	write('-     2. Player vs. Player          -'), nl,
	write('-     3. Player vs. Random Computer -'), nl,
  write('-     4. Player vs. Smart Computer  -'), nl,
  write('-     5. SComputer vs. SComputer    -'), nl,
  write('-     6. Quit                       -'), nl,
	write('-                                   -'), nl,
	write('-------------------------------------'), nl,
	write('Choose an option:'), nl.

  gameModeMenu:-
    printGameMenu,
    getChar(Input),
  	(
  		Input = '1' -> explain, gameModeMenu;
  		Input = '2' -> startHvHGame;
  		Input = '3' -> startHvCGame;
        Input = '4' -> startHvSCGame;
		Input = '5' ->startSCvSCGame;
  		Input = '6';


  		nl,
  		write('Error: invalid input.'), nl,
  		gameModeMenu
  	).

% --- Explanation for the Human User ---
explain:-
  write('~~~~~~~~~~~~~~~~~~~~~ Yavalath ~~~~~~~~~~~~~~~~~~~~'), nl,
  write('You play by entering one of the following positions'), nl,
  write('---------------------------------------------------'), nl,
  nl,
  padding(X),
  displayBoard([['#',  '#',  'A3', 'A4', 'A5', 'A6', 'A7', '#',  '#'],
           ['#',  'B2', 'B3', 'B4', 'B5', 'B6', 'B7', '#',  '#'],
           ['#',  'C2', 'C3', 'C4', 'C5', 'C6', 'C7', 'C8', '#'],
           ['D1', 'D2', 'D3', 'D4', 'D5', 'D6', 'D7', 'D8', '#'],
           ['E1', 'E2', 'E3', 'E4', 'E5', 'E6', 'E7', 'E8', 'E9'],
           ['F1', 'F2', 'F3', 'F4', 'F5', 'F6', 'F7', 'F8', '#'],
           ['#',  'G2', 'G3', 'G4', 'G5', 'G6', 'G7', 'G8', '#'],
           ['#',  'H2', 'H3', 'H4', 'H5', 'H6', 'H7', '#',  '#'],
           ['#',  '#',  'I3', 'I4', 'I5', 'I6', 'I7', '#',  '#']], X), nl,
  write('---------------------------------------------------'), nl, nl, nl.

% --- Display predicates ---
padding(['A      ', 'B      ', 'C    ', 'D    ', 'E  ', 'F    ', 'G    ', 'H      ', 'I      ']).

displayBoard([], []).
displayBoard([L|R], [A|B]) :-
  write(A), displayrow(L), nl,
  displayBoard(R, B).

displayrow([]).
displayrow(['#'|R]) :- write('  '), !, displayrow(R).
displayrow([' '|R]) :- write('( ) '), !, displayrow(R).
displayrow(['x'|R]) :- write('(x) '), !, displayrow(R).
displayrow(['o'|R]) :- write('(o) '), !, displayrow(R).
displayrow([H|R]) :- write('('), write(H), write(')'), !, displayrow(R).


% --- End Game ---
haswon(P, Board)  :- endgame([P, P, P, P], Board).
haslost(P, Board) :- endgame([P, P, P], Board).

endgame(Pattern, Board) :- search(Pattern, Board).
endgame(Pattern, Board) :- rotate60(Board, Board60), search(Pattern, Board60).
endgame(Pattern, Board) :- rotate120(Board, Board120), search(Pattern, Board120).

search(Pattern, Board) :- member(Row, Board), sublist(Pattern, Row).

sublist(X, Y) :- prefix(Y, X), !.
sublist(X, [_|YS]) :- sublist(X, YS).

rotate60([[_,  _,  A3, A4, A5, A6, A7, _,  _ ],
          [_,  B2, B3, B4, B5, B6, B7, _,  _ ],
          [_,  C2, C3, C4, C5, C6, C7, C8, _ ],
          [D1, D2, D3, D4, D5, D6, D7, D8, _ ],
          [E1, E2, E3, E4, E5, E6, E7, E8, E9],
          [F1, F2, F3, F4, F5, F6, F7, F8, _ ],
          [_,  G2, G3, G4, G5, G6, G7, G8, _ ],
          [_,  H2, H3, H4, H5, H6, H7, _,  _ ],
          [_,  _,  I3, I4, I5, I6, I7, _,  _ ]],

         [['#', '#', A7, B7, C8, D8, E9, '#', '#'],
          ['#', A6,  B6, C7, D7, E8, F8, '#', '#'],
          ['#', A5,  B5, C6, D6, E7, F7, G8,  '#'],
          [A4,  B4,  C5, D5, E6, F6, G7, H7,  '#'],
          [A3,  B3,  C4, D4, E5, F5, G6, H6,  I7 ],
          [B2,  C3,  D3, E4, F4, G5, H5, I6,  '#'],
          ['#', C2,  D2, E3, F3, G4, H4, I5,  '#'],
          ['#', D1,  E2, F2, G3, H3, I4, '#', '#'],
          ['#', '#', E1, F1, G2, H2, I3, '#', '#']]).

rotate120(A, B) :- rotate60(A, C), rotate60(C, B).

% --- Board Movement ---
move(Player, Board, X, Y, NewBoard) :-
  nth0(X, Board, Row, T1),
  nth0(Y, Row, ' ', T2),
  nth0(Y, NewRow, Player, T2),
  nth0(X, NewBoard, NewRow, T1).

movmessage('x', 'white to move: ').
movmessage('o', 'black to move: ').

computermessage('x', 'Computer1 just playing ').
computermessage('o', 'Computer2 just playing ').

switch('x', 'o').
switch('o', 'x').

% --- Game Modes ---

startHvHGame  :- init(Board), playloopHH('x', Board).
startHvCGame  :- init(Board), playloopHC('x', Board, randomplayer).
startHvSCGame :- init(Board), playloopHC('x', Board, smartrandomplayer).
startSCvSCGame :- init(Board), playloopCC('x', Board, smartrandomplayer).

% --- Computer Interaction ---
freecells(Board, [X, Y]) :-
  nth0(X, Board, Row),
  nth0(Y, Row, ' ').

randomplay(Board, X, Y) :-
  findall(C, freecells(Board, C), L),
  random_member([X, Y], L).

randomplayer(P, Board, NewBoard) :-
  randomplay(Board, X, Y),
  move(P, Board, X, Y, NewBoard).

% Win positions
winpositions(P, Board, [X, Y]) :-
  freecells(Board, [X, Y]),
  move(P, Board, X, Y, NewBoard),
  haswon(P, NewBoard).

% Non-lost positions
nonlostpositions(P, Board, [X, Y]) :-
  freecells(Board, [X, Y]),
  move(P, Board, X, Y, NewBoard),
  \+ haslost(P, NewBoard),
  switch(P, P2),
  \+ winpositions(P2, NewBoard, _).

smartrandomplayer(P, Board, NewBoard) :-
  setof(C, winpositions(P, Board, C), L),
  random_member([X, Y], L),
  move(P, Board, X, Y, NewBoard).

smartrandomplayer(P, Board, NewBoard) :-
  setof(C, nonlostpositions(P, Board, C), L),
  random_member([X, Y], L),
  move(P, Board, X, Y, NewBoard).

smartrandomplayer(P, Board, NewBoard) :-
  randomplayer(P, Board, NewBoard).

% --- Human Player Interaction ---
humanplayer(P, Board, NewBoard) :-
  padding(Pad),
  displayBoard(Board, Pad),
  nl,nl,
  movmessage(P, M), write(M),
  getCoordinate(X, Y), nl,
  move(P, Board, X, Y, NewBoard).

humanplayer(P, Board, NewBoard) :-
  write('Invalid move. Try again.'), nl,
  humanplayer(P, Board, NewBoard).

% --- Main Game Loop ---
playloopHH(_, Board) :- haswon('x', Board), padding(Pad), displayBoard(Board, Pad),nl, write('White won!!'),  nl.
playloopHH(_, Board) :- haswon('o', Board), padding(Pad), displayBoard(Board, Pad),nl, write('Black won!!'),  nl.
playloopHH(_, Board) :- haslost('x', Board), padding(Pad), displayBoard(Board, Pad),nl, write('White lost!!'), nl.
playloopHH(_, Board) :- haslost('o', Board), padding(Pad), displayBoard(Board, Pad),nl, write('Black lost!!'), nl.

playloopHH(Player, Board) :-
  humanplayer(Player, Board, B1),
  switch(Player, NewPlayer), !,
  playloopHH(NewPlayer, B1).

% ---- Human vs Computer Loop ----
playloopHC(_, Board, _) :- haswon('x', Board),  write('Player has won!!'), nl.
playloopHC(_, Board, _) :- haswon('o', Board),  write('Computer has won!!'), nl.
playloopHC(_, Board, _) :- haslost('x', Board), write('Player has lost!!'), nl.
playloopHC(_, Board, _) :- haslost('o', Board), write('Computer has lost!!'), nl.

playloopHC('o', Board, Strategy) :-
  call(Strategy, 'o', Board, B1), !,
  playloopHC('x', B1, Strategy).

playloopHC('x', Board, Strategy) :-
  humanplayer('x', Board, B1), !,
  playloopHC('o', B1, Strategy).

 %------- Computer vs Computer Loop --------
playloopCC(_, Board, _) :- haswon('x', Board),  write('Computer1 has won!!'), nl.
playloopCC(_, Board, _) :- haswon('o', Board),  write('Computer2 has won!!'), nl.
playloopCC(_, Board, _) :- haslost('x', Board), write('Computer1 has lost!!'), nl.
playloopCC(_, Board, _) :- haslost('o', Board), write('Computer2 has lost!!'), nl.


playloopCC(P, Board, Strategy) :-
  pressEnterToContinue,
  call(Strategy, P, Board, B1),
  padding(Pad),
  displayBoard(B1, Pad),
  nl,
  computermessage(P,M),write(M),
  nl,!,
  switch(P,P1),
  playloopCC(P1, B1, Strategy).


start:-
  gameModeMenu.
