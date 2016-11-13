:- use_module(library(lists)).
:- use_module(library(random)).
:- include('utilities.pl').

% --- Initialize board ---
init([['#', '#', ' ', ' ', ' ', ' ', ' ', '#', '#'],
      ['#', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#'],
      ['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
      [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
      [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
      [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
      ['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
      ['#', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#'],
      ['#', '#', ' ', ' ', ' ', ' ', ' ', '#', '#']]).

xlooses(
     [['#', '#', 'x', 'x', 'x', ' ', ' ', '#', '#'],
      ['#', ' ', 'o', ' ', ' ', ' ', ' ', '#', '#'],
      ['#', ' ', ' ', 'o', ' ', ' ', ' ', ' ', '#'],
      [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
      [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
      [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
      ['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
      ['#', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#'],
      ['#', '#', ' ', ' ', ' ', ' ', ' ', '#', '#']]).

xwins(
     [['#', '#', 'x', 'x', 'x', 'x', ' ', '#', '#'],
      ['#', ' ', 'o', ' ', ' ', ' ', ' ', '#', '#'],
      ['#', ' ', ' ', 'o', 'o', ' ', ' ', ' ', '#'],
      [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
      [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
      [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
      ['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
      ['#', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#'],
      ['#', '#', ' ', ' ', ' ', ' ', ' ', '#', '#']]).

olooses(
     [['#', '#', 'x', 'x', ' ', 'x', 'x', '#', '#'],
      ['#', ' ', 'o', ' ', ' ', ' ', ' ', '#', '#'],
      ['#', ' ', ' ', 'o', 'o', 'o', ' ', ' ', '#'],
      [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
      [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
      [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
      ['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
      ['#', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#'],
      ['#', '#', ' ', ' ', ' ', ' ', ' ', '#', '#']]).

owins(
     [['#', '#', 'x', 'x', ' ', 'x', 'x', '#', '#'],
      ['#', 'o', ' ', ' ', ' ', ' ', ' ', '#', '#'],
      ['#', ' ', ' ', 'o', 'o', 'o', 'o', ' ', '#'],
      [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
      [' ', ' ', ' ', ' ', 'x', ' ', ' ', ' ', ' '],
      [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
      ['#', ' ', ' ', ' ', ' ', ' ', ' ', ' ', '#'],
      ['#', ' ', ' ', ' ', ' ', ' ', ' ', '#', '#'],
      ['#', '#', ' ', ' ', ' ', ' ', ' ', '#', '#']]).

sanitycheck :-
  xlooses(B1), haslost('x', B1),
  xwins(B2),   haswon('x', B2),
  olooses(B3), haslost('o', B3),
  owins(B4),   haswon('o', B4).

% --- Game Menu ---
printGameMenu:-
  write('---------------------------------'), nl,
	write('-            Yavalath           -'), nl,
	write('---------------------------------'), nl,
	write('-                               -'), nl,
  write('-   > 1. How to Play            -'), nl,
  write('-   > Game Mode:                -'), nl,
	write('-     2. Player vs. Player      -'), nl,
	write('-     3. Player vs. Computer    -'), nl,
	write('-     4. Computer vs. Computer  -'), nl,
  write('-     5. Quit                   -'), nl,
	write('-                               -'), nl,
	write('---------------------------------'), nl,
	write('Choose an option:'), nl.

<<<<<<< HEAD
  gameModeMenu:-
    printGameMenu,
    getChar(Input),
  	(
  		Input = '1' -> explain, gameModeMenu;
  		Input = '2' -> startHvHGame;
  		Input = '3' -> startHvCGame;
  		Input = '4' -> startCvCGame;
      Input = '5';

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
padding(['    ', '    ', '  ', '  ', '', '  ', '  ', '    ', '    ']).

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

switch('x', 'o').
switch('o', 'x').

% --- Game Modes ---

startHvHGame :- init(Board), playloopHH('x', Board).
startHvCGame :- init(Board), playloopHC('x', Board).
startCvCGame :- nl.

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
playloopHH(_, Board) :- haswon('x', Board),  write('white won!!'),  nl.
playloopHH(_, Board) :- haswon('o', Board),  write('black won!!'),  nl.
playloopHH(_, Board) :- haslost('x', Board), write('white lost!!'), nl.
playloopHH(_, Board) :- haslost('o', Board), write('black lost!!'), nl.

playloopHH(Player, Board) :-
  humanplayer(Player, Board, B1),
  switch(Player, NewPlayer), !,
  playloopHH(NewPlayer, B1).

playloopHC(_, Board) :- haswon('x', Board),  write('player has won!!'), nl.
playloopHC(_, Board) :- haswon('o', Board),  write('computer has won!!'), nl.
playloopHC(_, Board) :- haslost('x', Board), write('player has lost!!'), nl.
playloopHC(_, Board) :- haslost('o', Board), write('computer has lost!!'), nl.

playloopHC('o', Board) :-
  randomplayer('o', Board, B1), !,
  playloopHC('x', B1).

playloopHC('x', Board) :-
  humanplayer('x', Board, B1), !,
  playloopHC('o', B1).

start:-
  gameModeMenu.
=======
gameModeMenu:-
  printGameMenu,
  getChar(Input),
	(
		Input = '1' -> startPvPGame;
		Input = '2' -> startPvMGame;
		Input = '3' -> startMvMGame;
		Input = '4';

		nl,
		write('Error: invalid input.'), nl,
		pressEnterToContinue, nl,
		gameModeMenu
	).

player(whiteP).
player(blackP).


translate(s, '   ').
translate(w, ' o ').
translate(b, ' x ').


getPlayerName(whiteP, 'White').
getPlayerName(blackP, 'Black').

pieceOwnedBy(w, whiteP).
pieceOwnedBy(b, blackP).

startPvPGame:-
	createPvPGame(Game),
	playGame(Game).
startPvMGame:-
	createPvBGame(Game),
	playGame(Game).
startMvMGame:-
	createBvBGame(Game),
	playGame(Game).


	
createPvPGame(Game):-
	iBoard(Board),
	Game = [Board,whiteP,pvp], !.
	
playGame(Game):-
		getBoard(Game,Board),
		displayBoard(Board),
		(write('Choose the line'),nl,
		getCode(Line),
		write('Choose the collumn'),nl,
		getInt(Colum),
		putPiece(Board,'w',Colum,Line,NewBoard),
		displayBoard(NewBoard)),!.
	
	
%%%%%%%%%	
changePlayer(Game,NewGame):-
	getPlayerName(Game,Player),
	(
	     Player == whiteP ->
			NextPlayer = blackP;
		NextPlayer = whiteP
	
	),
	setGamePlayerTurn(NextPlayer,Game,NewGame).
	
	
	
	
	
	
	
	
	%%%%%%
%%%%%%%
find([],N) :-
    write('There is no such element in the list'),nl.

%%%%%%%%%%%%%
find([Element|List],N, I,Result) :-
	(N = I),
	Result = Element;
	(I1 is I+1,
    find(List,N, I1,Result)).
	%%%%%%%%%%%
	
findInBoard([Element|List],C,L,Result):-
	find([Element|List],L,1,Linha),
	(find(Linha,C,1,Result));
	 (L1 is L-1,
	 findInBoard(Element,L1,1,Linha)).
	 
%%%%%%%%
	
replace([X|Xs],1,Z,[Z|Xs]).
replace([X|Xs],N,Z,[X|Zs]):-
 N1 is N-1,
 replace(Xs,N1,Z,Zs).
 
 %%%%%%%% C = coluna da peça jogada, L = linha da peça - 4
	
checkVerticalDL(Board,C,L, NewList,List):-
  (L < 10)->(
	(L < 5) -> (
	
	findInBoard(Board,C,L,Piece)->(
	append(NewList,[Piece],List1),
	L1 is L+1,
	checkVerticalDL(Board,C,L1,List1, List));List = NewList
);
(	findInBoard(Board,C,L,Piece)->(
	append(NewList,[Piece],List1),
	C1 is C-1,
	L1 is L+1,
	checkVerticalDL(Board,C1,L1,List1,List));List = NewList
));	List = NewList.

 
		
			
 
 teste2:-
	iBoard(Board),
	displayBoard(Board),
	checkVerticalDL(Board,5,1,List,List2),
	checkHorizontalWin([List2],w,0,1,Result),
	write(Result),
	write(List2).
 
 
 
 
 
 
 
 
 
 %%%%%% C tem de ir a 1 pq basicamente ele testa a linha toda desde a primira celula ate ao fim 
 checkHorizontalLose(Board,PlayerPiece,C,L,Result):-
 find(Board,L,1,Linha),
 C < 10 ->
 (checkHorizontalFront(Linha,PlayerPiece,C,3)-> Result = 'lose';(
	C1 is C+1,
   checkHorizontalLose(Board,PlayerPiece,C1,L,Result)));
   Result = 'play'.

  
%%%%%%%%

checkHorizontalWin(Board,PlayerPiece,C,L,Result):-
  find(Board,L,1,Linha),
 C < 10 ->
 (checkHorizontalFront(Linha,PlayerPiece,C,4)-> Result = 'win';(
	C1 is C+1,
   checkHorizontalWin(Board,PlayerPiece,C1,L,Result)));
   checkHorizontalLose(Board,w,0,L,Result).

	
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%%%%% N = numero de peças seguidas
checkHorizontalFront(Linha,PlayerPiece,C,N):-
checkHorizontalFront(Linha,PlayerPiece,C,N,0).

checkHorizontalFront(Linha,PlayerPiece,C,N,Counter):-
	C1 is C+1,
	find(Linha,C1,1,Piece),
	(Piece == PlayerPiece )->(
	Counter1 is Counter+1,
	checkHorizontalFront(Linha,PlayerPiece,C1,N,Counter1));Counter == N.
	
 
 %%%%%%Aqui o player vai servir para saber qual a peça a por
 
 putPiece(Board,Piece,C,L,NewBoard):-
 find(Board,L,1,Linha),
 replace(Linha,C,Piece,NewBoard1),
 replace(Board,L,NewBoard1,NewBoard).
	

%%%%%%%%%%para testar as funçoes
teste:-
	testBoard(Board),
	displayBoard(Board),
	putPiece(Board,w,1,1,NewBoard),
	displayBoard(NewBoard).
	
	%%%%%
	teste1:-
	testBoard(Board),
	putPiece(Board,w,6,5,NewBoard),
	checkHorizontalWin(NewBoard,w,0,1,Result),
	
	write(Result).
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	%%%%%%%%%%%%%%%%
setGamePlayerTurn(Player, Game, NewGame):-
		getListElemAt(1,Player,Game, NewGame).




getPlayerTurn(Game,Player):-
	getListElemAt(1,Game,Player).	

getBoard(Game, Board):-
	getListElemAt(0,Game,Board).


getListElemAt(0, [ElemAtTheHead|_], ElemAtTheHead).
getListElemAt(Pos, [_|RemainingElems], Elem):-
	Pos > 0,
	Pos1 is Pos-1,
	getListElemAt(Pos1, RemainingElems, Elem).








%deve estar no final deste file
display_board([L1|Ls]):-
  display_line(L1), nl,
  display_board(Ls).

display_board([]):- nl.

display_line([E|Es]):-
  translate(E,V),
  write(V),
  write('|'),
  display_line(Es).

display_line([E|Es]):-
  write(E),
  write('|'),
  display_line(Es).

display_line([]).


%%%%%%%%%%%%%%%%%%%

>>>>>>> 24da9303c52e1c3a7efd492c2b0e7821164cc1e4
