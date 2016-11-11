:- use_module(library(lists)).
:- use_module(library(system)).
:- include('logic.pl').
:- include('utilities.pl').

yavalath:-
  gameModeMenu.

printBoard:-
  initialBoard(Board),
  display_board(Board).

printGameMenu:-
  write('================================='), nl,
	write('=            Yavalath           ='), nl,
	write('================================='), nl,
	write('=                               ='), nl,
	write('=   1. Player vs. Player        ='), nl,
	write('=   2. Player vs. Computer      ='), nl,
	write('=   3. Computer vs. Computer    ='), nl,
  write('=   4. Quit                     ='), nl,
	write('=                               ='), nl,
	write('================================='), nl,
	write('Choose an option:'), nl.

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
	testBoard(Board),
	Game = [Board,whiteP,pvp], !.
	
playGame(Game):-
	printBoard.
	
%%%%%%%%%	
changePlayer(Game,NewGame):-
	getPlayerName(Game,Player),
	(
	     Player == whiteP ->
			NextPlayer = blackP;
		NextPlayer = whiteP
	
	),
	setGamePlayerTurn(NextPlayer,Game,NewGame).
	
%%%%%%%
find([],N) :-
    write('There is no such element in the list'),nl.


find([Element|List],N, I,Result) :-
	(N = I),
	Result = Element;
	(I1 is I+1,
    find(List,N, I1,Result)).
	
findInBoard([Element|List],C,L,Result):-
	find([Element|List],L,1,Linha),
	(find(Linha,C,1,Result));
	 (L1 is L-1,
	 findInBoard(Element,L1,1,Linha)).
	 
	
replace([X|Xs],1,Z,[Z|Xs]).
replace([X|Xs],N,Z,[X|Zs]):-
 N1 is N-1,
 replace(Xs,N1,Z,Zs).
 
 
 
 putPiece(Board,Player,C,L,NewBoard):-
 find(Board,C,1,Linha),
 replace(Linha,L,w,NewBoard1),
 replace(Board,C,NewBoard1,NewBoard).
	

%%%%%%%%%%5
teste:-
	testBoard(Board),
	findInBoard(Board,5,4,Result),
	putPiece(Board,Player,5,4,NewBoard),
	display_board(NewBoard).
	
	
setGamePlayerTurn(Player, Game, NewGame):-
		getListElemAt(1,Player,Game, NewGame).




getPlayerTurn(Game,Player):-
	getListElemAt(1,Game,Player).	

	


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

