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

