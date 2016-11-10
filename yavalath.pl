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
	initialBoard(Board),
	Game = [Board,whitePlayer,pvp], !.
	
playGame(Game):-
	printBoard.
	
switchPlayer(WhitePlayer, NewPlayer):-
	NewPlayer=BlackPlayer.

switchPlayer(BlackPlayer, NewPlayer):-
	NewPlayer=WhitePlayer.

	


	








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

