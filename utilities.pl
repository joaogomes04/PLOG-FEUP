clearConsole:-
	clearConsole(40), !.

clearConsole(0).
clearConsole(N):-
	nl,
	N1 is N-1,
	clearConsole(N1).

getCoordinate(X, Y) :-
	getCode(X),
	getInt(Y),
	get_char(_).

getChar(Input):-
	get_char(Input),
	get_char(_).

getCode(Input):-
	get_code(TempInput),
	Input is TempInput - 65.

getInt(Input):-
	get_code(TempInput),
	Input is TempInput - 49.

	pressEnterToContinue:-
	write('Press <Enter> to continue.'), nl,
	waitForEnter, !.

waitForEnter:-
	get_char(_).