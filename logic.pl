:- use_module(library(lists)).
:- use_module(library(random)).




initialBoard([['               ',' 1 ',' 2 ',' 3 ',' 4 ',' 5 '],
      ['         ',' A ',s,s,s,s,s,' 6 '],
      ['       ',' B ',s,s,s,s,s,s,' 7 '],
      ['     ',' C ',s,s,s,s,s,s,s,' 8 '],
      ['   ',' D ',s,s,s,s,s,s,s,s,' 9 '],
      [' ',' E ',s,s,s,s,s,w,s,s,s],
      ['   ',' F ',s,s,s,w,w,b,s,s],
      ['     ',' G ',s,s,s,b,s,s,s],
      ['       ',' H ',s,s,s,s,s,s],
      ['         ',' I ',s,s,s,s,s]
      ]	).

	  
	  testBoard([
      [s,s,s,s,s],
      [s,s,s,s,s,s],
      [s,s,s,s,s,s,s],
      [s,s,s,s,w,w,s,s],
      [s,s,s,w,w,w,w,s,s],
      [s,s,s,s,b,s,s,s],
      [s,s,s,s,s,s,s],
      [s,s,s,s,s,s],
      [s,s,s,s,s]
      ]	).

		iBoard([
      [s,s,s,s,s],
      [s,s,s,s,s,s],
      [s,s,s,s,s,s,s],
      [s,s,s,s,w,w,s,s],
      [s,s,s,w,w,w,w,s,s],
      [s,s,s,s,b,s,s,s],
      [s,s,s,s,s,s,s],
      [s,s,s,s,s,s],
      [s,s,s,s,s]
      ]	).
	  
	ins(Val,[H|List],Pos,[H|Res]):- 
	Pos > 1, !, 
    Pos1 is Pos - 1, ins(Val,List,Pos1,Res). 
	
     ins(Val, List, 1, [Val|List]).

	   
	   
	 arrangeLine(Board,Line,Head,Tail,NewBoard):-
		find(Board,Line,1,Linha),
		concatenate(Head,Linha,NewLinha),
		concatenate(NewLinha,Tail,NovaLinha),
		replace(Board,Line,NovaLinha,NewBoard).
	   
	   
	 concatenate([],List,List).
    
    concatenate([X|List1],List2,[X|List3]) :-
        concatenate(List1,List2,List3).
		
	 displayBoard(Board):- 
	 arrangeLine(Board,1,['         ',' A '],[' 6 '],FinalBoard1),
	 arrangeLine(FinalBoard1,2,['       ',' B '],[' 7 '],FinalBoard2),
	 arrangeLine(FinalBoard2,3,['     ',' C '],[' 8 '],FinalBoard3),
	 arrangeLine(FinalBoard3,4,['   ',' D '],[' 9 '],FinalBoard4),
	 arrangeLine(FinalBoard4,5,[' ',' E '],[],FinalBoard5),
	 arrangeLine(FinalBoard5,6,['   ',' F '],[],FinalBoard6),
	 arrangeLine(FinalBoard6,7,['     ',' G '],[],FinalBoard7),
	 arrangeLine(FinalBoard7,8,['       ',' H '],[],FinalBoard8),
	 arrangeLine(FinalBoard8,9,['         ',' I '],[],FinalBoard9),
	 ins(['               ',' 1 ',' 2 ',' 3 ',' 4 ',' 5 '],FinalBoard9,1,FinalBoard),
	 display_board(FinalBoard).
	 
	 
%canMovePiece(Board, Letter, Number)
%movePiece(Bi, Letter, Number, Bf)
