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
      [s,s,s,w,w,s,s,s,s],
      [s,s,s,s,b,s,s,s],
      [s,s,s,s,s,s,s],
      [s,s,s,s,s,s],
      [s,s,s,s,s]
      ]	).



%canMovePiece(Board, Letter, Number)
%movePiece(Bi, Letter, Number, Bf)
