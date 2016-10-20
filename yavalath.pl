:- use_module(library(lists)).


board(['               ',' 1 ',' 2 ',' 3 ',' 4 ',' 5 '],
      ['         ',' A ',s,s,s,s,s,' 6 '],
      ['       ',' B ',s,s,s,s,s,s,' 7 '],
      ['     ',' C ',s,s,s,s,s,s,s,' 8 '],
      ['   ',' D ',s,s,s,s,s,s,s,s,' 9 '],
      [' ',' E ',s,s,s,s,s,s,s,s,s],
      ['   ',' F ',s,s,s,s,s,s,s,s],
      ['     ',' G ',s,s,s,s,s,s,s],
      ['       ',' H ',s,s,s,s,s,s],
      ['         ',' I ',s,s,s,s,s]
      ).

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

translate(s, '   ').
%translate(e, '#').
translate(w, ' o ').
translate(b, ' x ').
