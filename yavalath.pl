board([e,e,e,e,e, e,'1','2','3','4','5',e,e,e,e],
      [e,e,e,e,'A',s,s,s,s,s,'6',e,e,e,e],
      [e,e,e,'B',s,s,s,s,s,s,s,'7',e,e,e],
      [e,e,'C',s,s,s,s,s,s,s,s,s,'8',e,e],
      [e,'D',s,s,s,s,s,s,s,s,s,s,s,'9',e],
      ['E',s,s,s,s,s,s,s,s,s,s,s,s,s,e],
      [e,'F',s,s,s,s,s,s,s,s,s,s,s,e,e],
      [e,e,'G',s,s,s,s,s,s,s,s,s,e,e,e],
      [e,e,e,'H',s,s,s,s,s,s,s,e,e,e,e],
      [e,e,e,e,'I',s,s,s,s,s,e,e,e,e,e]
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


translate(s, ' ').
translate(e, '#').
