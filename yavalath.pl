board([e,e,e,e,e,s,s,s,s,s,e,e,e,e,e],
      [e,e,e,e,s,s,s,s,s,s,s,e,e,e,e],
      [e,e,e,s,s,s,s,s,s,s,s,s,e,e,e],
      [e,e,s,s,s,s,s,s,s,s,s,s,s,e,e],
      [e,s,s,s,s,s,s,s,s,s,s,s,s,s,e],
      [e,e,s,s,s,s,s,s,s,s,s,s,s,e,e],
      [e,e,e,s,s,s,s,s,s,s,s,s,e,e,e],
      [e,e,e,e,s,s,s,s,s,s,s,e,e,e,e],
      [e,e,e,e,e,s,s,s,s,s,e,e,e,e,e]
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

display_line([]).


translate(s, ' ').
translate(e, '#').
