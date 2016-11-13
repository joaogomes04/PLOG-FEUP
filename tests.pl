:- include('yavalath.pl').

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