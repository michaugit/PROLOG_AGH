################# I - Rekurencyjne przetwarzanie list#################
#1 Zadanie
---------------------------------------------------
usuń(_, [], []).

usuń(Term, [Term|Tail], Tail).

usuń(Term, [Head|Tail], [Head|Result]) :-
  Term \= Head,
  usuń(Term, Tail, Result).

#2 zadanie
---------------------------------------------------

usuń_wszystkie(_, [], []).

usuń_wszystkie(Term, [Term|Tail], Result):-
    usuń_wszystkie(Term, Tail, Result).

usuń_wszystkie(Term, [Head|Tail], [Head|Result]) :-
  Term \= Head,
  usuń_wszystkie(Term, Tail, Result).

#3 zadanie
---------------------------------------------------
usuń(_, [], []).
usuń(Term, [Term|Tail], Tail).
usuń(Term, [Head|Tail], [Head|Result]) :-
  Term \= Head,
  usuń(Term, Tail, Result).
dodaj(A,B,C) :- usuń(A,C,B).

#4 zadanie
---------------------------------------------------
palindrom(X):-
    reverse(X,X).

#5 zadanie
---------------------------------------------------
podlista([],[]).
podlista([First|Rest],[First|Sub]):-
    podlista(Rest,Sub).
podlista([_|Rest],Sub):-
    podlista(Rest,Sub).
    
#6 zadanie
---------------------------------------------------
odetnij_z_lewej(L, X, R) :-
    length(A, L),
    append(A, R, X).

odetnij_z_prawej(L, X, R) :-
    length(A, L),
    append(R, A, X).

#7 zadanie
---------------------------------------------------
zawiera( List, Sublist ) :-
    append( [_, Sublist, _], List ).

#8 zadanie
---------------------------------------------------
dodaj(Elem, T, [Elem|T]).
dodaj(Elem, [H|T], [H|X]):-
	dodaj(Elem, T, X).

permutacja([], []).
permutacja([H|T], X) :- 
	permutacja(T, TX), 
	dodaj(H, TX, X).

#9 zadanie
---------------------------------------------------
podziel(A, L1, L2) :- 
	append(L1, L2, A), 
	length(L1, N), 
	length(L2, N).

podziel(A, L1, L2) :- 
	append(L1, L2, A), 
	length(L1, N), 
	length(L2, N1), 
	N1 is N+1.

#10 zadanie
---------------------------------------------------
spłaszcz(X,[X]) :- 
	\+ is_list(X).
spłaszcz([],[]).
spłaszcz([X|Xs],Zs) :- 
	spłaszcz(X,Y), 
	spłaszcz(Xs,Ys), 
	append(Y,Ys,Zs).


*********************************************************************
################# II - Listy różnicowe i prosta rekurencja #################
#1 zadanie
---------------------------------------------------
length_diff([]-_, 0):- !.
length_diff([_|E]-_, X):-
	length_diff(E-[], X1),
	X is X1 + 1.

#2 zadanie
---------------------------------------------------
member_diff(A, [A|_]-_):- !.
member_diff(A, [_|E]-_):-
	member_diff(A, E-_).