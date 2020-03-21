% Cwiczenie 1
%-------------------------------------------------------- 
not(Q) :- call(Q), !, false;true.
%-------------------------------------------------------- 
ifelse(Warunek,Prawda,Falsz):- call(Warunek),!,Prawda;Falsz.
%-------------------------------------------------------- 






% Cwiczenie 2
use_module(library(assoc)).

suma(A,B,C) :- C is A + B.

% Może się przydać predykat wkładający elementy do 
% listy na wskazanym indeksie
insert(Element, 0, Lista, [Element|Lista]).
insert(Element, I, [H|T], [H|NT]) :-
    I > 0,
    NI is I - 1,
    insert(Element, NI, T, NT).


 %--------------------------------------------------------   
func_call(Fun,Wynik):-
    Fun =.. List,
    append(List,[Wynik],X),
    Zapytanie =.. X,
    Zapytanie.

%--------------------------------------------------------   
func_call(Fun,Gdzie,X):-
    Fun =.. List,
    insert(X,Gdzie,List,L),
    Zapytanie =.. L,
    Zapytanie.

% func_call(Fun,Gdzie,X):-
%     % wynik na trzeciej pozycji
%     Gdzie=3 ->
%     Fun =.. List,
%     append(List,[X],L),
%     Zapytanie =.. L,
%     Zapytanie;
%     % wynik na drugiej pozycji
%     Gdzie=2 ->
%     Fun =.. List,
%     [Predykat,Arg_1,Arg_2]=List,
%     Arg_1_negative is 0 - Arg_1,
%     Zapytanie =.. [Predykat,Arg_1_negative,Arg_2,X],
%     Zapytanie;
%     % wynik na pierwszej pozycji
%     Fun =.. List,
%     [Predykat,Arg_1,Arg_2]=List,
%     Arg_2_negative is 0 - Arg_2,
%     Zapytanie =.. [Predykat,Arg_1,Arg_2_negative,X],
%     Zapytanie.


%--------------------------------------------------------   
    klucz_mniejszy_od_wartosci(K-V) :- K < V.


    include_assoc(_,X,X):-
        empty_assoc(X),!.

    include_assoc(Filtr,Tab,Wynik):-
        del_min_assoc(Tab,Key,Value,Tab2),
        (call(Filtr,Key-Value)-> (include_assoc(Filtr,Tab2,Wynik2),put_assoc(Key,Wynik2,Value,Wynik));
        include_assoc(Filtr,Tab2,Wynik)).

%--------------------------------------------------------   
dodaj(X,Y,Z) :- Z is X + Y.


reduce(_, [] ,E, E).

reduce(Predykat, [H|T], P, Sum) :-
    call(Predykat, H, P, Ans),
    reduce(Predykat, T, Ans,Sum).

%-------------------------------------------------------- 
:- dynamic findallsol/1.
:- dynamic findal_assoc_sol/2.



findall_assoc(Key,Goal,Solutions) :-
        call(Goal),
        assert(findal_assoc_sol(Key,t)),
        fail.

findall_assoc(Key,Goal,Solutions) :-
        collect_assoc(Solutions).

collect_assoc(Solutions) :-
        retract(findal_assoc_sol(Key,Value)),
        !,
        collect_assoc(RestSols),
        put_assoc(Key,RestSols,Value,Solutions).

collect_assoc(X):-
 empty_assoc(X).


% trakotwanie jako liste, a później konwersja na asocjacyjną
findal(Key,Goal,Solutions) :-
        call(Goal),
        assert(findallsol(Key-t)),
        fail.

findal(Key,Goal,Solutions) :-
        collect(List),
       list_to_assoc(List,Solutions).

collect(Solutions) :-
        retract(findallsol(Template)),
        !,
        Solutions = [Template|RestSols],
        collect(RestSols).
collect([]).

%-------------------------------------------------------- 
























































% stara werjsa :D
% include_assoc(Filtr,Tab,Wynik):-
%         min_assoc(Tab,Key,Value),
%         del_min_assoc(Tab,_,_,Tab1),
%         (call(Filtr,Key-Value) -> 
%             (include_assoc(Filtr,Tab1,Wynik1),
%             put_assoc(Key,Wynik1,Value,Wynik));
%         % else
%         include_assoc(Filtr,Tab1,Wynik1)).