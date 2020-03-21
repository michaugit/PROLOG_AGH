pod(K1, K2) :- 
    na(K2,K1).


%-----------------------------------------
nad(K1,K2):-
    na(K1,K2).

nad(K1, K2) :-
    na(K1,K1s),
    nad(K1s,K2).


%-----------------------------------------
poniżej(K1, K2) :- 
    nad(K2,K1).


%-----------------------------------------
na_lewo(s1,s2).   
na_lewo(s2,s3).
na_lewo(s1,s3).

na_lewo(K1,K2):-
    stół(K1),
    \+ stół(K2),
    pod(K2s,K2),
    na_lewo(K1,K2s).

na_lewo(K1,K2):-
    \+ stół(K1),
    stół(K2),
    pod(K1s,K1),
    na_lewo(K1s,K2).

na_lewo(K1,K2) :-
    \+ stół(K1),
    \+ stół(K2),
    pod(K1s,K1),
    pod(K2s,K2),
    na_lewo(K1s,K2s).


%-----------------------------------------
na_prawo(K1, K2) :- 
    na_lewo(K2,K1).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
jest_przodkiem(X,Y) :- jest_rodzicem(X,Y).
jest_przodkiem(X,Y) :- jest_rodzicem(Z,Y),
                       jest_przodkiem(X,Z).
:- dynamic krewni/2.
jest_krewnym(X,Y) :- 
    				jest_przodkiem(Z,X),
                    jest_przodkiem(Z,Y),
    				X \= Y,
    				\+ krewni(X,Y),
    				\+ krewni(Y,X),
					assert(krewni(X,Y)).


%-----------------------------------------  			
% jest_rodzicem(imię rodzica, imię dziecka)
jest_rodzicem(kasia,robert).
jest_rodzicem(kasia,michał).
jest_rodzicem(tomek,robert).
jest_rodzicem(tomek,eliza).
jest_rodzicem(robert,miriam).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
peano_number(zero,0).
peano_number(s(X), Y) :-
    peano_number(X,Ys),
    Y is Ys + 1.


%-----------------------------------------
peano_add(X,zero,X).
peano_add(X, s(Y), s(Z)) :-
    peano_add(X,Y,Z).


%-----------------------------------------
peano_minus(X,zero,X).
peano_minus(s(X),s(Z), R):-
    peano_minus(X,Z,R).


%-----------------------------------------
peano_times(X,zero,zero).
peano_times(X, s(Y), Z) :-
    peano_times(X,Y,Zs),
    peano_add(Zs,X,Z).


%-----------------------------------------
peano_divide(X,Y,R):-
    peano_times(R,Y,X).


%-----------------------------------------
gt(s(X),zero):-true.
gt(zero,s(Y)):-false.
gt(s(X),s(Y)):-
    gt(X,Y).


%-----------------------------------------
peano_gte(zero,zero):-!,true.
peano_gte(s(_),zero):-!,true.
peano_gte(zero,s(_)):-!,false.
peano_gte(s(X),s(Y)):-
    peano_gte(X,Y).


%-----------------------------------------
peano_mod(_, zero, zero).
peano_mod(zero, _ , zero).
peano_mod(_, s(zero),zero).
peano_mod(X, Y, R):- 
    (   peano_gte(X,Y) ->  peano_minus(X,Y,Rs), peano_mod(Rs,Y,R) ; R=X).


%-----------------------------------------
peano_is_parzysta(zero):-!,true.
peano_is_parzysta(s(zero)):-!,false.
peano_is_parzysta(X):-
    peano_minus(X,s(s(zero)),R),
    peano_is_przysta(R).


%-----------------------------------------
peano_is_nieparzysta(zero):-!,false.
peano_is_nieparzysta(s(zero)):-!,true.
peano_is_nieparzysta(X):-
    peano_minus(X,s(s(zero)),R),
    peano_is_nieparzysta(R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
factorial(0,1).
factorial(Number, Result) :-
    Number > 0,
    N is Number - 1,
    factorial(N,R),
    Result is R * Number.
 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic jest_znajomym/1.

main :- 
    read_command(Command),
    process_command(Command),
    main.

read_command(Command) :- read(Command).

process_command(help) :- 
    writeln("Lista komend:"),
    writeln("- 'add' - dodaj znajomego"),
    writeln("- 'del' - usuń znajomego"),
    writeln("- 'list' - wypisz wszystkich znajomych"),
    writeln("- 'help' - wyświetl listę komend"),
    writeln("- 'exit' - zakończ program").    


%-----------------------------------------
process_command(add) :-
    writeln('Podaj imię znajomego do zapamiętania:'),
    read(Imie),
    dodaj_znajomego(Imie).

dodaj_znajomego(Imie) :-
    jest_znajomym(Imie),
    format('Znam już kogoś o imieniu: ~w\n', [Imie]).
dodaj_znajomego(Imie) :-
    \+ jest_znajomym(Imie),
    assert(jest_znajomym(Imie)),
    writeln('Nowy znajomy zapamiętany!').


%-----------------------------------------
process_command(del) :-
    writeln('Podaj imię znajomego do zapomnienia:'),
    read(Imie),
    usun_znajomego(Imie).

    usun_znajomego(Imie) :-
    \+ jest_znajomym(Imie),
    format('Nie znam nikogo o imieniu: ~w\n', [Imie]).
usun_znajomego(Imie) :-
    jest_znajomym(Imie),
    retract(jest_znajomym(Imie)),
    writeln('Usunięto znajomego').


%-----------------------------------------
process_command(list) :-
    writeln('Znajomi:'),
    fail.
process_command(list) :-
    %foreach(jest_znajomym(Imie),writeln(Imie)). % wersja bez faila 
    jest_znajomym(Imie),
    format('- ~w', [Imie]),
    fail.
process_command(list).


%-----------------------------------------
process_command(exit) :- fail.
process_command(CMD) :-
    \+ known_command(CMD),
    format('Nie znam komendy: ~w\n', [CMD]).


%-----------------------------------------
known_command(add). known_command(del). known_command(exit).
known_command(list). known_command(help).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1. listę autorów książek, bez duplikatów
findall(Autor,
        ksiazka(_,_,autor(Autor,_),_),
        Autorzy),
sort(Autorzy,AutorzyBezDuplikatów).

% 2. listę tytułów książek, które zostały wydane po śmierci swojego autora
findall( Tytuł,
       ( ksiazka(_,Tytuł,autor(_,_-Smierc),wydanie(_,Wydanie)), Wydanie > Smierc ),
        Tytuły).

% 3.  listę par o postaci <imię autora> - <lista napisanych przez niego książek>.
findall(Autor-Tytuły,
        bagof(Tytul,(A,B,C)^ksiazka(A,Tytul,autor(Autor,C),B),Tytuły), % (A,B,C)^ ignoruje nieinteresujące zmienne w zapytaniu
        Lista).

        % Bagof znajduje listy obiektów, dla których zapytania zwracają dokładnie to samo, z wyjątkiem zmiennej, która ma wylądować w liście wynikowej,
        % Zmienne występujące w nawiasach przed daszkiem ^ mówią, żeby bagof ignorował różnice wartości tych zmiennych pomiędzy poszczególnymi wynikami zapytania


% 4. listę par autorów, którzy mogli się spotkać za swojego życia - podpowiedź: warto zdefiniować osobno predykat sprawdzający, czy dwa okresy czasu nachodzą na siebie
:-dynamic autorzy/2.

sprawdz_date(U1-S1,U2-S2):-
    U2 =< S1,
    S2 >= U1;
    U1 =< S2,
    S1 >= U2.

findall(Autor1-Autor2,
        (ksiazka(_,_,autor(Autor1,Data1),_),
            ksiazka(_,_,autor(Autor2,Data2),_), 
            sprawdz_date(Data1,Data2),
            Autor1 \= Autor2, 
            \+ autorzy(Autor1,Autor2),
            \+autorzy(Autor2,Autor1),
            assert(autorzy(Autor1,Autor2))),
        ListaPar).

% 5. imię autora, który żył najdłużej - podpowiedź: proszę zastosować predykat foldl/4 do znalezienia maksymalnego elementu w liście
oblicz_wiek(U-S,Wiek):-
    Wiek is S - U.

list_maximum([H|T],Max):- 
    foldl(maximum,T,H,Max).

maximum(X,Y,Max):-
    X > Y -> Max is X ; Max is Y. 


findall(Lata,
        (ksiazka(_,_,autor(_,Daty),_),
            oblicz_wiek(Daty,Lata)),
        Lista), 
list_maximum(Lista,Max),
findall(Autor-Ile,
        (ksiazka(_,_,autor(Autor,Data),_),
            oblicz_wiek(Data,Ile),
            Ile=Max), 
        MASTER).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
list_number([],0).
list_number([H|T],Y) :-
    list_number(T,Ys),
    Y is Ys + 1.


%-----------------------------------------
list_add([],L2,L2).

list_add([H|T], L2, [H|L3]) :-
    list_add(T,L2,L3).


%-----------------------------------------
sumuj(_,B,X):- X is B + 1.

list_number_mfr(L, Length) :- foldl(sumuj,L,0,Length).


%-----------------------------------------
add(X,L,[X|L]).

list_add_mfr(L1, L2, L3) :- 
    foldl(add,L1,[],L1r),
    foldl(add,L1r,L2,L3).


%-----------------------------------------
usuń(_, [], []).

usuń(Term, [Term|Tail], Tail).

usuń(Term, [Head|Tail], [Head|Result]) :-
  Term \= Head,
  usuń(Term, Tail, Result).


%-----------------------------------------
usuń_wszystkie(_, [], []).

usuń_wszystkie(Term, [Term|Tail], Result):-
    usuń_wszystkie(Term, Tail, Result).

usuń_wszystkie(Term, [Head|Tail], [Head|Result]) :-
  Term \= Head,
  usuń_wszystkie(Term, Tail, Result).


%-----------------------------------------
dodaj_na_dowolnej(X,L,R):-
    usuń(X,R,L).

    
%-----------------------------------------
palindrom(X):- reverse(X,X).


%-----------------------------------------
podlista([],[]).
podlista([First|Rest],[First|Sub]):-
    podlista(Rest,Sub).
podlista([_|Rest],Sub):-
    podlista(Rest,Sub).


%-----------------------------------------
odetnij_z_lewej(Ile,L,Result):-
    length(List,Ile),
    append(List,Result,L).
odetnij_z_prawej(Ile,L,Result):-
    length(List,Ile),
    append(Result,List,L).


%-----------------------------------------
zawiera(L,X):-
    append([_,X,_],L).


%-----------------------------------------
usuń(Term, [Term|Tail], Tail).
usuń(Term, [Head|Tail], [Head|Result]) :-
  Term \= Head,
  usuń(Term, Tail, Result).

permutacja([], []).
permutacja([H|T], X) :- 
	permutacja(T, TX), 
	usuń(H, X, TX).


%-----------------------------------------
podziel([X], [X], []).
podziel([X,Y], [X], [Y]).
podziel([X,Y|A], [X|B], [Y|C]) :- podziel(A, B, C).  


%-----------------------------------------
parzyste(1):- !, false.
parzyste(0):- !, true.
parzyste(X):-
    Xs is X - 2,
    parzyste(Xs).

podziel(List,L,P):-
    length(List,ListTH),
    (   parzyste(ListTH) ->  (   LTH is ListTH /2) ; (   LTH is (ListTH + 1 )/2 )),
    PTH is (ListTH-LTH),
    length(L,LTH),
    length(P,PTH),
    append(L,P,List).

 %lub 

podziel(A, L1, L2) :- 
	append(L1, L2, A), 
	length(L1, N), 
	length(L2, N).

podziel(A, L1, L2) :- 
	append(L1, L2, A), 
	length(L1, N), 
	length(L2, N1), 
	N1 is N+1.


%-----------------------------------------
length_diff(X,0):-  is_empty_diff(X),!.
length_diff([_|T]-E, X):-
    length_diff(T-E, Xs),
    X is Xs + 1.


member_diff(_,L):- is_empty_diff(L),!, false.
    
member_diff(X,[X|T]-E):- !, true.

member_diff(X,[_|T]-E):-
    member_diff(X,T-E).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cwiczenie 1
%----------------------------------------- 
not(Q) :- call(Q), !, false; true.


%----------------------------------------- 
ifelse(Warunek,Prawda,Falsz):- call(Warunek),!,Prawda;Falsz.


%----------------------------------------- 
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


 %-----------------------------------------   
func_call(Fun,Wynik):-
    Fun =.. List,
    append(List,[Wynik],X),
    Zapytanie =.. X,
    Zapytanie.


%-----------------------------------------   
func_call(Fun,Gdzie,X):-
    Fun =.. List,
    insert(X,Gdzie,List,L),
    Zapytanie =.. L,
    Zapytanie.


%-----------------------------------------   
    klucz_mniejszy_od_wartosci(K-V) :- K < V.


    include_assoc(_,X,X):-
        empty_assoc(X),!.

    include_assoc(Filtr,Tab,Wynik):-
        del_min_assoc(Tab,Key,Value,Tab2),
        (call(Filtr,Key-Value)-> (include_assoc(Filtr,Tab2,Wynik2),put_assoc(Key,Wynik2,Value,Wynik));
        include_assoc(Filtr,Tab2,Wynik)).


%-----------------------------------------   
dodaj(X,Y,Z) :- Z is X + Y.


reduce(_, [] ,E, E).

reduce(Predykat, [H|T], P, Sum) :-
    call(Predykat, H, P, Ans),
    reduce(Predykat, T, Ans,Sum).


%----------------------------------------- 
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



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
func_call(Fun/Arg/I,Argumenty,Wynik):-
    !,
    List=[Fun|Argumenty],
    nth0(I,List2,Wynik,List),
    Zapytanie =.. List2,
    Zapytanie.

func_call(Fun/Arg,Argumenty,Wynik):-
    !,
    func_call(Fun/Arg/Arg,Argumenty,Wynik).


func_call(Fun,Argumenty,Wynik):-
    !,
    current_predicate(Fun/Arg),
    func_call(Fun/Arg/Arg,Argumenty,Wynik).


%-----------------------------------------
:- op(800, xfx, <#).
:- op(700, xfy, #).
:- op(400, fy, #).


curry_call(function(Fun/Arg/I,X),Dodaj,Wynik):-
    append(X,[Dodaj],Y),
    ArgsWithoutResult is Arg-1,
    (length(Y,ArgsWithoutResult) -> 
        func_call(Fun/Arg/I,Y,Wynik);
        Wynik=function(Fun/Arg/I,Y)).

curry_call(function(Fun/Arg,X),Dodaj,Wynik):-
    !,
    curry_call(function(Fun/Arg/Arg,X),Dodaj,Wynik).

curry_call(function(Fun,X),Dodaj,Wynik):-
    !,
    current_predicate(Fun/Arg),
    curry_call(function(Fun/Arg/Arg,X),Dodaj,Wynik).


%-----------------------------------------
Result <# Function # ArgsChain :-
    initial_function(Function, IF),
    curry(IF, ArgsChain, Result).

initial_function(function(F,A), function(F,A)) :-
    !.
initial_function(F, function(F, [])).

%##########################################
%-----------------------------------------                   
% curry(F, A # T, Result) :-
%     !,
%     curry_call(F,A,X),
%     curry(X,T,Result).
%     % przypadek, gdy jeszcze nie doszliśmy do końca argumentów

% curry(F, A, Result):-
%     curry_call(F,A,Result).
%     % przypadek, gdy już doszliśmy do końca argumentów
%-----------------------------------------
%#########################################


curry(F, A # T, Result) :-
    !,
    curry_unpack(A, UA),
    curry_call(F, UA, Res),
    curry(Res, T, Result).
   
curry(F, A, Result) :-
    curry_unpack(A, UA),
    curry_call(F, UA, Result).
   
curry_unpack(#(F # Args), AU) :-
    !,
    initial_function(F, IF),
    curry(IF, Args, AU).
curry_unpack(A, A).
%-----------------------------------------




