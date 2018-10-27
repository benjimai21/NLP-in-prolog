%%%%% Natural Language Program

sentence(S) :-
	noun_phrase(NP),
	verb_phrase(VP),
	append(NP, VP, S).

noun_phrase(NP) :-
	article(A),
	noun(N),
	append(A, N, NP).

verb_phrase(V) :-
	verb(V).
verb_phrase(VP) :-
	verb(V),
	noun_phrase(NP),
	append(V, NP, VP).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Question a

%Base case
conj(Text) :- sentence(Text).

%Recursive case
conj(Text) :- 
	append(L1,[and|L2],Text), 
	sentence(L1),
	conj(L2).


%Question b 

%Base case
encode([],[]).

%first recursive case when the head of T is not a noun
encode(T,ET):-
	T = [H|Tail], 
	\+noun([H]), 
	Z = H, 
	encode(Tail,TTail), 
	append([Z],TTail,ET).

%second recursive case when the head of T is a noun
encode(T,ET) :-
	T = [H|Tail], 
	noun([H]),
	anima([H],F), 
	size([H],C), 
	change([H],D), 
	atom_chars(Z,[F,C,D]), 
	encode(Tail,TTail),
	append([Z],TTail,ET).       

%anima(X,F) means that if the noun in list X is animated, F will be assigned letter a and d %otherwise
anima(X,F) :- 
	noun(X), 
	X = [Noun], 
	animate(L), 
	((member(Noun,L), F = a) ; ( \+member(Noun,L), (F = d))).
  
%size(X,C) means that if the noun in list X is less or equal that 3 letters, then F will be %assigned letter s and l otherwise
size(X,C) :- 
	noun(X), 
	X = [Noun], 
	atom_chars(Noun,L), 
	length(L,Y), 
	(((Y =< 3), C = s) ; ((Y > 3), C = l)).

%change(X,D) means that D will be assigned the first letter of the noun in list X
change(X,D) :- 
	noun(X), 
	X = [Noun], 
	atom_chars(Noun,L), 
	L = [E|_], 
	(D = E).


%Question c 
                
%base case
%%in same(Text,Y,Act), Y is an empty list and Act is a list that contains the noun actor in Text. 
same(Text,Y,Act) :-
	noun_phrase(NP),
	verb_phrase(VP),
	append(NP,VP,Text),
	article(A),
	noun(N),
	append(A,N,NP),
	append(N,Y,Act).

%recursive case
%in same(Text,Y,Act), Y is an empty list and Act is a list that contains all the noun actors per bit of sentence. 
same(Text,Y,Act) :-
	noun_phrase(NP),
	verb_phrase(VP),
	append(NP,VP,H),
	append(H,[and|T], Text),
	article(A),
	noun(N),
	append(A,N,NP),
	append(N,Y,Act2),
	same(T,Act2,Act).


%same_actor(Text) tests if the actor in each bit of sentence is the same
same_actor(Text) :-
	same(Text,[],Act),
	setof(Z,member(Z,Act),U),
	length(U,L),
	L is 1.                       %we make sure that there is one single actor



	

                    

 

