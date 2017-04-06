% Practica 3 - Alfonso Y David


% Ejercicio 1

pertenece(X,[X|_]).
pertenece(X,[_|L2]):- pertenece(X,L2).

% Ejercicio 2

concatena([], L, L).
concatena([X|L1],L2,[X|L3]):- concatena(L1, L2, L3).

invierte([],[]).
invierte([H|T],L):- invierte(T,R), concatena(R,[H],L).

% Ejercicio 3

preorder(nil, []).
preorder(tree(INFO, LEFT, RIGHT), [INFO|L]) :- preorder(LEFT, LL),
                                               concatena(LL, RL, L),
                                               preorder(RIGHT, RL).
%preorder(tree(1,nil,nil),L).
%preorder(tree(1,tree(2,nil,nil),tree(3,nil,nil)),L).
 
 
 
inorder(nil, []).
inorder(tree(INFO, LEFT, RIGHT), L):- inorder(LEFT,LL),
                                      inorder(RIGHT, RL),
                                      concatena(LL, [INFO|RL],L).
 
%inorder(tree(1,nil,nil),L).
%inorder(tree(1,tree(2,nil,nil),tree(3,nil,nil)),L).
 
postorder(nil, []).
postorder(tree(INFO, LEFT, RIGHT), L):- postorder(LEFT ,LL),
                                        postorder(RIGHT, RL),
                                        concatena(LL, RL,R1),
                                        concatena(R1, [INFO], L).
%postorder(tree(1,nil,nil),L).
%postorder(tree(1,tree(2,nil,nil),tree(3,nil,nil)),L).
										
% Ejercicio 4

insertar(X, L, 1, [X|L]).
insertar(X,[H|L],Pos,[H|R]):- Pos > 1, !, Pos1 is Pos - 1, insertar(X,L,Pos1,R). 


% Ejercicio 5

extract(L1,X,L2):- concatena(L4,L5,L1), concatena([X],L3,L5), concatena(L4,L3,L2).

% Ejercicio 6

man_pref(juan, [maria, carmen, pilar]).
man_pref(pedro, [carmen, maria, pilar]).
man_pref(mario, [maria, carmen, pilar]).

woman_pref(maria, [juan, pedro, mario]).
woman_pref(carmen, [pedro, juan, mario]).
woman_pref(pilar, [juan, pedro, mario]).

% Ejercicio 7

pos(X,[X|_], 1).
pos(X,[_|R],N):- pos(X,R,M), N is M + 1.

unstable(_-_, []) :- not(!).
unstable(M1-W1, [M1-W1]) :- not(!).
unstable(M1-W1, [C|R]):- man_pref(M1, WL1),
    					woman_pref(W1, ML1),
    					pertenece(M2-_, [C]),
    					pertenece(_-W2, [C]),
    					pos(M2, ML1, Pos1),
    					pos(M1, ML1, Pos2),
    					pos(W2,WL1, Pos3),
    					pos(W1,WL1, Pos4),
    					Pos1<Pos2,
    					Pos3<Pos4,
    					unstable(M1-W1, R).

% Ejercicio 8

smp(FreeMen, FreeWoman, MarriagesIn, MarriagesOut):- 
    not(unstable(FreeMen-FreeWoman, MarriagesIn)), 
    concatena(MarriagesIn, [FreeMen-FreeWoman], MarriagesOut).
	
	
