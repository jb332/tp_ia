%:- lib(listut).       % Placer cette directive en commentaire si vous utilisez Swi-Prolog 
   
                      % Sinon ne pas modifier si vous utilisez ECLiPSe Prolog :
                      % -> permet de disposer du predicat nth1(N, List, E)
                      % -> permet de disposer du predicat sumlist(List, S)
                      % (qui sont predefinis en Swi-Prolog)

                      
%***************************
%DESCRIPTION DU JEU DU TAKIN
%***************************

   %********************
   % ETAT INITIAL DU JEU
   %********************   
   % format :  initial_state(+State) ou State est une matrice (liste de listes)


initial_state([ [b, h, c],       % C'EST L'EXEMPLE PRIS EN COURS
                [a, f, d],       % 
                [g,vide,e] ]).   % h1=4,   h2=5,   f*=5

/*
initial_state([ [a, b, c, d],
                [e, f, g, h],
                [i, j, k, l],
                [m, n, o, vide] ]).
*/

% AUTRES EXEMPLES POUR LES TESTS DE  A*

/*
initial_state([ [ a, b, c],        
                [ g, h, d],
                [vide,f, e] ]). % h2=2, f*=2


initial_state([ [b, c, d],
                [a,vide,g],
                [f, h, e]  ]). % h2=10 f*=10

initial_state([ [f, g, a],
                [h,vide,b],
                [d, c, e]  ]). % h2=16, f*=20
		
initial_state([ [e, f, g],
                [d,vide,h],
                [c, b, a]  ]). % h2=24, f*=30

initial_state([ [a, b, c],
                [g,vide,d],
                [h, f, e]]). % etat non connexe avec l'etat final (PAS DE SOLUTION)  
*/

   %******************
   % ETAT FINAL DU JEU
   %******************
   % format :  final_state(+State) ou State est une matrice (liste de listes)
   
final_state([[a, b,  c],
             [h,vide, d],
             [g, f,  e]]).
/*
final_state([ [a, b, c, d],
                [l, m, n, e],
                [k, vide, o, f],
                [j, i, h, g] ]).
*/
			 
   %********************
   % AFFICHAGE D'UN ETAT
   %********************
   % format :  write_state(?State) ou State est une liste de lignes a afficher

write_state([]).
write_state([Line|Rest]) :-
   writeln(Line),
   write_state(Rest).
   

%**********************************************
% REGLES DE DEPLACEMENT (up, down, left, right)             
%**********************************************
   % format :   rule(+Rule_Name, ?Rule_Cost, +Current_State, ?Next_State)
   
rule(up,   1, S1, S2) :-
   vertical_permutation(_X,vide,S1,S2).

rule(down, 1, S1, S2) :-
   vertical_permutation(vide,_X,S1,S2).

rule(left, 1, S1, S2) :-
   horizontal_permutation(_X,vide,S1,S2).

rule(right,1, S1, S2) :-
   horizontal_permutation(vide,_X,S1,S2).

   %***********************
   % Deplacement horizontal
   %***********************
    % format :   horizontal_permutation(?Piece1,?Piece2,+Current_State, ?Next_State)
	
horizontal_permutation(X,Y,S1,S2) :-
   append(Above,[Line1|Rest], S1),
   exchange(X,Y,Line1,Line2),
   append(Above,[Line2|Rest], S2).

   %***********************************************
   % Echange de 2 objets consecutifs dans une liste             
   %***********************************************
   
exchange(X,Y,[X,Y|List], [Y,X|List]).
exchange(X,Y,[Z|List1],  [Z|List2] ):-
   exchange(X,Y,List1,List2).

   %*********************
   % Deplacement vertical
   %*********************
   
vertical_permutation(X,Y,S1,S2) :-
   append(Above, [Line1,Line2|Below], S1), % decompose S1
   delete(N,X,Line1,Rest1),    % enleve X en position N a Line1,   donne Rest1
   delete(N,Y,Line2,Rest2),    % enleve Y en position N a Line2,   donne Rest2
   delete(N,Y,Line3,Rest1),    % insere Y en position N dans Rest1 donne Line3
   delete(N,X,Line4,Rest2),    % insere X en position N dans Rest2 donne Line4
   append(Above, [Line3,Line4|Below], S2). % recompose S2 

   %***********************************************************************
   % Retrait d'une occurrence X en position N dans une liste L (resultat R)
   %***********************************************************************
   % use case 1 :   delete(?N,?X,+L,?R)
   % use case 2 :   delete(?N,?X,?L,+R)   
   
delete(1,X,[X|L], L).
delete(N,X,[Y|L], [Y|R]) :-
   delete(N1,X,L,R),
   N is N1 + 1.



   
   %*******************
   % PARTIE A COMPLETER
   %*******************
   
   %*******************************************************************
   % Coordonnees X(colonne),Y(Ligne) d'une piece P dans une situation U
   %*******************************************************************
	% format : coordonnees(?Coord, +Matrice, ?Element)
	% Définit la relation entre des coordonnees [Ligne, Colonne] et un element de la matrice
	/*
	Exemples
	
	?- coordonnees(Coord, [[a,b,c],[d,e,f]],  e).        % quelles sont les coordonnees de e ?
	Coord = [2,2]
	yes
	
	?- coordonnees([2,3], [[a,b,c],[d,e,f]],  P).        % qui a les coordonnees [2,3] ?
	P=f
	yes
	*/

	
coordonnees([L,C], Mat, Elt) :-
    nth1(L, Mat, Ligne), % récupère la L-ième ligne dans la matrice du taquin
    nth1(C, Ligne, Elt). % récupère le C-ième élément dans cette ligne

											 
   %*************
   % HEURISTIQUES
   %*************
   
heuristique(U,H) :-
    %heuristique1(U, H).  % au debut on utilise l'heuristique 1
    heuristique2(U, H).  % ensuite utilisez plutot l'heuristique 2


   %****************
   %HEURISTIQUE no 1
   %****************
   % Nombre de pieces mal placees dans l'etat courant U
   % par rapport a l'etat final F
   
    heuristique1(U, H) :-
        findall(
            P_U					% on veut regrouper les pieces P mal placees dans List
            ,(
                final_state(Fin),		% Fin : matrice état final
                coordonnees(Coord, U, P_U),	% pour toutes les pieces P_U de U, récupère toutes les coordonnées Coord
                coordonnees(Coord, Fin, P_Fin),	% récupère pour les coordonées Coord d'un pièce P_U de U, la pièce P_Fin ayant les coordonnées Coord dans Fin
		P_U \= P_Fin,			% la clause sera vraie, et donc P_U sera dans List, si ces deux pièces sont différentes c'est-à-dire si P_U est mal placée dans U
		P_U \= vide			% et si P_U est différente de vide
            ),
            List				% le findall met dans List toutes les pièces mal placées dans U (vide inclus)
        ),
	length(List, H).			% H prend la taille de List c'est-à-dire le nombre de pièces mal placées


   %****************
   %HEURISTIQUE no 2
   %****************
   
   % Somme des distances de Manhattan à parcourir par chaque piece
   % entre sa position courante et sa positon dans l'etat final

    manhattan(P, U, M) :-
        final_state(Fin),				% Fin : matrice état final
        coordonnees([L_U, C_U], U, P),			% recupere les coordonnees de la piece P dans la matrice d'état U
        coordonnees([L_Fin, C_Fin], Fin, P),		% recupere les coordonnees de la piece P dans la matrice d'état Fin
        M is (abs(C_Fin - C_U) + abs(L_Fin - L_U)).	% calcule la distance de manhattan entre les coordonnees de P dans U et celles de P dans l'état final Fin
   
    heuristique2(U, H) :-
        findall(
            M,						% on regroupe les distances de manhattan calculées pour chaque pièce dans la liste M_List
            (manhattan(P, U, M), P \= vide),		% calcule la distance de manhattan pour toutes les places P de U, on exclut vide
            M_List					% M_List : liste des distances de manhattan à parcourir par chaque pièce
        ),
        sumlist(M_List, H).				% H prend la somme des distances de manhattan de la liste M_List

