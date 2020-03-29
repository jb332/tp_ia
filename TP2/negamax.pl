	/*
	Ce programme met en oeuvre l'algorithme Minmax (avec convention
	negamax) et l'illustre sur le jeu du TicTacToe (morpion 3x3)
	*/
	
:- [tictactoe].


	/****************************************************
  	ALGORITHME MINMAX avec convention NEGAMAX : negamax/5
  	*****************************************************/

	/*
	negamax(+J, +Etat, +P, +Pmax, [?Coup, ?Val])

	SPECIFICATIONS :

	retourne pour un joueur J donne, devant jouer dans
	une situation donnee Etat, de profondeur donnee P,
	le meilleur couple [Coup, Valeur] apres une analyse
	pouvant aller jusqu'a la profondeur Pmax.

	Il y a 3 cas a decrire (donc 3 clauses pour negamax/5)
	
	1/ la profondeur maximale est atteinte : on ne peut pas
	developper cet Etat ; 
	il n'y a donc pas de coup possible a jouer (Coup = rien)
	et l'evaluation de Etat est faite par l'heuristique.

	2/ la profondeur maximale n'est pas  atteinte mais J ne
	peut pas jouer ; au TicTacToe un joueur ne peut pas jouer
	quand le tableau est complet (totalement instancie) ;
	il n'y a pas de coup a jouer (Coup = rien)
	et l'evaluation de Etat est faite par l'heuristique.

	3/ la profondeur maxi n'est pas atteinte et J peut encore
	jouer. Il faut evaluer le sous-arbre complet issu de Etat ; 

	- on determine d'abord la liste de tous les couples
	[Coup_possible, Situation_suivante] via le predicat
	 successeurs/3 (deja fourni, voir plus bas).

	- cette liste est passee a un predicat intermediaire :
	loop_negamax/5, charge d'appliquer negamax sur chaque
	Situation_suivante ; loop_negamax/5 retourne une liste de
	couples [Coup_possible, Valeur]

	- parmi cette liste, on garde le meilleur couple, c-a-d celui
	qui a la plus petite valeur (cf. predicat meilleur/2);
	soit [C1,V1] ce couple optimal. Le predicat meilleur/2
	effectue cette selection.

	- finalement le couple retourne par negamax est [Coup, V2]
	avec : V2 is -V1 (cf. convention negamax vue en cours).

A FAIRE : ECRIRE ici les clauses de negamax/5
.....................................
	*/

	% profondeur max atteinte, evalutation de la valeur du coup avec l'heuristique
	negamax(J, Etat, Pmax, Pmax, [_, Val]) :-
		heuristique(J, Etat, Val).

	% pat
	negamax(J, Etat, _, _, [_, Val]) :-
		situation_terminale(J, Etat),
		heuristique(J, Etat, Val).

	% boucle
	negamax(J, Etat, P, Pmax, [Coup, Ret_Val]) :-
		successeurs(J,Etat,Succ),			% renvoie la liste des successeurs sous la forme [Coup, Successeur]
		loop_negamax(J, P, Pmax, Succ, Liste_Couples),	% appelle negamax sur tous les successeurs et récupère la liste des valeurs des successeurs sous la forme [Coup, valeur situation suivante]
		meilleur(Liste_Couples, [Coup, Val]),		% récupère la valeur maximale et son coup correspondant
		Ret_Val is -Val.				% renvoie l'opposé de cette valeur

	/*******************************************
	 DEVELOPPEMENT D'UNE SITUATION NON TERMINALE
	 successeurs/3 
	 *******************************************/

	 /*
   	 successeurs(+J,+Etat, ?Succ)

   	 retourne la liste des couples [Coup, Etat_Suivant]
 	 pour un joueur donne dans une situation donnee 
	 */

successeurs(J,Etat,Succ) :-
	copy_term(Etat, Etat_Suiv),
	findall([Coup,Etat_Suiv],
		    successeur(J,Etat_Suiv,Coup),
		    Succ).

	/*************************************
         Boucle permettant d'appliquer negamax 
         a chaque situation suivante :
	*************************************/

	/*
	loop_negamax(+J,+P,+Pmax,+Successeurs,?Liste_Couples)
	retourne la liste des couples [Coup, Valeur_Situation_Suivante]
	a partir de la liste des couples [Coup, Situation_Suivante]
	*/

loop_negamax(_,_, _  ,[],                []).
loop_negamax(J,P,Pmax,[[Coup,Suiv]|Succ],[[Coup,Vsuiv]|Reste_Couples]) :-
	loop_negamax(J,P,Pmax,Succ,Reste_Couples),
	adversaire(J,A),
	Pnew is P+1,
	negamax(A,Suiv,Pnew,Pmax, [_,Vsuiv]).

	/*

A FAIRE : commenter chaque litteral de la 2eme clause de loop_negamax/5,
	en particulier la forme du terme [_,Vsuiv] dans le dernier
	litteral ?


	

	*/

	/*********************************
	 Selection du couple qui a la plus
	 petite valeur V 
	 *********************************/

	/*
	meilleur(+Liste_de_Couples, ?Meilleur_Couple)

	SPECIFICATIONS :
	On suppose que chaque element de la liste est du type [C,V]
	- le meilleur dans une liste a un seul element est cet element
	- le meilleur dans une liste [X|L] avec L \= [], est obtenu en comparant
	  X et Y,le meilleur couple de L 
	  Entre X et Y on garde celui qui a la petite valeur de V.

A FAIRE : ECRIRE ici les clauses de meilleur/2
	*/

	meilleur([], Meilleur_Couple, Meilleur_Couple).			% cas trivial : fin de la liste, on renvoie le meilleur couple

	meilleur([[Coup, Valeur] | Tail], [_, MValeur], Final) :-	% cas mise à jour du meilleur coup
		Valeur < MValeur,
		meilleur(Tail, [Coup, Valeur], Final).

	meilleur([_ | Tail], Meilleur_Couple, Final) :-			% cas pas de mise à jour du meilleur coup
		meilleur(Tail, Meilleur_Couple, Final).
		
	meilleur(Liste_de_Couples, Meilleur_Couple) :-
		meilleur(Liste_de_Couples, [[0,0], 42000001], Meilleur_Couple).

	/******************
  	PROGRAMME PRINCIPAL
  	*******************/

main(B, V, Pmax) :-
	situation_initiale(M),
	joueur_initial(J),
	negamax(J, M, 1, Pmax, [B, V]).

test_main(Pmax, N) :-
	Pmax > N.

test_main(Pmax, N) :-
	write('Pmax = '),
	writeln(Pmax),
	main([L, C], V, Pmax),
	nl,
	write('L = '),
	writeln(L),
	nl,
	write('C = '),
	writeln(C),
	nl,
	write('Valeur = '),
	writeln(V),
	nl,
	nl,
	Pmax_new is Pmax + 1,
	test_main(Pmax_new, N),
	!.

test_main(N) :-	% N : profondeur maximale maximale testée
	test_main(1, N).

test_main :-
	test_main(7).

	/*
A FAIRE :
	Compléter puis tester le programme principal pour plusieurs valeurs de la profondeur maximale.
	Pmax = 1, 2, 3, 4 ...
	Commentez les résultats obtenus.
	*/

