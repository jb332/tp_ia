	/*********************************
	DESCRIPTION DU JEU DU TIC-TAC-TOE
	*********************************/

	/*
	Une situation est decrite par une matrice 3x3.
	Chaque case est soit un emplacement libre, soit contient le symbole d'un des 2 joueurs (o ou x)

	Contrairement a la convention du tp pr�c�dent, pour mod�liser une case libre
	dans une matrice on n'utilise pas une constante sp�ciale (ex : nil, 'vide', 'libre','inoccupee' ...);
	On utilise plut�t une variable libre (_), c'est�-dire un terme non instanci� ('_').
	La situation initiale est donc une matrice 3x3 composee uniquement de variables libres (_). 
	Ceci est possible car le jeu consiste � instancier la grille avec des symboles et non � d�placer les symbles d�j� affect�s.
	
	
	
	Jouer un coup, c-a-d placer un symbole dans une grille S1 ne consiste pas � g�n�rer une nouvelle grille S2 obtenue 
	en copiant d'abord S1 puis en remplacant le symbole de case libre par le symbole du joueur, mais plus simplement
	� INSTANCIER (au sens Prolog) la variable libre qui repr�sentait la case libre par la valeur associ�e au joueur, ex :
	Case = Joueur, ou a realiser indirectement cette instanciation par unification via un pr�dicat comme member/2, select/3, nth1/3 ...
	
	Ainsi si on joue un coup en S, S perd une variable libre, mais peut continuer � s'appeler S (on n'a pas besoin de la d�signer
	par un nouvel identificateur).
	La situation initiale est une "matrice" 3x3 (liste de 3 listes de 3 termes chacune)
	o� chacun des 9 termes est une variable libre.	
	*/

situation_initiale([ [_,_,_],
                     [_,_,_],
                     [_,_,_] ]).

	% Convention (arbitraire) : c'est x qui commence

joueur_initial(x).


	% Definition de la relation adversaire/2

adversaire(x,o).
adversaire(o,x).


	/****************************************************
	 DEFINIR ICI � l'aide du pr�dicat ground/1 comment
	 reconnaitre une situation terminale dans laquelle il
	 n'y a aucun emplacement libre : aucun joueur ne peut
	 continuer � jouer (quel qu'il soit).
	 ****************************************************/

situation_terminale(_Joueur, Situation) :- ground(Situation).

/***************************
 DEFINITIONS D'UN ALIGNEMENT
 ***************************/

alignement(L, Matrix) :- ligne(    L,Matrix).
alignement(C, Matrix) :- colonne(  C,Matrix).
alignement(D, Matrix) :- diagonale(D,Matrix).

	/********************************************
	 DEFINIR ICI chaque type d'alignement maximal 
 	 existant dans une matrice carree NxN.
	 ********************************************/
	
ligne(L, M) :-
	nth1(_, M, L).			% verifie s'il existe un ligne de M qui vaut L

test_ligne(L) :-
	M = [[a,b,c], [d,e,f], [g,h,i]],
	ligne(L, M).

colonne(C, M) :-
	length(M, N),			% N : taille d'un côté du morpion
	colonne(N, C, M).		% appel à colonne\3, on teste d'abord la colonne N

colonne(N, C, M) :-
	findall(
		E
		,(
			nth1(_, M, L),	% trouve toutes les lignes L de M
			nth1(N, L, E)	% prend le N-ième élément de L
		),
		C					% liste des N-ième éléments de chaque ligne de M <=> N-ième colonne
							% et vérification de la correspondance avec C
	).						% si faux alors on c'est la clause ci-dessous qui est testée et on passe à la colonne suivante (par décrémentation de N) jusqu'à la colonne 1

colonne(N, C, M) :-
	N > 1,
	N_aux is N-1,
	colonne(N_aux, C, M).

test_colonne(C) :-
	M = [[a,b,c], [d,e,f], [g,h,i]],
	colonne(C, M).

	/* D�finition de la relation liant une diagonale D � la matrice M dans laquelle elle se trouve.
		il y en a 2 sortes de diagonales dans une matrice carree(https://fr.wikipedia.org/wiki/Diagonale) :
		- la premiere diagonale (principale) (descendante) : (A I)
		- la seconde diagonale  (ascendante)  : (R Z)
		A . . . . . . . Z
		. \ . . . . . / .
		. . \ . . . / . .
		. . . \ . / . . .
		. . . . X . . .
		. . . / . \ . . . 
		. . / . . . \ . .
		. / . . . . . \ .
		R . . . . . . . I
	*/
		
diagonale(D, M) :- premiere_diag(1,D,M).
diagonale(D, M) :- seconde_diag(1,D,M).

% definition de la premiere diagonale
premiere_diag(_,[],[]).
premiere_diag(K,[E|D],[Ligne|M]) :-
	nth1(K,Ligne,E),
	K1 is K+1,
	premiere_diag(K1,D,M).

% definition de la seconde diagonale
seconde_diag(K, Diag, Matrix) :-
	reverse(Matrix, Reversed_Matrix),
	premiere_diag(K, Diag, Reversed_Matrix).

test_diagonale(D) :-
	M = [[a,b,c], [d,e,f], [g,h,i]],
	diagonale(D, M).

	/***********************************
	 DEFINITION D'UN ALIGNEMENT POSSIBLE
	 POUR UN JOUEUR DONNE
	 **********************************/

possible([X|L], J) :- unifiable(X,J), possible(L,J).
possible([   ], _).

	/* Attention 
	il faut juste verifier le caractere unifiable
	de chaque emplacement de la liste, mais il ne
	faut pas realiser l'unification.
	*/

% A FAIRE 
unifiable(X,_) :-
	var(X).

unifiable(X,J) :-
	X == J.
	
	/**********************************
	 DEFINITION D'UN ALIGNEMENT GAGNANT
	 OU PERDANT POUR UN JOUEUR DONNE J
	 **********************************/

	/*
	Un alignement gagnant pour J est un alignement
possible pour J qui n'a aucun element encore libre.
Un alignement perdant pour J est gagnant
pour son adversaire.
	*/

% A FAIRE

alignement_gagnant(A, J) :-
	possible(A, J),
	ground(A).

alignement_perdant(A, J) :-
	adversaire(J, Adv),
	alignement_gagnant(A, Adv).

	/******************************
	DEFINITION D'UN ETAT SUCCESSEUR
	*******************************/

     /*Il faut definir quelle op�ration subit une matrice M representant la situation courante
	lorsqu'un joueur J joue en coordonnees [L,C]
     */	

% A FAIRE
successeur(J, M, [L,C]) :-
	nth1(L, M, Ligne),
	nth1(C, Ligne, J).

	/**************************************
   	 EVALUATION HEURISTIQUE D'UNE SITUATION
  	 **************************************/

/*
1/ l'heuristique est +infini si la situation J est gagnante pour J
2/ l'heuristique est -infini si la situation J est perdante pour J
3/ sinon, on fait la difference entre :
	   le nombre d'alignements possibles pour J
	moins
 	   le nombre d'alignements possibles pour l'adversaire de J
*/

nombre_alig_possible(J, S, N) :-
	findall(
		A,
		(
			alignement(A, S),
			possible(A, J)
		),
		Liste_A					% liste des alignements possibles pour J
	),
	length(Liste_A, N).			% N : nombre d'aligments possibles


heuristique(Joueur, Sit, H) :-	% cas trivial gagnant
	alignement(A, Sit),
	alignement_gagnant(A, Joueur),
	!,	% si une situation gagnante est trouvée, on peut ignorer les autres alignements
	H = 42000000.	% +oo
	
heuristique(Joueur, Sit, H) :-	% cas trivial perdant
	alignement(A, Sit),
	alignement_perdant(A, Joueur),
	!,	% si une situation perdante est trouvée, on peut ignorer les autres alignements
	H = -42000000.	% -oo

heuristique(Joueur, Sit, H) :-	% cas général
	nombre_alig_possible(Joueur, Sit, N_Alig_J),
	adversaire(Joueur, Adv),
	nombre_alig_possible(Adv, Sit, N_Alig_Adv),
	H is N_Alig_J - N_Alig_Adv.

test_heuristique :-
	situation_initiale(M0),
	M1 = [[o, x, x], [x, o, o], [x, _, o]], % o gagnant
	M2 = [[x, o, o], [o, x, x], [o, _, x]], % o perdant
	M3 = [[o, x, x], [x, o, o], [x, o, x]], % match nul
	heuristique(o, M0, H0),
	heuristique(o, M1, H1),
	heuristique(o, M2, H2),
	heuristique(o, M3, H3),
	write('M0 = '),
	writeln(M0),
	write('M1 = '),
	writeln(M1),
	write('M2 = '),
	writeln(M2),
	write('M3 = '),
	writeln(M3),
	nl,
	writeln('H0 ='),
	writeln('Expected : 0'),
	write('Gotten : '),
	writeln(H0),
	nl,
	writeln('H1 ='),
	writeln('Expected : 42000000 (+oo)'),
	write('Gotten : '),
	writeln(H1),
	nl,
	writeln('H2 ='),
	writeln('Expected : -42000000 (-oo)'),
	write('Gotten : '),
	writeln(H2),
	nl,
	writeln('H3 ='),
	writeln('Expected : 0'),
	write('Gotten : '),
	writeln(H3).
