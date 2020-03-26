%***************************
% Gestion d'un AVL en Prolog
%***************************

%***************************
% INSA TOULOUSE - P.ESQUIROL
% mars 2017
%***************************

%*************************
% unit tests          : OK
% integration aetoile : OK
%*************************

% Les AVL sont des arbres BINAIRES DE RECHERCHE H-EQUILIBRES : 
% La hauteur de l'avl A est définie par :
%  -1, si A est vide (A=nil)
%  1 + max( hauteur(ss_arbre_gauche(A)), hauteur(ss_arbre_droitee(A)) ) sinon

% Tout noeud de l'arbre est soit :
% - une feuille
% - un noeud interne tel que la différence de hauteur entre le sous-arbre droit
% 	et le sous-arbre gauche appartient à  [-1,0,+1]


%***********************************************
% PREDICATS EXPORTES ET COMPLEXITE ALGORITHMIQUE
%***********************************************
% soit N = nombre de noeuds de l'arbre				%   UTILITE POUR A*
%													%   ----------------													
% empty(?Avl)						O(1)			%<<< initialisation de P et Q
% height(+Avl, ?Height)             O(1)			
% put_flat(+Avl)                    O(N)			
% put_90(+Avl)                      O(N)			
% belongs(+Elem, +Avl)              O(log N)		%<<< appartenance d'un noeud à Q
% subtree(+Elem, +Avl, Ss_Avl)      O(log N)
% insert(+Elem, +Avant, ?Apres)     O(log N)		%<<< insertion d'un nouveau noeud dans P ou dans Q
% suppress(+Elem,+Avant,?Apres)     O(log N)		%<<< mise  à jour <=> suppression puis insertion
% suppress_min(?Min,+Avant,?Apres)  O(log N)		%<<< supression du noeud minimal
% suppress_max(?Max,+Avant,?Apres)  O(log N)

%****************************
% Prédicats internes (prives)
%****************************

% left_rotate(+Avant, ?Apres)		O(1)
% right_rotate(+Avant, ?Apres)		O(1)
% left_balance(+Avant, ?Apres)      O(1)
% right_balance(+Avant, ?Apres)     O(1)



	%------------------------------
	% Constructeur et test AVL vide
	%------------------------------

empty(nil).

	%-----------------
	% Hauteur d'un AVL
	%-----------------
	% par convention, un avl vide a une hauteur de -1
	% sinon la hauteur est enregistree au meme niveau que la racine de l'avl
	% elle n'est pas calculee recursivement "from scratch"
	% elle est mise à jour de façon incrémentale, apres chaque insertion ou suppression
	% d'ou sa complexité en O(1)  :-)

height(nil,             -1).
height(avl(_G,_R,_D, H), H).

	%-------------------
	% Affichage d'un AVL
	%-------------------
	% dans l'ordre croissant (lexicographique)

put_flat(nil).
put_flat(avl(G,R,D,_H)) :-
	put_flat(G),
	nl, write(R), 
	put_flat(D).

	%----------------------------
	% Affichage (couché) d'un AVL
	%----------------------------

put_90(Avl) :-
	nl, writeln('----------------------------------'),
	put_90(Avl,"").

put_90(nil,Str) :-
	write(Str), write('.').
put_90(avl(G,R,D,_H),Str) :-
	append_strings(Str, "   ", Str2),
	put_90(D,Str2),
	nl, write(Str), write(R),nl,
	put_90(G,Str2).

	%-----------------------------------------
	% Appartenance d'un element donne a un AVL	
	%-----------------------------------------

belongs(Elem, avl(G,Racine,D,_Hauteur)) :-
	(Elem = Racine ->
		true
	;
		(Elem @< Racine ->
			belongs(Elem, G)
		;
			belongs(Elem, D) 		%Racine @< Elem
		)
	).
	
	%------------------------------------------------------------
	% Recherche du sous-arbre qui a comme racine un element donne	
	%------------------------------------------------------------

subtree(Elem, avl(G,Racine,D,H), A) :-
	(Elem = Racine ->
		A = avl(G,Racine,D,H)
	;
		(Elem @< Racine ->
			subtree(Elem,G,A)
		;
			subtree(Elem,D,A) 		%Racine @< Elem
		)
	).
	
	%----------------------
	% Rotations dans un avl
	%----------------------
	% Les rotations ci-dessous décrivent uniquement les cas ou la rotation est possible.
	% Dans les autres cas, ces relations échouent ; plus précisément :
	% a/ si l'arbre est un avl vide, alors aucune rotation n'est possible ;
	% b/ si l'arbre est un avl non vide mais si son ss-arbre gauche est un avl vide
	%    alors la rotation droite n'est pas possible ;
	% c/ si l'arbre est un avl non vide mais si son ss-arbre droite est un avl vide
	%    alors la rotation gauche n'est pas possible.

right_rotate(avl(G,R,D,_H), A_Apres) :-
	height(D,HD),
	G       = avl(SG,RG,SD,_HG),
	height(SD,HSD),
	H_Inter is 1 + max(HSD, HD),
	Inter   = avl(SD,R,D,H_Inter),
	height(SG,HSG),
	H_Apres is 1 + max(HSG,H_Inter),
	A_Apres = avl(SG,RG,Inter,H_Apres).
	
left_rotate(avl(G,R,D,_), A_Apres) :-
	height(G,HG),
	D       = avl(SG,RD,SD,_),
	height(SG,HSG),
	H_Inter is 1 + max(HSG, HG),
	Inter   = avl(G,R,SG,H_Inter),
	height(SD,HSD),
	H_Apres is 1 + max(H_Inter,HSD),
	A_Apres = avl(Inter,RD,SD,H_Apres).	

	%---------------------------------
	% Insertion equilibree dans un avl
	%---------------------------------
	% On suppose que l'arbre avant insertion est equilibré (difference de hauteur
	% entre les ss-arbres gauche et droite de 1 au maximum)
	% L'insertion doit assurer qu'apres insertion l'arbre est toujours equilibre
	% sinon les rotations necessaires sont effectuees.

	% On suppose que les noeuds contiennent des informations que l'on peut comparer
	% a l'aide d'une relation d'ordre lexicographique (la cle c'est l'info elle-meme)
	% En prolog, c'est la relation '@<'
	% On peut comparer par exemple des integer, des string, des constantes,
	% des listes d'entiers, des listes de constantes, etc ... bref, des termes clos
	% T1 @< T2 est vrai si T1 est lexicographiquement inférieur a T2.

insert(Elem, nil, avl(nil,Elem,nil,0)).
insert(Elem, AVL, NEW_AVL) :-
	AVL = avl(Gauche,Racine,Droite,_Hauteur),
	(Elem = Racine ->
			% l'élément est déjà present, pas d'insertion possible
		fail
	;
		(Elem @< Racine ->
			% insertion dans le ss-arbre gauche
			insert(Elem, Gauche, New_Gauche),
			height(New_Gauche, New_HG),
			height(Droite, HD),
			H_Int is 1+max(New_HG, HD),
			AVL_INT = avl(New_Gauche, Racine, Droite, H_Int), 
			right_balance(AVL_INT, NEW_AVL)
		;
	    % Elem @> Racine
			% insertion dans le ss-arbre droite
			insert(Elem, Droite, New_Droite),
			height(New_Droite, New_HD),
			height(Gauche, HG),
			H_Int is 1+max(New_HD, HG),
			AVL_INT =avl(Gauche, Racine,New_Droite, H_Int),
			left_balance(AVL_INT, NEW_AVL)
		)
	).
	
	%------------------------------------------------
	% Suppression d'un element quelconque dans un avl
	%------------------------------------------------
	% On suppose que l'élément à supprimer appartient bien à l'AVL,
	% sinon le predicat échoue (en particulier si l'AVL est vide).
	
suppress(Elem, AVL, NEW_AVL) :-
	AVL = avl(Gauche, Racine, Droite, _Hauteur),
	(Elem = Racine ->
		% cas de la suppression de la racine de l'avl
		(Gauche = nil -> % cas simple d'une feuille ou d'un avl sans fils gauche
			NEW_AVL = Droite
		; 
			(Droite = nil -> % cas simple d'un avl avec fils gauche mais sans fils droit
				NEW_AVL = Gauche
			;
				% cas d'un avl avec fils gauche ET fils droit 
				%Gauche \= nil
				%Droite \= nil
				suppress_max(Max, Gauche, New_Gauche),
				AVL_INT = avl(New_Gauche,Max,Droite,_),
				left_balance(AVL_INT, NEW_AVL)
			)
		)
	;
		% cas des suppressions d'un element autre que la racine 
		(Elem @< Racine ->
			% suppression dans le ss-arbre gauche
			suppress(Elem, Gauche, New_Gauche),
			AVL_INT = avl(New_Gauche, Racine, Droite,_),
			left_balance(AVL_INT, NEW_AVL)
		;
		%Racine @< Droite
			% suppression dans le ss-arbre droite 
			suppress(Elem, Droite, New_Droite),
			AVL_INT = avl(Gauche, Racine, New_Droite,_),
			right_balance(AVL_INT, NEW_AVL)
		)
	).
	
	%-------------------------------------------------------
	% Suppression du plus petit element dans un avl non vide
	%-------------------------------------------------------
	% Si l'avl est vide, le prédicat échoue

suppress_min(Min, AVL, NEW_AVL) :-
	AVL = avl(Gauche,Racine,Droite, _Hauteur),
	(Gauche = nil ->
		Min = Racine,
		NEW_AVL = Droite
	;
		% Gauche \= nil
		suppress_min(Min, Gauche, New_Gauche),
		AVL_INT = avl(New_Gauche, Racine, Droite,_),
		left_balance(AVL_INT, NEW_AVL)
	).

	%-------------------------------------------------------
	% Suppression du plus grand element dans un avl non vide
	%-------------------------------------------------------
	% Si l'avl est vide, le prédicat échoue

suppress_max(Max, AVL, NEW_AVL) :-
	AVL = avl(Gauche,Racine,Droite, _Hauteur),
	(Droite = nil ->
		Max = Racine,
		NEW_AVL = Gauche
	;
		% Droite \= nil
		suppress_max(Max, Droite, New_Droite),
		AVL_INT = avl(Gauche, Racine, New_Droite,_),
		right_balance(AVL_INT, NEW_AVL)
	).
	
	%----------------------------------------
	% Re-equilibrages d'un avl vers la gauche
	%----------------------------------------
	% - soit apres insertion   d'un element dans le sous-arbre droite
	% - soit apres suppression d'un élément dans le sous-arbre gauche
	%----------------------------------------------------------------

left_balance(Avl, New_Avl) :-
	Avl = avl(Gauche, Racine, Droite, _Hauteur),
	height(Gauche, HG),
	height(Droite, HD),
	(HG is HD-2 ->
	% le sous-arbre droite est trop haut 
		Droite = avl(G_Droite, _R_Droite, D_Droite, _HD),
		height(G_Droite, HGD),
		height(D_Droite, HDD),
		(HDD > HGD ->
		% une simple rotation gauche suffit
			left_rotate(Avl, New_Avl)
		;
		% il faut faire une rotation droite_gauche
			right_rotate(Droite, New_Droite),
			height(New_Droite, New_HD),
			H_Int is 1+ max(HG, New_HD),
			Avl_Int = avl(Gauche, Racine, New_Droite, H_Int),
			left_rotate(Avl_Int, New_Avl)
		)
	;
	% la suppression n'a pas desequilibre l'avl
		New_Hauteur is 1+max(HG,HD),
		New_Avl = avl(Gauche, Racine, Droite, New_Hauteur)
	).

	%----------------------------------------
	% Re-equilibrages d'un avl vers la droite
	%----------------------------------------
	% - soit apres insertion   d'un element dans le sous-arbre gauche
	% - soit apres suppression d'un élément dans le sous-arbre droite
	%----------------------------------------------------------------
	
right_balance(Avl, New_Avl) :-
	Avl = avl(Gauche, Racine, Droite, _Hauteur),
	height(Gauche, HG),
	height(Droite, HD),
	(HD is HG-2 ->
	% le sous-arbre gauche est trop haut 
		Gauche = avl(G_Gauche, _R_Gauche, D_Gauche, _HG),
		height(G_Gauche, HGG),
		height(D_Gauche, HDG),
		(HGG > HDG ->
		% une simple rotation droite suffit
			right_rotate(Avl, New_Avl)
		;
		% il faut faire une rotation gauche_droite
			left_rotate(Gauche, New_Gauche),
			height(New_Gauche, New_HG),
			H_Int is 1+ max(New_HG, HD),
			Avl_Int = avl(New_Gauche, Racine, Droite, H_Int),
			right_rotate(Avl_Int, New_Avl)
		)
	;
	% la suppression n'a pas desequilibre l'avl
		New_Hauteur is 1+max(HG,HD),
		New_Avl = avl(Gauche, Racine, Droite, New_Hauteur)
	).
	
%-----------------------------------------
% Arbres utilises pour les tests unitaires
%-----------------------------------------
avl_test(1, nil).
avl_test(2, avl(nil, 1, nil,              0)).
avl_test(3, avl(nil, 1, avl(nil,2,nil,0), 1)).
avl_test(4, avl(avl(nil,1,nil,0),2, nil,  1)).
avl_test(5, avl(avl(nil,1,nil,0), 2, avl(nil,3,nil,0),1)	).
avl_test(6, avl(avl(nil,5,nil,0), 6, avl(nil,7,nil,0),1)	).
avl_test(7,  avl(G,4,D,2)) :-
	avl_test(5,G),
	avl_test(6,D).
avl_test(8, avl(G,5,D,2)) :-
	D = avl(nil,6,nil,0),
	avl_test(3,G).
avl_test(9, avl(G,3,D,2)) :-
	G = avl(nil,1,nil,0),
	avl_test(4,D).
	
/* Test uniquement valable avec ECLiPSe

avl_test(10, Final) :-
   empty(Init),
   (for(I,1,20), fromto(Init,In,Out,Final) do
     insert(I,In,Out)
   ).
*/
