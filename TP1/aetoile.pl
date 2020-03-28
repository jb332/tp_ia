%*******************************************************************************
%                                    AETOILE
%*******************************************************************************

/*
Rappels sur l'algorithme
 
- structures de donnees principales = 2 ensembles : P (etat pendants) et Q (etats clos)
- P est dedouble en 2 arbres binaires de recherche equilibres (AVL) : Pf et Pu
 
   Pf est l'ensemble des etats pendants (pending states), ordonnes selon
   f croissante (h croissante en cas d'egalite de f). Il permet de trouver
   rapidement le prochain etat a developper (celui qui a f(U) minimum).
   
   Pu est le meme ensemble mais ordonne lexicographiquement (selon la donnee de
   l'etat). Il permet de retrouver facilement n'importe quel etat pendant

   On gere les 2 ensembles de fa�on synchronisee : chaque fois qu'on modifie
   (ajout ou retrait d'un etat dans Pf) on fait la meme chose dans Pu.

   Q est l'ensemble des etats deja developpes. Comme Pu, il permet de retrouver
   facilement un etat par la donnee de sa situation.
   Q est modelise par un seul arbre binaire de recherche equilibre.

Predicat principal de l'algorithme :

   aetoile(Pf,Pu,Q)

   - reussit si Pf est vide ou bien contient un etat minimum terminal
   - sinon on prend un etat minimum U, on genere chaque successeur S et les valeurs g(S) et h(S)
	 et pour chacun
		si S appartient a Q, on l'oublie
		si S appartient a Pu (etat deja rencontre), on compare
			g(S)+h(S) avec la valeur deja calculee pour f(S)
			si g(S)+h(S) < f(S) on reclasse S dans Pf avec les nouvelles valeurs
				g et f 
			sinon on ne touche pas a Pf
		si S est entierement nouveau on l'insere dans Pf et dans Pu
	- appelle recursivement etoile avec les nouvelles valeurs NewPF, NewPs, NewQs

*/

%*******************************************************************************

:- ['avl.pl'].       % predicats pour gerer des arbres bin. de recherche
:- ['taquin.pl'].    % predicats definissant le systeme a etudier

%*******************************************************************************

print_list([]).
print_list([H|T]) :-
    writeln(H),
    print_list(T).


affiche_solution(nil, _).


affiche_solution(Fin, Q) :-
    suppress([Fin, [_, H, G], Pere, A], Q, _),
    affiche_solution(Pere, Q),
    
    %atomic_list_concat(['U = ', Fin, '  G = ', G, '  H = ', H, '  A = ', A], Str),
    %writeln(S),

    write('U = '),
    write(Fin),
    write('  G = '),
    write(G),
    write('  H = '),
    write(H),
    write('  A = '),
    writeln(A).

expand(U, Successors, G) :-
    findall(
        [S, [F_new, H_new, G_new], U, A] % forme d'un �tat successeur
        ,(
            rule(A, K, U, S),		 % renvoie les �tats successeurs de toutes les actions possible avec leurs co�ts
            heuristique(S, H_new),	 % calcule l'heuristique d'un �tat successeur
            G_new is G + K,		 % calcule le co�t d'un �tat successeur
            F_new is G_new + H_new	 % calcule f = g + h
        ),
        Successors			 % liste qui regroupe tous les �tats successeurs avec leurs co�ts, heuristiques, etc.
    ).

test_expand :-
    U0 = [[b, h, c], [a, f, d], [g, vide, e]],
    G0 = 0,
    writeln('U0 :'),
    writeln(U0),
    nl,
    expand(U0, Successors, G0),
    writeln('Successors :'),
    print_list(Successors).

loop_successors([], Pf, Pu, _, Pf, Pu). % cas trivial : s'il n'y a plus de successeurs alors on fait remonter les entr�es Pf et Pu en sortie

loop_successors([[S, _, _, _] | Rest], Pf, Pu, Qs, Pf_ret, Pu_ret) :- % si le successeur courant appartient � Q, on ne fait rien car le noeud a d�j� �t� trait�
    belongs([S, _, _, _], Qs),
    loop_successors(Rest, Pf, Pu, Qs, Pf_ret, Pu_ret).

/*
version avec if then else des deux clauses en-dessous

loop_successors([[S, [F, H, G], U, A] | Rest], Pf, Pu, Qs, Pf_ret, Pu_ret) :-
    belongs([S, [F_old, _, _], _, _], Pu),
    (
        F < F_old
    ->
        suppress([S, _, _, _], Pu, Pu_aux),
        insert([S, [F, H, G], U, A], Pu_aux, Pu_new),
        suppress([_, S], Pf, Pf_aux),
        insert([[F, H, G], S], Pf_aux, Pf_new)
    ;
	Pu_new = Pu,
	Pf_new = Pf
    ),
    loop_successors(Rest, Pf_new, Pu_new, Qs, Pf_ret, Pu_ret).
*/

loop_successors([[S, [F, H, G], U, A] | Rest], Pf, Pu, Qs, Pf_ret, Pu_ret) :- % si le successeur appartient � Pu et si la nouvelle valeur f est inf�rieure � l'existante, on la met � jour
    belongs([S, [F_old, _, _], _, _], Pu),			% v�rifie si le successeur appartient � P
    F < F_old,							% v�rifie si la nouvelle valeur F est inf�rieure � l'ancienne
    suppress([S, _, _, _], Pu, Pu_aux),				% supprime S de Pu
    insert([S, [F, H, G], U, A], Pu_aux, Pu_new),		% puis le r�ins�re avec la nouvelle valeur F inf�rieure
    suppress([_, S], Pf, Pf_aux),				% idem avec Pf
    insert([[F, H, G], S], Pf_aux, Pf_new),
    loop_successors(Rest, Pf_new, Pu_new, Qs, Pf_ret, Pu_ret).

loop_successors([[S, [F, _, _], _, _] | Rest], Pf, Pu, Qs, Pf_ret, Pu_ret) :- % si le successeur appartient � Pu et si la nouvelle valeur f est sup�rieure ou �gale � l'ancienne, on ne fait rien
    belongs([S, [F_old, _, _], _, _], Pu),			% v�rifie si le successeur appartient � P
    F >= F_old,							% v�rifie si la nouvelle valeur F est sup�rieure ou �gale � l'ancienne
    loop_successors(Rest, Pf, Pu, Qs, Pf_ret, Pu_ret).

loop_successors([[S, [F, H, G], U, A] | Rest], Pf, Pu, Qs, Pf_ret, Pu_ret) :- % sinon on ins�re le successeur dans P
    insert([S, [F, H, G], U, A], Pu, Pu_new),			% ins�re le successeur dans Pu
    insert([[F, H, G], S], Pf, Pf_new),				% ins�re le successeur dans Pf
    loop_successors(Rest, Pf_new, Pu_new, Qs, Pf_ret, Pu_ret).

test_loop_successors :-
    U0 = [[b, h, c], [a, f, d], [g, vide, e]],
    G0 = 0,
    expand(U0, Successors, G0),
    empty(Pf),
    empty(Pu),
    empty(Qs),
    loop_successors(Successors, Pf, Pu, Qs, Pf_new, Pu_new),
    writeln('U0 ='),
    writeln(U0),
    nl,
    writeln('Successors ='),
    print_list(Successors),
    nl,
    writeln('Pf ='),
    put_flat(Pf_new),
    nl,
    nl,
    writeln('Pu ='),
    put_flat(Pu_new).

main :-
	% initialisations Pf, Pu et Q

	% lancement de Aetoile

    statistics(runtime, [Start, _]),
    initial_state(S0),					%S0 : �tat initial
    G0 is 0,
    heuristique(S0, H0),
    F0 is G0 + H0,
    empty(Pf_0),					% Pf arbre vide
    empty(Pu_0),					% Pu arbre vide
    empty(Qs),						% Qs arbre vide
    insert([[F0, H0, G0], S0], Pf_0, Pf),		% on ins�re l'�tat initial dans Pf
    insert([S0, [F0, H0, G0], nil, nil], Pu_0, Pu),	% et dans Pu
    aetoile(Pf, Pu, Qs),
    statistics(runtime, [Stop, _]),
    Runtime is Stop-Start,
    write('Runtime = '),
    writeln(Runtime).

%*******************************************************************************

aetoile(Pf, Pu, _) :-	% si P est vide alors la solution est inatteignable
    empty(Pf),
    empty(Pu),
    writeln('PAS de SOLUTION : L\'ETAT FINAL N\'EST PAS ATTEIGNABLE !').


aetoile(Pf, Pu, Qs) :-	% si l'�tat de P ayant le f le plus petit est l'�tat final alors l'algorithme est termin� et on affiche la solution
    suppress_min([_, U], Pf, _),
    suppress([U, [F, H, G], Pere, A], Pu, _),
    final_state(U),
    insert([U, [F, H, G], Pere, A], Qs, Qs_new),
    affiche_solution(U, Qs_new).


aetoile(Pf, Pu, Qs) :-	% sinon on enl�ve l'�l�ment de P ayant le f le plus petit et on d�termine ses successeurs, puis on les parcourt pour les traiter
    suppress_min([[F, H, G], U], Pf, Pf_aux),				% on supprime l'�tat U qui a le plus petit f (h+g) de l'arbre des �tats pendants Pf
    suppress([U, [F, H, G], Pere, A], Pu, Pu_aux),			% on supprime le m�me �tat dans l'arbre Pu
    expand(U, Successors, G),						% on trouve les successeurs de U avec les actions possibles
    loop_successors(Successors, Pf_aux, Pu_aux, Qs, Pf_new, Pu_new),	% on parcourt les successeurs de U et on les traite
    insert([U, [F, H, G], Pere, A], Qs, Qs_new),			% on ajoute l'�tat U dans l'abre Q
    aetoile(Pf_new, Pu_new, Qs_new),
    !.

