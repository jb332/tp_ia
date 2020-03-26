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

   On gere les 2 ensembles de façon synchronisee : chaque fois qu'on modifie
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

affiche_solution(nil, _).
    

affiche_solution(Fin, Q) :-
    suppress([Fin, [_, H, G], Pere, A], Q, _),
    affiche_solution(Pere, Q),
    
    %atomic_list_concat(['U = ', Fin, '  G = ', G, '  H = ', H, '  A = ', A], Str),
    %writeln(S),

    writeln('U = '),
    writeln(Fin),
    writeln('  G = '),
    writeln(G),
    writeln('  H = '),
    writeln(H),
    writeln('  A = '),
    writeln(A),
    nl.

expand(U, Successors, G) :-
    findall(
        [S, [F_new, H_new, G_new], U, A]
        ,(
            rule(A, 1, U, S),
            heuristique(S, H_new),
            G_new is G + 1,
            F_new is G_new + H_new
        ),
        Successors
    ).

loop_successors([], Pf, Pu, _, Pf, Pu).

loop_successors([[S, _, _, _] | Rest], Pf, Pu, Qs, Pf_ret, Pu_ret) :-
    belongs([S, _, _, _], Qs),
    loop_successors(Rest, Pf, Pu, Qs, Pf_ret, Pu_ret).

loop_successors([[S, [F, H, G], U, A] | Rest], Pf, Pu, Qs, Pf_ret, Pu_ret) :-
    belongs([S, [F_old, _, _], _, _], Pu),
    (
        F < F_old
    ->
        suppress([S, _, _, _], Pu, Pu_aux),
        insert([S, [F, H, G], U, A], Pu_aux, Pu_new),
        suppress([_, S], Pf, Pf_aux),
        insert([[F, H, G], S], Pf_aux, Pf_new)
    ),
    loop_successors(Rest, Pf_new, Pu_new, Qs, Pf_ret, Pu_ret).

loop_successors([[S, [F, H, G], U, A] | Rest], Pf, Pu, Qs, Pf_ret, Pu_ret) :-
    insert([S, [F, H, G], U, A], Pu, Pu_new),
    insert([[F, H, G], S], Pf, Pf_new),
    loop_successors(Rest, Pf_new, Pu_new, Qs, Pf_ret, Pu_ret).
    

main :-
	% initialisations Pf, Pu et Q

	% lancement de Aetoile

    statistics(runtime, [Start, _]),
    initial_state(S0),
    G0 is 0,
    heuristique(S0, H0),
    F0 is G0 + H0,
    empty(Pf_0),
    empty(Pu_0),
    empty(Qs),
    insert([[F0, H0, G0], S0], Pf_0, Pf),
    insert([S0, [F0, H0, G0], nil, nil], Pu_0, Pu),
    aetoile(Pf, Pu, Qs),
    statistics(runtime, [Stop, _]),
    Runtime is Stop-Start,
    writeln('Runtime = '),
    writeln(Runtime).

%*******************************************************************************

aetoile(Pf, Pu, _) :-
    empty(Pf),
    empty(Pu),
    writeln('PAS de SOLUTION : L\'ETAT FINAL N\'EST PAS ATTEIGNABLE !').


aetoile(Pf, Pu, Qs) :-
    suppress_min([_, U], Pf, _),
    suppress([U, [F, H, G], Pere, A], Pu, _),
    final_state(U),
    insert([U, [F, H, G], Pere, A], Qs, Qs_new),
    affiche_solution(U, Qs_new),
    !.


aetoile(Pf, Pu, Qs) :-
    suppress_min([[F, H, G], U], Pf, Pf_aux),
    suppress([U, [F, H, G], Pere, A], Pu, Pu_aux),
    expand(U, Successors, G),
    loop_successors(Successors, Pf_aux, Pu_aux, Qs, Pf_new, Pu_new),

    insert([U, [F, H, G], Pere, A], Qs, Qs_new),
    aetoile(Pf_new, Pu_new, Qs_new),
    !.

