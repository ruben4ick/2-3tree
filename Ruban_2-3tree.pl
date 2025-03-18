is_node2(node2(_, _, _)).
is_node3(node3(_, _, _, _, _)).
branch(X) :- is_node2(X); is_node3(X).


exists(node2(I, _, _), X) :- X = I.
exists(node2(I, branch(NL), _), X) :- X < I, !, exists(NL, X).
exists(node2(I, _, (NL)), X) :- X > I, !, exists(NL, X).

exists(node3(IL, IR, _, _, _), X) :- X = IL, !; X = IR, !.
exists(node3(IL, _, branch(N), _, _), X) :- X < IL, !, exists(X, N).
exists(node3(IL, IR, _ , branch(N), _), X) :- X > IL, X < IR, !, exists(X, N).
exists(node3(_, IR, _, _, branch(N)), X) :- X > IR, !, exists(X, N).

insert_sorted([], V, R) :- R = [V].
insert_sorted([A|N], V, R) :- V < A, R = [V,A|N].
insert_sorted([A|N], V, R) :- V = A, R = [A|N].
insert_sorted([A|N], V, R) :- V > A, insert_sorted(N, V, R2), R = [A|R2].


sort_insert(P, [], P).
sort_insert(P, [A|N], R) :- insert_sorted(P, A, P2), sort_insert(P2, N, R).
sort_insert(P, R) :- sort_insert([], P, R).

was_promoted(promote(V, NL, NR), promote(V, NL, NR)).

insert_node_internal(empty, V, R) :- !, R =  node2(V, empty, empty).
insert_node_internal(node2(IL, empty, empty), V, R) :- !, sort_insert([IL, V], [S1, S2]),
    R = node3(S1, S2, empty, empty, empty).
insert_node_internal(node3(IL, IR, empty, empty, empty), V, R) :- !, sort_insert([IL, IR, V], [S1, S2, S3]),
    R = promote(S2, node2(S1, empty, empty), node2(S3, empty, empty)).

insert_node_internal(node2(I, NL, NR), V, R) :- V < I, !, insert_node_internal(NL, V, R2),
    ((was_promoted(R2, promote(V2, PL, PR)), R = node3(V2, I, PL, PR, NR), !);
    R = node2(I, R2, NR)).
insert_node_internal(node2(I, NL, NR), V, R) :- V > I, !, insert_node_internal(NR, V, R2),
    ((was_promoted(R2, promote(V2, PL, PR)), R = node3(I, V2, NL, PL, PR), !);
    R = node2(I, NL, R2)).

insert_node_internal(node3(IL, IR, NL, NM, NR), V, R) :- V > IR, !, insert_node_internal(NR, V, R2),
    ((was_promoted(R2, promote(V2, PL, PR)), R = promote(IR, node2(IL, NL, NM), node2(V2, PL, PR)));
    R = node3(IL, IR, NL, NM, R2)).
insert_node_internal(node3(IL, IR, NL, NM, NR), V, R) :- V < IL, !, insert_node_internal(NL, V, R2),
    ((was_promoted(R2, promote(V2, PL, PR)), R = promote(IL, node2(V2, PL, PR), node2(IR, NM, NR)),  !);
    R = node3(IL, IR, R2, NM, NR)).
insert_node_internal(node3(IL, IR, NL, NM, NR), V, R) :- V > IL, V < IR, !, insert_node_internal(NM, V, R2),
    ((was_promoted(R2, promote(V2, PL, PR)), R = promote(V2, node2(IL, NL, PL), node2(IR, PR, NR)),  !);
    R = node3(IL, IR, NL, R2, NR)).

insert_node(N, V, R) :- insert_node_internal(N, V, R2),
    ((was_promoted(R2, promote(V2, PL, PR)), R = node2(V2, PL, PR));
    R = R2
    ).

insert_all(N, [], N).
insert_all(N, [V | L], R) :- insert_node(N, V, R2), insert_all(R2, L, R), !.



handle_hole(X, hole(RBH), node2(Y, YNL, YNR), R) :- !,
    R = hole(node3(X, Y, RBH, YNL, YNR)).
handle_hole(X, node2(Y, YNL, YNR), hole(RBH), R) :- !,
    R = hole(node3(Y, X, YNL, YNR, RBH)).
handle_hole(X, hole(A), node3(Y, Z, B, C, D), R) :- !,
    R = node2(Y, node2(X, A, B), node2(Z, C, D)).
handle_hole(Z, node3(X, Y, A, B, C), hole(D), R) :- !,
    R = node2(Y, node2(X, A, B), node2(Z, C, D)).
handle_hole(Z, node3(X, Y, A, B, C), hole(D), R) :- !,
    R = node2(Y, node2(X, A, B), node2(Z, C, D)).
handle_hole(X, NL, NR, node2(X, NL, NR)). % When there is no hole do nothing

handle_hole(X, Z, hole(A), node2(Y, B, C), D, R) :- !,
    R = node2(Z, node3(X, Y, A, B, C), D).
handle_hole(Y, Z, node2(X, A, B), hole(C), D, R) :- !,
    R = node2(Z, node3(X, Y, A, B, C), D).
handle_hole(X, Y, A, hole(B), node2(Z, C, D), R) :- !,
    R = node2(X, A, node3(Y, Z, B, C, D)).
handle_hole(X, Z, A, node2(Y, B, C), hole(D),  R) :- !,
    R = node2(X, A, node3(Y, Z, B, C, D)).
handle_hole(W, Z, hole(A), node3(X, Y, B, C, D), E, R) :- !,
    R = node3(X, Z, node2(W, A, B), node2(Y, C, D), E).
handle_hole(Y, Z, node3(W, X, A, B, C), hole(D), E, R) :- !,
    R = node3(X, Z, node2(W, A, B), node2(Y, C, D), E).
handle_hole(W, X, A, hole(B), node3(Y, Z, C, D, E), R) :- !,
    R = node3(W, Y, A, node2(X, B, C), node2(Z, D, E)).
handle_hole(W, Z, A, node3(X, Y, B, C, D), hole(E), R) :- !,
    R = node3(W, Y, A, node2(X, B, C), node2(Z, D, E)).

handle_hole(X, Y, NL, NM, NR, node3(X, Y, NL, NM, NR)). % When there is no hole do nothing



is_hole(hole(N), N).

delete_node(T, V, R) :-
    delete_node_internal(T, V, RU),
    (is_hole(RU, R), !;
     R = RU
    ).

delete_node_internal(node3(IL, IR, empty, empty, empty), V, R) :-
    IL = V, !, R = node2(IR, empty, empty);
    IR = V, !, R = node(IL, empty, empty).
delete_node_internal(node2(V, empty, empty), V, hole(empty)) :- !.

% When value is in branch.
delete_node_internal(node2(X, NL, NR), X, R) :-
    take_successor(NR, X, I, RT),
    delete_node_internal(RT, X, NRT),
    handle_hole(I, NL, NRT, R).
delete_node_internal(node3(X, IR, NL, NM, NR), X, R) :-
    take_successor(NM, X, I, RT),
    delete_node_internal(RT, X, NMT),
    handle_hole(I, IR, NL, NMT, NR, R).
delete_node_internal(node3(IL, X, NL, NM, NR), X, R) :-
    take_successor(NR, X, I, RT),
    delete_node_internal(RT, X, NRT),
    handle_hole(IL, I, NL, NM, NRT, R).


delete_node_internal(node2(X, NL, NR), V, R) :-
    X > V, !,
    delete_node_internal(NL, V, RB), % going downward to delete item
    handle_hole(X, RB, NR, R). % handle having hole
delete_node_internal(node2(X, NL, NR), V, R) :-
    X < V, !,
    delete_node_internal(NR, V, RB), % going downward to delete item
    handle_hole(X, NL, RB, R). % handle having hole
delete_node_internal(node3(IL, IR, NL, NM, NR), V, R) :-
    IL > V, !,
    delete_node_internal(NL, V, RB),
    handle_hole(IL, IR, RB, NM, NR, R).
delete_node_internal(node3(IL, IR, NL, NM, NR), V, R) :-
    IL < V, V < IR, !,
    delete_node_internal(NM, V, RB),
    handle_hole(IL, IR, NL, RB, NR, R).
delete_node_internal(node3(IL, IR, NL, NM, NR), V, R) :-
    IR < V, !,
    delete_node_internal(NR, V, RB),
    handle_hole(IL, IR, NL, NM, RB, R).

take_successor(node2(X, empty, empty), V, X, node2(V, empty, empty)) :- !.
take_successor(node3(X, IR, empty, empty, empty), V, X, node3(V, IR, empty, empty, empty)) :- !.
take_successor(node2(I, NL, NR), V, RV, RT) :-
    take_successor(NL, V, RV, RNL), RT = node2(I, RNL, NR).
take_successor(node3(IL, IR, NL, NM, NR), V, RV, RT) :-
    take_successor(NL, V, RV, RNL), RT = node3(IL, IR, RNL, NM, NR).






