:- prolog_load_context(directory,Dir), atom_concat(Dir, 'wordnet', WNDB_), absolute_file_name(WNDB_, WNDB), asserta(user:file_search_path(wndb, WNDB)).
:-use_module(wnload/prolog/wn).
:- use_module(library(clpfd)).

connected(A, B, Rel) :- ((wn_hyp(A, B); wn_hyp(B, A)), Rel = hyp);
                        ((wn_mm(A, B); wn_mm(B, A)), Rel = mm);
                        ((wn_mp(A, B); wn_mp(B, A)), Rel = mp).

get_s(S1/W1/P1/Sen1, S2/W2/P2/Sen2) :-
   wn_s(S1, _, W1, P1, Sen1, _), wn_s(S2, _, W2, P2, Sen2, _).

print_rel([_], Acc, Acc).
print_rel([Syn1, Rel, Syn2|Tail], Acc, Con) :-
   get_s(Syn1/W1/P1/Sen1, Syn2/W2/P2/Sen2),
   New = r(W2/P2/Sen2, Rel, W1/P1/Sen1),
   print_rel([Syn2|Tail], [New|Acc], Con).

nums(1).
nums(N) :- nums(M), N is M + 1.

move([H|Tail], [New, Rel, H|Tail]) :-
   connected(H, New, Rel), \+member(New, Tail).

% dfs(TemporaryWay, Finish, Way, Distance).
dfs([Finish|Tail], Finish, [Finish|Tail], 0).
dfs(TempWay, Finish, Way, N) :- N > 0, move(TempWay, NewWay),
   N1 is N - 1, dfs(NewWay, Finish, Way, N1).

route(A, B, Way, MaxDist) :- nums(L), (L > MaxDist, !, fail;
   dfs([A], B, Way, L)).

related_words(Word1/PoS1/Sense1/Syn1, Word2/PoS2/Sense2/Syn2, MaxDist, Connection) :-
wn_s(Syn1, _, Word1, PoS1, Sense1, _),
wn_s(Syn2, _, Word2, PoS2, Sense2, _),
route(Syn1, Syn2, Con, MaxDist),
print_rel(Con, [], Connection).









