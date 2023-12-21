:- use_module(library(clpfd)).

book(away).
book(call).
book(dessert).
book(wendalissa).

author(olson).
author(hansen).
author(jones).
author(stout).

unique([]) :- !.
unique([H|T]) :-  member(H, T), !, fail; unique(T).

myprint([], Acc, Ans) :- Ans = Acc, !.
myprint([H|T], Acc, Ans) :- atomic_list_concat(H, "-", Elem),
    append(Acc, [Elem], L), myprint(T, L, Ans).

sol(Ans) :- S = [[12, B1, A1], [19, B2, A2], [26, B3, A3], [33, B4, A4]],
    book(B1), book(B2), book(B3), book(B4), unique([B1, B2, B3, B4]),
    author(A1), author(A2), author(A3), author(A4), unique([A1, A2, A3, A4]),

    %  first hint
    ((member([19, dessert,  _],S), member([_, wendalissa, olson], S));
    (member([19, wendalissa, _], S), member([_, dessert, olson], S))),
    \+member([19, _, olson], S),

    % second hint
    ((member([19, away,  _],S), member([33, _, jones], S));
    (member([19, _, jones], S), member([33, away, _], S))),
    \+member([_, away, jones], S),

    % third hint
    member([X, wendalissa, _], S), member([Y, _, stout], S), X #= Y + 7,
    \+member([_, wendalissa, stout], S),

    % print answer
    myprint(S,[], Ans).














