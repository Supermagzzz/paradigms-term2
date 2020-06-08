get_height(leaf, 0).
get_height(node(_, _, _, H, _, _), H).

get_size(leaf, 0).
get_size(node(_, _, SZ, _, _, _), SZ).

max(A, B, C) :- A > B, C is A.
max(A, B, C) :- A =< B, C is B.

update(leaf, leaf).
update(node(K, V, SZ, H, L, R), Result) :-
    get_height(L, LH), get_height(R, RH), get_size(L, LSZ), get_size(R, RSZ),
	max(LH, RH, BH), ResultH is BH + 1, ResultSZ = LSZ + RSZ + 1, Result = node(K, V, ResultSZ, ResultH, L, R).

small_rotate_left(node(K, V, _, _, L, node(RK, RV, _, _, RL, RR)), Result) :-
	N1 = node(K, V, _, _, L, RL), update(N1, N2), R1 = node(RK, RV, _, _, N2, RR), update(R1, Result).

small_rotate_right(node(K, V, _, _, node(LK, LV, _, _, LL, LR), R), Result) :-
	N1 = node(K, V, _, _, LR, R), update(N1, N2), L1 = node(LK, LV, _, _, LL, N2), update(L1, Result).

big_rotate_left(node(K, V, _, _, L, R), Result) :- small_rotate_right(R, R1), N1 = node(K, V, _, _, L, R1), small_rotate_left(N1, Result).

big_rotate_right(node(K, V, _, _, L, R), Result) :- small_rotate_left(L, L1), N1 = node(K, V, _, _, L1, R), small_rotate_right(N1, Result).

balance(leaf, leaf).
balance(N, Result) :- N = node(_, _, _, _, L, R), update(N, N1), get_height(L, LH), get_height(R, RH),
	((LH is RH - 2, R = node(_, _, _, _, RL, RR), get_height(RL, RLH), get_height(RR, RRH),
			((RLH is RRH + 1, big_rotate_left(N, Result)); small_rotate_left(N1, Result)));
	 (LH is RH + 2, L = node(_, _, _, _, LL, LR), get_height(LL, LLH), get_height(LR, LRH),
			((LLH is LRH - 1, big_rotate_right(N, Result)); small_rotate_right(N1, Result)));
	 (Result = N1)), !.

map_put(leaf, Key, Value, node(Key, Value, 1, 1, leaf, leaf)).
map_put(node(K, V, _, _, L, R), Key, Value, Result) :-
	((Key < K, map_put(L, Key, Value, L1), V1 = node(K, V, _, _, L1, R));
	 (Key > K, map_put(R, Key, Value, R1), V1 = node(K, V, _, _, L, R1));
	 (Key is K, V1 = node(K, Value, _, _, L, R))),
	 balance(V1, Result).

map_get(node(K, V, _, _, L, R), Key, Value) :-
	((Key < K, map_get(L, Key, Value));
	 (Key > K, map_get(R, Key, Value));
	 (Key = K, Value = V)).

map_build([], leaf).
map_build([(K, V) | T], Result) :- map_build(T, N), map_put(N, K, V, Result).

map_min(node(K, V, _, _, L, R), Result) :- ((L \= leaf, map_min(L, Result)); (Result = (K, V))).

map_remove(leaf, Key, leaf).
map_remove(node(Key, V, _, _, L, R), Key, Result) :-
    ((L \= leaf, R \= leaf, map_min(R, (MinK, MinV)), map_remove(R, MinK, R1), V1 = node(MinK, MinV, _, _, L, R1), balance(V1, Result));
	 (L \= leaf, Result = L);
	 (R \= leaf, Result = R);
	 (Result = leaf)), !.
map_remove(node(K, V, _, _, L, R), Key, Result) :-
	((Key < K, map_remove(L, Key, L1), V1 = node(K, V, _, _, L1, R));
	 (Key > K, map_remove(R, Key, R1), V1 = node(K, V, _, _, L, R1))),
	 balance(V1, Result).

map_lower_bound(leaf, Key, 0).
map_lower_bound(node(K, V, _, _, L, R), Key, Result) :-
	((Key < K, map_lower_bound(L, Key, Result));
	 (map_lower_bound(R, Key, A), get_size(L, B), ((Key = K, Result = A + B); (Result = A + B + 1)))).

map_submapSize(Map, FromKey, ToKey, Size) :-
	Left = FromKey, Right = ToKey,
	map_lower_bound(Map, Left, A), map_lower_bound(Map, Right, B), Size is B - A, !.
