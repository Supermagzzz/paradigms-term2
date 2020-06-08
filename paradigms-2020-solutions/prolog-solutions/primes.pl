min_divisor(N, P) :- primes(P), P * P =< N, 0 is mod(N, P), !.
min_divisor(N, N).

prime(N) :- min_divisor(N, P), N is P.
composite(N) :- \+(prime(N)).

calc_primes(I, RANGE) :-
  member(I, RANGE),
  prime(I),
  assert(primes(I)).

range(L, L, []).
range(N, L, [N | T]) :- N < L, N1 is N + 1, range(N1, L, T).

init(N) :- SQRT is ceiling(sqrt(N)) + 2, range(2, SQRT, RANGE), findall(I, calc_primes(I, RANGE), R).

prime_divisors(1, []).
prime_divisors(N, [H | T]) :- integer(N), N > 1, min_divisor(N, H), N1 is div(N, H), prime_divisors(N1, T).

check_sorted([A]).
check_sorted([A, B | T]) :- A =< B, check_sorted([B | T]).
get_divisors(N, [H | T]) :- prime(H), prime_divisors(R, T), N is R * H.

prime_divisors(N, P) :- \+(integer(N)), check_sorted(P), get_divisors(N, P).

merge(A, [], A) :- !.
merge([], B, B) :- !.
merge([H | T1], [H | T2], [H | T]) :- merge(T1, T2, T).
merge([H1 | T1], [H2 | T2], [H1 | L]) :- H1 < H2, merge(T1, [H2 | T2], L).
merge([H1 | T1], [H2 | T2], [H2 | L]) :- H1 > H2, merge([H1 | T1], T2, L).

lcm(A, B, LCM) :- prime_divisors(A, R1), prime_divisors(B, R2), merge(R1, R2, R), prime_divisors(LCM, R).