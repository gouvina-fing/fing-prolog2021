%% insertion sort
% ----------------------------------------------------------------
% insertOrder(X, L1, L2) <- L2 es el resultado de insertar X en forma ordenada en la lista L1, donde L1 ya esta ordenada
insertOrder(X, [], [X]).
insertOrder(X, [H1|L1], [X, H1|L1]) :- X < H1.
insertOrder(X, [H1|L1], [H1|L2]) :- not(X < H1), insertOrder(X, L1, L2).

% insertionsort(+L,?S)  <- S es el resultado de ordenar la lista L utilizando el algoritmo insertion sort
insertionSort([], []).
insertionSort([H|L], L2) :- insertionSort(L, L1), insertOrder(H, L1, L2).


%% merge sort
% ----------------------------------------------------------------
% merge(+L1,+L2,?L3) <- L3   es   el   resultado   de   combinar ordenadamente los elementos de las listas(ordenadas) L1 y L2
merge([],L,L).
merge(L,[],L).
merge([H1|L1], [H2|L2], [H1|L3]) :- H1 =< H2, merge(L1, [H2|L2], L3).
merge([H1|L1], [H2|L2], [H2|L3]) :- H2 < H1, merge([H1|L1], L2, L3).

% splitLists(?L, ?A, ?B) <- Se divide L en partes iguales A y B
splitLists(L, A, B) :- append(A, B, L), length(A, N), length(B, N).
splitLists(L, A, B) :- append(A, B, L), length(A, N), length(B, N1), N1 is N + 1.

% mergeSort(+L,?S) <- S   es   el   resultado   de   ordenar   la   lista   L utilizando el algoritmo merge sort
mergeSort([], []).
mergeSort([X], [X]).
mergeSort([H1, H2 | L],S) :- splitLists([H1, H2 | L], LA, LB), mergeSort(LA, LA1), mergeSort(LB, LB1), merge(LA1, LB1, S).



%% quick sort
% ----------------------------------------------------------------
% quicksort(+L,?S) <- S   es   el   resultado   de   ordenar   la   lista   L utilizando el algoritmo quick sort

% ACUMULADORES
menores_mayores(_, [], Menores, Mayores, Menores, Mayores).
menores_mayores(X, [H|L], Men, May, MenAc, MayAc) :- H =< X, menores_mayores(X, L, [H | Men], May, MenAc, MayAc).
menores_mayores(X, [H|L], Men, May, MenAc, MayAc) :- H > X, menores_mayores(X, L, Men, [H |May], MenAc, MayAc).

% RECURSIVO
menores_mayores(_, [], [], []).
menores_mayores(X, [H|L], [H|Menores], Mayores) :- H =< X, menores_mayores(X, L, Menores, Mayores).
menores_mayores(X, [H|L], Menores, [H|Mayores]) :- H > X, menores_mayores(X, L, Menores, Mayores).



quickSort([], []).
quickSort([H|L], S) :- menores_mayores(H, L, Men, May), quickSort(Men, MenOrd), quickSort(May, MayOrd), append(MenOrd, [H | MayOrd], S ).
%quickSort([H|L], S) :- menores_mayores(H, L, [], [], Men, May), quickSort(Men, MenOrd), quickSort(May, MayOrd), append(MenOrd, [H | MayOrd], S ).
