% ej3.pl
time((caminito(3,[],pos(1,1),pos(3,3),Caminito), fail; true)).
% 2,659 inferences, 0.000 CPU in 0.000 seconds (100% CPU, 6554556 Lips)
true.

% ej3-NoEff.pl
?- time((caminito(3,[],pos(1,1),pos(3,3),Caminito), fail; true)).
% 199,746 inferences, 0.030 CPU in 0.030 seconds (100% CPU, 6682336 Lips)
true.