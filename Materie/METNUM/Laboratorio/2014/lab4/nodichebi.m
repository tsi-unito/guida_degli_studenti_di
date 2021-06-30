function xn=nodichebi(N)
% function xn=nodichebi(N)
% Nodi di Chebichev per l'interpolazione
%  polinomiale di grado N in [-1,1]

f=(2*(1:N+1)-1)/2/(N+1);
xn=cos(pi*f);
