function r= radqua_N(x,N)
% Calcola N valori della successione
% r_0 = x
% r_{k+1} = (r_k + x / r_k) / 2
% e restituisce l'ultimo valore calcolato

r=x;
for k=1:N
    r=0.5*(r+x/r);
end
