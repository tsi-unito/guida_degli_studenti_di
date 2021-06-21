function [r,N] = radqua_fast(x,toll)
% Approssima sqrt(x) a meno di toll
% usando la successione 
% r_0 = x
% r_{k+1} = (r_k + x / r_k) / 2
% e la stima d'errore
% e_{k+1} <= L^k / (1-L) * |r_{k+1}-r_{k}|
% con L=0.5-0.5*x/r_k^2
%Questa versione è pensata per un dispositivo 
% dalle risorse limitate su cui possiamo facilmente accedere
% all'esponente della rappresentazione floating point,
% (anzichè fare l'inversione se x<1) potremmo moltiplicare 
% o dividere un qualunque input x per 4^m in modo che x stia 
% in [1,4], applicare il metodo iterativo e poi 
% dividere/moltiplicare r per 2^m
% Nota: rispetto alla versione precedente, toll diventa
% la tolleranza relativa e non la tolleranza assoluta

if (x<0)
    error('Il radicando deve essere positivo');
end

[f,e]=log2(x);        %mantissa ed esponente di x
m = floor( (e-1)/2 ); %scelgo la potenza di 4
x = pow2(x,-2*m);     % divide x per 2^(2m)=4^m
% ora x sta fra 1 e 4

r=x; N=0; err = 2*toll;
while (err > toll)
    r1=0.5*(r+x/r);
    L=0.5*(1-x/r^2);
    err = L/(1-L)*abs(r1-r); %abs si può togliere, tanto r1>r se r0>1
    N = N+1;
    r = r1;
end
r = pow2(r,m); %moltiplicazione per 2^m
