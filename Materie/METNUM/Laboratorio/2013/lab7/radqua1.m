function [r,N] = radqua1(x,toll)
% Approssima sqrt(x) a meno di toll
% usando la successione 
% r_0 = x
% r_{k+1} = (r_k + x / r_k) / 2
% e la stima d'errore
% e_{k+1} <= L^k / (1-L) * |r1-r0|
% con L=0.5-0.5*x/r0^2

% Se x<1, il metodo converge, ma la stima d'errore non
% è più valida, quindi in questo caso invertiamo il numero
if (x<1)
    x=1/x;
    flag=true;
else
    flag=false;
end

r=x; N=0;
r1=0.5*(r+x/r);
D=abs(r1-r);
L=0.5-0.5*x/r^2;
while (L^N/(1-L)*D > toll)
    r=0.5*(r+x/r);
    N = N+1;
end
% Se avevamo invertito x, invertiamo la radice
if flag
    r=1/r;
end
