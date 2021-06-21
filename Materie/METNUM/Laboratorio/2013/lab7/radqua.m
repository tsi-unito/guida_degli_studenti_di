function [r,n]=radqua(x,toll)
% Approssima sqrt(x) a meno di toll
% usando la successione 
% r_0 = x
% r_{k+1} = (r_k + x / r_k) / 2
% e la stima d'errore
% e_{k+1} <= L^k / (1-L) * |r_{k+1}-r_{k}|
% con L=0.5-0.5*x/r_k^2

if nargin<2
    toll=1e-6;
end
if (x<0)
    error('Il radicando deve essere positivo');
end

% Vogliamo che la succ sia monotona descrescente
% in modo da poter stimare L dall'alto.
if x<1
    flag=true;
    x=1/x;
else
    flag = false;
end
err=1+toll;
r=x;
n=0;
while (err>toll)
    r1=0.5*(r+x/r);
    L=0.5*(1-x/r^2);
    err=abs(r1-r)*L/(1-L);
    r=r1;
    n=n+1;
end
if flag
    r=1/r;
end