function [T,Y]=Heun(f,y0,tfin,h0)
% Approssima la soluzione di 
% y' = f(y) , y(0)=y0
% per t da 0 a tfin,
% usando il metodo di Heun con passo h0

t=0; % tempo corrente
T=0; % elenco dei tempi t
Y=y0; % elenco dei valori di y(t)
while (t<tfin)
   % 1 passo di Heun lungo h0
   K1 = f(Y(end));
   K2 = f(Y(end)+h0*K1);
   ynew = Y(end) + h0*(0.5*K1+0.5*K2);
   % aggiorno t   
   t = t + h0;
   %salvo i valori per l'output
   T(end+1)=t;
   Y(end+1)=ynew;
end