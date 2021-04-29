function [T,Y]=calcola(f,y0,tfin,dt,RK)
% Approssima la soluzione di 
%   y' = f(y)    <--- senza la dipendenza da t!
% con il metodo Runge-Kutta (esplicito!) specificato
% a partire da y0 (colonna) e usando passo dt
% f(y) deve restituire un vettore colonna
% Se non passo RK, usa Heun come default
if nargin<5
    RK.A = [0,0 ; 1,0]; %coefficienti A
    RK.b = [1/2 ; 1/2]; %coefficienti b (in colonna!)
end

if nnz(triu(RK.A,1)>0)
    error 'Il Runge-Kutta deve essere esplicito!';
end

s = length(RK.b); %numero degli stadi
if (size(RK.A)~=[s,s])
    error 'Il numero dei coefficienti del Runge-Kutta non è consistente!';
end

K = zeros(length(y0),s); %matrice per salvare gli stadi
% uno stadio per colonna, una componente di y per ciascuna riga
t=0; %tempo corrente
T=0; Y=y0; % per l'output
while t<tfin
    if (t+dt>tfin)
        dt=tfin-t;
    end
    for i=1:s
        y1 = y0 + dt * K* (RK.A(i,:)');
        K(:,i) = f(y1); %stadio i-esimo
    end
    y1 = y0 + dt* (K*RK.b); %valore a t+dt
    t=t+dt;
    T(end+1) = t; %salvo per l'output
    Y(:,end+1) = y1;
    y0 = y1; %preparo per il nuovo passo
end