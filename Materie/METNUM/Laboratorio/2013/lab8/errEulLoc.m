%Verifichiamo che l'errore locale del metodo di 
% Eulero esplicito sia O(h^2)
%Usiamo come esempio la ODE
%  y' = y(1-y)
%  y(0) = y0 = 0.3
% la cui soluzione esatta è
%  y(t) = y0*exp(t)/(1+y0*(exp(t)-1))
  
y0=0.3;

H= 2.^ (0:-1:-6);
clear err
for k=1:length(H)
    h=H(k);
    esatta = y0*exp(h)/(1+y0*(exp(h)-1)); % al tempo t=h
    y1 = y0 + h* y0*(1-y0);  %Metodo di E.E.
    err(k) = abs(y1-esatta);
end
loglog(H, err,'o-');
title 'Errore locale del metodo Eulero esplicito'
xlabel 'passo'
ylabel 'errore'

p=polyfit(log(H), log(err) ,1);
disp(sprintf('Ordine di convergenza misurato: %f',p(1)))