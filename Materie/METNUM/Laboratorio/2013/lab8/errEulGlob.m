%Verifichiamo che l'errore globale del metodo di 
% Eulero esplicito sia O(h)
%Usiamo come esempio la ODE
%  y' = y(1-y)
%  y(0) = y0 = 0.3
% la cui soluzione esatta è
%  y(t) = y0*exp(t)/(1+y0*(exp(t)-1))
  
y0=0.3;
esatta = y0*exp(1)/(1+y0*(exp(1)-1)); % al tempo t=1

clear H err
for k=1:6
    h=2^(-k); % lunghezza del passo
    N=2^k;    % numero dei passi

    %Applico N passi di E.E. di lunghezza h
    y1=y0;
    for i=1:N
        y1 = y1 + h* y1*(1-y1);
    end
    H(k) = h;
    err(k) = abs(y1-esatta);
end
loglog(H, err,'o-');
title 'Errore globale del metodo Eulero esplicito (tfin=1)'
xlabel 'passo'
ylabel 'errore'

p=polyfit(log(H), log(err) ,1);
disp(sprintf('Ordine di convergenza misurato: %f',p(1)))