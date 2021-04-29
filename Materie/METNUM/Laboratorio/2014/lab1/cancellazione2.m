xx = logspace(0, 10,20);
% Sostituisco il ciclo for con le operazioni "element-wise"
% non serve più preallocare err
S = sqrt(xx.^2+1);
E = 1./(S+xx);
F = S-xx; 
err = abs((E-F)./E);
loglog(xx,err,'ro-')
