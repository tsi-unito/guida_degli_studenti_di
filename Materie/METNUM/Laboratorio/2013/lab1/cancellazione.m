xx = logspace(0, 10,20);
err = zeros(1,length(xx)); %length = lunghezza vettore
for k = 1:length(xx) %sintassi: for variabile=vettore dei valori
    x = xx(k);
    E = 1/(sqrt(x^2+1)+x);
    F = sqrt(x^2+1)-x; 
    err(k) = abs((E-F)/E);
end
loglog(xx,err,'ro-')
