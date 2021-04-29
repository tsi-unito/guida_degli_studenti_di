function c=diffdiv(xn,fn)
%function c=diffdiv(xn,fn)
% Calcola i coefficienti del polinomio interpolatore
% nella forma di Newton, per i dati (xn,fn) 
% Usa l'algoritmo delle differenze divise
% In ingresso xn sono i nodi di interpolazione
%             fn=f(xn) valori della funzione nei nodi
% In uscita
% c(k) = f[xn(1),...,xn(k)]

N=length(xn);
c=fn;
for j=1:(N-1)
    for k=N:-1:(j+1)
        c(k) = (c(k)-c(k-1))/(xn(k)-xn(k-j));
    end
end
        
        
        
        
        
        
        

