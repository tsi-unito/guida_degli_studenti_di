function x=lusolve(B,b)
% b è un vettore
% function x=lusolve(B,b)
% Risolve il sistema lineare L*U*x=b
% dove
% L=tril(B,-1)+eye(N)
% U=triu(B)
% Se il sistema da risolvere è A*x=b, 
% la matrice B è quella resituita da lunopivot(A)

% (Matteo Semplice, 2012)

N=size(B);
if N(1)==N(2)
    N=N(1);
else
    error('La matrice deve essere quadrata! B è %d x %d',N(1),N(2));
end

% Non servono! 
% Posso usare direttamente B nei cicli for
% L=tril(B,-1)+eye(N);
% U=triu(B);

%Risolve il sistema triangolare L*y=b
y=b; % y(1)=b(1) !
for k=2:N
    y(k) = b(k) - B(k,1:k-1)*y(1:k-1);
end


% Questo for non si può "vettorizzare"
% perché ciascuna iterazione usa un valore
% calcolato nella precedente.

%Risolve il sistema triangolare U*x=y
x=y;
x(N)=x(N)/B(N,N);
for k=N-1:-1:1
    x(k) = y(k) - B(k,k+1:N)*x(k+1:N);
    x(k) = x(k) / B(k,k);
end
