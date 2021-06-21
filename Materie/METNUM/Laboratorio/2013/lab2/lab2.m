function [B] = lab2(A,f)
%  function B=lab2(A,f)
%
% Esercizio: 
%    cosa fa questo algoritmo?
%    scrivere l'help

% (Matteo Semplice, 2012)

if nargin<2
    f = false;
end

N=size(A); 
if N(1)==N(2)
    N=N(1);
else
    error('La matrice deve essere quadrata! A è %d x %d',N(1),N(2));
end

for m=2:N
    if abs(A(m-1,m-1))<eps
        warning 'Attento!';
    end

    A(m:N,m-1)=A(m:N,m-1)/A(m-1,m-1);
    A(m:N,m:N)=A(m:N,m:N)-A(m:N,m-1)*A(m-1,m:N);

    if (f)
        disp(sprintf('Passo %d',m-1))
        A
    end
end
B=A;
