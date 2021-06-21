function B=lunopivot(A,printdetails)
%  function B=lunopivot(A,printdetails)
% Calcola la decomposizione A=L*U (senza pivoting)
% e restituisce il risultato in una sola matrice
% quadrata B tale che
% L=tril(B,-1)+eye(N)
% U=triu(B)
%
% printdetails controlla la stampa ogni passo
% (default=false)

% (Matteo Semplice, 2012)

if nargin<2
    printdetails = false;
end

N=size(A); % numero di righe e di colonne
if N(1)==N(2)
    N=N(1); % È quadrata!
else
    error('La matrice deve essere quadrata! A è %d x %d',N(1),N(2));
end

for m=2:N
    if abs(A(m-1,m-1))<eps %eps è variabile predefinita
        warning 'Pivot < precisione macchina!';
    end
    A(m:N,m-1)=A(m:N,m-1)/A(m-1,m-1); %moltiplicatori
    A(m:N,m:N)=A(m:N,m:N)-A(m:N,m-1)*A(m-1,m:N); %passo di LU

    % L'ultimo comando "vettorizza" il seguente ciclo for
    %for k=m:N
    %    A(k,m:N)=A(k,m:N)-A(k,m-1)*A(m-1,m:N);
    %end

    if (printdetails)
        disp(sprintf('Passo %d',m-1))
        A
    end
end
B=A;
