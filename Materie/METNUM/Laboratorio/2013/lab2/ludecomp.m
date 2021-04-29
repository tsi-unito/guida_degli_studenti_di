function [B,P]=ludecomp(A,pivoting,printdetails)
%  function B=ludecomp(A,pivoting)
% Calcola la decomposizione P*A=L*U
% Se pivoting=0, non usa pivoting
%            =1, usa il primo pivot nonzero
%            =2, usa il pivot più grande
% Restituisce una matrice quadrata B tale che
% L=tril(B,-1)+eye(N)
% U=triu(B)
% e un vettore P di N elementi con l'ordine delle equazioni
% La soluzione di A*x=b si può ottenere con
%  lusolve(B,b(P))

% (Matteo Semplice, 2012)

if nargin<2
    pivoting=2;
end

if nargin<3
    printdetails=false;
end

N=size(A);
if N(1)==N(2)
    N=N(1);
else
    error('La matrice deve essere quadrata! A è %d x %d',N(1),N(2));
end

P=1:N;
for m=2:N
    if (printdetails && pivoting)
        disp('Choosing pivot among:')
        A(m-1:N,m-1)
    end
    if (pivoting==2)
        %Finds biggest pivot
        [x,k] = max(abs(A(m-1:N,m-1)));
        %se k=1, il pivot è in riga m-1
        % se k=2, il pivot è in riga m, etc
        k = k + m-2;
    else
        k=m-1; %no pivoting
        if (pivoting==1)
            while (abs(A(k,k))<eps)
                k=k+1;
            end
        end
    end
    %Scambia le righe, se necessario
    if (k>m-1)
        if (printdetails)
            disp(sprintf('Scambio righe %d e %d',m-1,k))
        end
        P([m-1,k]) = P([k,m-1]);
        A([m-1,k],:) = A([k,m-1],:);
    end
    %NOTA: per efficienza, se le matrici fossero 
    % molto grandi, non converrebbe scambiare
    % i dati in memoria, ma accedere agli elementi
    % di A tramite p: ad es la riga qui sotto diventerebbe
    %    A(p(m:N),p(m-1)) = A(p(m:N),p(m-1))/A(p(m-1),p(m-1))
    A(m:N,m-1)=A(m:N,m-1)/A(m-1,m-1);
    A(m:N,m:N)=A(m:N,m:N)-A(m:N,m-1)*A(m-1,m:N);

    if (printdetails)
        disp(sprintf('Passo %d',m-1))
        A
    end


end
B=A;
