function [m,q]=minqua(X,Y)
% function [m,q]=minqua(X,Y)
% Input: elenco di ascisse e ordinate
%        dei punti da approssimare
% Output: coeff angolare (m) e termine noto (q)
%        della retta che approssima ai minimi quadrati
%        i dati X e Y

%Assicuriamoci che X e Y siano colonne
if (size(X,2)>1)
    X=X';
end
if (size(Y,2)>1)
    Y=Y';
end

N=length(X);
V= ones(N,2);
V(:,1)=X; %assume che X sia una colonna
%
A = V'*V;
b = V'*Y; %assume che Y sia una colonna
% OPPURE
% A=ones(2);
% A(1,1) = sum(X.^2);
% A(1,2) = sum(X);
% A(2,1) = A(1,2);
% A(2,2) = N;
% b = ones(2,1);
% b(1) = sum(X.*Y);
% b(2) = sum(Y);
% (Assumo solo che X e Y siano entrambi righe o
%  entrambi colonne)

%a = lusolve(ludecomp(A) , b);
a = A \ b;
m = a(1);
q = a(2);
