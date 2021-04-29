function [a,v,r]=maxautovalore(A,it)
% Applica it iterazioni del metodo delle potenze
%  alla matrice A
% Restituisce l'ultimo quoziente di Railegh
%  (approx dell'autovalore max) e la relativa
%  approssimazione dell'autovettore
% r = successione quozienti Railegh

if nargin<2
    it=20;
end

if nargout>2
  r=zeros(1,it);
end

N=size(A);
if (N(1)==N(2))
    N=N(1);
else
    error
end
%Posso iniziare da un vettore qualsiasi, ad esempio "ones", oppure "rand", ...
y=ones(N,1);%rand(N,1);

for k=1:it
    y = y / sqrt(y'*y);% sqrt(y'*y) = norm(y,2);
    v = A*y;     % "v" è variabile in output
    a = (v'*y) ; % quoziente di Railegh ("a" è variabile in output)
    if nargout>2
      r(k) = a;  % solo se ho richiesto la "storia di convergenza" delle a 
    end
    y = v;
end
