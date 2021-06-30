function y=newtoneval(xn,c,x)
% Valuta il polinomio di newton con nodi xn
% e coefficienti c, nel/nei punto/i x
N=length(c);
y=c(N) * ones(size(x));
for k=N-1:-1:1
    y = y.*(x-xn(k)) + c(k); %<<< usare .*
end