function [Q,n]=autoquad(f,a,b,toll,S)
% function [Q,n]=autoquad(f,a,b,toll,S)
% f(x), estremi dell'intervallo a e b, tolleranza
% S deve essere una funzione S(f,a,b) che applica una 
%   formula di quadratura semplice ad f(x) su [a,b]
%   dichiarata come [I,r]=S(f,a,b)
%   con I=integrale approx
%       r=ordine esattezza polinomiale
% Output: Q=valore approx del'integrale a meno di toll
%         n=numero totale di intervalli usati
[I,r]=S(f,a,b);
I1=S(f,a,(a+b)/2); 
I2=S(f,(a+b)/2,b);
E = (I-I1-I2)/(2^(r+1)-1);
if (abs(E) > toll)
    [I1,n1] = autoquad(f,a,(a+b)/2,toll/2 , S);
    [I2,n2] = autoquad(f,(a+b)/2,b,toll/2 , S);
    n=n1+n2;
else
    x=[(3*a+b)/4,(a+3*b)/4];plot(x,f(x),'.');hold on;
    n=2;
end
Q= I1+I2;

