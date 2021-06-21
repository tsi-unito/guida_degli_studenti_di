function [I,r]=cavsimp(f,a,b)
%function I=cavsimp(f,a,b)
% Input: funzione f(x) (function handle)
%           ad es. f = @(x) exp(x) 
%        a,b: estremi dell'intervallo 
% Output: I = valore approx dell'integrale
%             usando Cavalieri-Simpson
%         r = ordine di esattezza polinomiale (3)
r=3;
I = (b-a)*(f(a)/6+2/3*f((a+b)/2)+f(b)/6);