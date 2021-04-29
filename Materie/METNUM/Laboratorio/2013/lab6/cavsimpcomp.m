function I=cavsimpcomp(f,a,b,N)
%function I=cavsimpcomp(f,a,b,N)
% Input: funzione f(x) (function handle)
%        a,b: estremi dell'intervallo 
%        N: numero sottointervalli
% Output: valore approx dell'integrale
%         usando Cavalieri-Simpson composita

h=(b-a)/N;
% I=0;
% for k=1:N
%     I = I + cavsimp(f, a+(k-1)*h , a+k*h);
% end

x = linspace(a,b,N+1); %estremi degli intervalli
m = (a+h/2) : h : b; %punti medi degli intervalli
% oppure m = linspace(a+h/2,b-h/2,N);
x = x(2:N); %tolgo a e b da x
I = h*( f(a)/6+f(b)/6 + sum(f(x))/3 + sum(f(m))*2/3); 