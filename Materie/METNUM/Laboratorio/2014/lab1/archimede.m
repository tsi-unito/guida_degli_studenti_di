% calcola 30 elementi della successione p(k)
%  dei perimetri dei poligoni inscritti
%  con 2^k lati
% 4 lati, perimetro 4*sqrt(2)
k = 2; % N lati = 2^k
p(2) = 4*sqrt(2);
P(2) = 4*sqrt(2);

k_max = 30;
K=k:k_max;

for k=K(2:end)
    l = p(k-1) / 2^(k-1); %ricava il lato dal perimetro
    l1 = sqrt(2-sqrt(4-l^2)); %lato per il poligono successivo
    p(k) = 2^k * l1; %perimetro poligono successivo

    l = P(k-1) / 2^(k-1);
    l1 = l / sqrt(2+sqrt(4-l^2)); %lato per il poligono successivo
    P(k) = 2^k * l1; %perimetro poligono successivo
end
figure(1)
plot(K,p(K) , 'bo-' , K , P(K), 'rv-')
title 'p(k) dovrebbe convergere a 2\pi dal basso'
figure(2)
semilogy(K , abs(2*pi - p(K)) , 'o-' , K , abs(2*pi-P(K)), 'rv-')
title 'La precisione si arresta a 10^{-8} con la prima formula'


