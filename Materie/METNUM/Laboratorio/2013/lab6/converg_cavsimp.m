HH=2.^(-(0:7)); E=[];
f=@(x) exp(x);
for k=1:length(HH)
    h=HH(k);
    I=cavsimp(f,0,h);
    E(k)=abs(exp(h)-1 - I);
end
loglog(HH,E,'.-');
r = minqua(log(HH),log(E)); %usando la funzione della volta scorsa
% oppure usando la funzione di MatLab:
% p = polyfit(log(HH),log(E),1); r=p(1);
disp(sprintf('Ordine di convergenza %f',r));
