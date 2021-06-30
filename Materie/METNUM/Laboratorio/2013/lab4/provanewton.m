function E=provanewton(f,a,b,N,R)

%% Funzione e intervallo
if nargin<3
    %f=@(x) sin(x);a=0;b=2*pi;
    f=@(x)1./(1+25*x.^2);a=-5;b=5;
end

%%Numero di nodi e perturbazione random
if nargin<4
    N=9
end
if nargin<5
    R=0.0 %usare R>0 per perturbare i dati sulle y
end
r= R * ( rand(1,N)-0.5);

%%Iterpolazione con nodi equispaziati
xe=linspace(a,b,N);
fe=f(xe) + r;
ce=diffdiv(xe,fe);

%%Interpolazione con nodi di Chebichev
xc= nodichebi(N-1)*(b-a)/2 + (b+a)/2;
fc=f(xc) + r;
cc=diffdiv(xc,fc);

%%Disegno
subplot(2,1,1)
xx=linspace(a,b,500);
yy=f(xx); %valori esatti
yye=newtoneval(xe,ce,xx); %interp. nodi equisp.
yyc=newtoneval(xc,cc,xx); %interp. nodi Chebi.
plot(xx,yy,xx,yye,xx,yyc);
legend('f(x)','p(x) equisp.','p(x) Chebi.')
subplot(2,1,2)
plot(xx,yy-yye,xx,yy-yyc);
legend('p(x) equisp.','p(x) Chebi.')
ylabel 'errore di interpolazione'

%%Output errori
E(1)=max(abs(yy-yye));
disp(sprintf('Errore di interpolazione con nodi equispaziati: %f',E(1)));
E(2)=max(abs(yy-yyc));
disp(sprintf('Errore di interpolazione con nodi di Chebichev: %f',E(2)));


