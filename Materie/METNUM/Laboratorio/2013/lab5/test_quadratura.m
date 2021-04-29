% Successione di intervalli che scende a zero
H = 2.^(- (0:10) );
for k=1:length(H)
    EXACT=(exp(H(k))-1);
    % Formula dei trapezi
    T(k) = H(k) * (0.5*exp(0) + 0.5*exp(H(k)));
    Etrap(k) = EXACT - T(k);
    % Formula del punto medio
    M(k) = H(k) * exp(H(k)/2);
    Emed(k) = EXACT - M(k);
    % Formula di Cavalieri-Simpson
    C(k) = H(k) * (1/6*exp(0) + 2/3*exp(H(k)/2) + 1/6*exp(H(k)) );
    Ecs(k) = EXACT - C(k);
end
% T = H .* (0.5*(exp(0*H) + 0.5*exp(H)) );
loglog(H , abs(Etrap) , 'b.');
% log(E) ~ m * log(H) + q
% ===> E ~ exp(q) * H^m
[m,q] = minqua(log(H') , log(abs(Etrap')));
hold on
loglog(H,abs(Etrap),'bx' ,H , exp(q)*H.^m , 'b--')
disp(sprintf('Etrap ~ %f * h^ %f',exp(q),m));

[m,q] = minqua(log(H') , log(abs(Emed')));
loglog(H,abs(Emed),'rx' , H , exp(q)*H.^m , 'r--')
disp(sprintf('Emed ~ %f * h^ %f',exp(q),m));

[m,q] = minqua(log(H') , log(abs(Ecs')));
loglog(H,abs(Ecs),'ko' , H , exp(q)*H.^m , 'k--')
disp(sprintf('Ecs ~ %f * h^ %f',exp(q),m));
hold off;
