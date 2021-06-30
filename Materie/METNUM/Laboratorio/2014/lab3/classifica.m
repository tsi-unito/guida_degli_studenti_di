[A,S]=ferrovielombarde();
N=size(A,1);

n_link = sum(A,1);
%vettore riga con la somma lungo le colonne

% for k=1:N
%     B(:,k)=A(:,k)/n_link(k);
% end
B = A ./ (ones(N,1)*n_link);

[a,v]=maxautovalore(B,100);
[s,ind]=sort(v);%v ordinato e ordine degli indici
for k=N:-1:1
    disp(sprintf('%15s %f',S{ind(k)},s(k)/s(end)));
end


