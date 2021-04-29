function [A,S]=ferrovielombarde()
% Restituisce la matrice A dei link ferroviari
% fra le stazioni S. S è un cell-array con le
% stringhe dei nomi delle stazioni

S{1}='Milano';
S{2}='Pavia';
S{3}='Lodi';
S{4}='Brescia';
S{5}='Bergamo';
S{6}='Como';
S{7}='Varese';
S{8}='Lecco';
S{9}='Sondrio';
S{10}='Cremona';
S{11}='Mantova';
S{12}='Asso';

N = length(S);

% Matrice dei collegamenti ferroviari
A=zeros(11,11);
I=[];
J=[];
%Milano
j=[2,3,4,5,6,7,12];
I=[I,1*ones(1,length(j))];
J=[J,j];
%Lodi
j=10;
I=[I,3*ones(1,length(j))];
J=[J,j];
%Brescia
j=[10,5];
I=[I,4*ones(1,length(j))];
J=[J,j];
%Bergamo
j=[6,8];
I=[I,5*ones(1,length(j))];
J=[J,j];
%Como
j=[8];
I=[I,6*ones(1,length(j))];
J=[J,j];
%Lecco
j=[9];
I=[I,8*ones(1,length(j))];
J=[J,j];
%10
j=[11];
I=[I,8*ones(1,length(j))];
J=[J,j];

A=sparse(I,J,1+0*I,N,N);
A=full(A+A');

