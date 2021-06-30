function dy = f(t,y)
dy=zeros(size(y));
s=sin(t);
dy(1) = -2*y(1) + y(2) +2*s;
dy(2) = 998*y(1) - 999*y(2) + 999*(cos(t)-s);
