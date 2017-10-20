% This M-file defines the right-hand side of the ODE model
% equations in Two-Strain Model with Mutation
 
function Yp=size_eqns(t,Y)
 
persistent  b1 b2 v1 v2 v3 k p1 p2 w1 w2 w3 u u1 u2 u3
b1=2*10^(-6);
b2=1*10^(-7);
v1=0.00017;
v2=0.0055;
v3=0.0037;
k=0.35;
p1=0.33;
p2=0.02*0.8;
w1=0.7;
w2=0.9;
w3=0.9;
u=0.006;
u1=0.061;
u2=0.02;
u3=0.002;

% The following is to extract S,E1,E2,I1,I2,A1,and A2 from Y:
 
S=Y(1);
E=Y(2);
I1=Y(3);
I2=Y(4);
N=Y(5);
R=Y(6);
 
% Here are the real definitions of the right-hand:
Sp=u*(S+E+I1+I2+N+R)+u1*I1+u2*I2+u3*N-b1*S*I1-b2*S*I2-u*S;
Ep=b1*S*I1+b2*S*I2-(v1+v2+v3+u)*E;
I1p=v1*k*p1*E+p2*I2-(w1+u+u1)*I1;
I2p=v2*k*(1-p1)*E-(p2+w2+u+u2)*I2;
Np=v3*(1-k)*E-(w3+u+u3)*N;
Rp=w1*I1+w2*I2+w3*N-u*R;

% The following is to store those right-hand functions in Yp:
 
Yp(1)=Sp;
Yp(2)=Ep;
Yp(3)=I1p;
Yp(4)=I2p;
Yp(5)=Np;
Yp(6)=Rp;

 
% Return Yp as a vector:
 
Yp=[Yp(1);Yp(2);Yp(3);Yp(4);Yp(5);Yp(6)];