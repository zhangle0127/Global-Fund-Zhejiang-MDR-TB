% This M-file defines the right-hand side of the ODE model
% equations in Two-Strain Model with Mutation
 
function Yp=size_eqns(t,Y)
 
global global a b1 b2 b3 u d1 d2 d3 k p1 p2 p3 o1 o2 m q r1 r2 r3 w
 
% The following is to extract S,E1,E2,I1,I2,A1,and A2 from Y:
 
S=Y(1);
E=Y(2);
I1=Y(3);
I2=Y(4);
I3=Y(5);
R=Y(6);
 
% Here are the real definitions of the right-hand:
Sp=a-b1*S*I1-b2*S*I2-b3*S*I3-u*S;
Ep=b1*S*I1+b2*S*I2+b3*S*I3-(k*p1+k*p2+k*p3+u)*E+w*R;
I1p=k*p1*E-(o1+q*r1+u+d1)*I1;
I2p=k*p2*E+o1*m*I1-(o2+q*r2+u+d2)*I2;
I3p=k*p3*E+o1*(1-m)*I1+o2*I2-(q*r3+u+d3)*I3;
Rp=q*r1*I1+q*r2*I2+q*r3*I3-(w+u)*R;
 
% The following is to store those right-hand functions in Yp:
 
Yp(1)=Sp;
Yp(2)=Ep;
Yp(3)=I1p;
Yp(4)=I2p;
Yp(5)=I3p;
Yp(6)=Rp;

 
% Return Yp as a vector:
 
Yp=[Yp(1);Yp(2);Yp(3);Yp(4);Yp(5);Yp(6)];