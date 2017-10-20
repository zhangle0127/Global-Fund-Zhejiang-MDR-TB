 % This is the program to the two-strain ODE model
 % defined in "Two_Strain Mutation Model".
 % The model equations are defined in eqns.m.
 
 
 % The following is a list of parameters in the model equations:

global b1 b2 v1 v2 v3 k p1 p2 w1 w2 w3 u u1 u2 u3
b1=2*10^(-6);
b2=1*10^(-7);
v1=0.00017;
v2=0.0055;
v3=0.0037;
k=0.35;
p1=0.33;
p2=0.02;
w1=0.7;
w2=0.9;
w3=0.9;
u=0.006;
u1=0.061;
u2=0.02;
u3=0.002;


 % The following statement prints the parameters on the screen:

 par=[b1,b2,v1,v2,v3,k,p1,p2,w1,w2,w3,u,u1,u2,u3];

 
 % Define the time interval in the calculation:
 
 tspan=[0,50];
 
 % Define the initial values for S,E,I1,I2,N,and R:

 initial=[5000/,8000000,830,13081,24684,34794];
 
% Invoke an ODE solver when the rights hand equations are
% called from eqns.m:
 
 [t,Y]=ode45('zj0608',tspan,initial);
 [t1,Y1]=ode45('zj0608_scenario1',tspan,initial);
 [t2,Y2]=ode45('zj0608_scenario2',tspan,initial);
 [t3,Y3]=ode45('zj0608_scenario3',tspan,initial);
 [t4,Y4]=ode45('zj0608_scenario4',tspan,initial);
 % Extract S0,E11,E21,I11,I21,E11,E21,A11,A21,S1,E12,E22,I12,I22,A12,A22,and S2
 % from columns 1 through 7 from Y:

 S=Y(:,1);
 E=Y(:,2);
 I1=Y(:,3);
 I11=Y1(:,3);
 I12=Y2(:,3);
 I13=Y3(:,3);
 I14=Y4(:,3);
 I2=Y(:,4);
 I21=Y1(:,4);
 I22=Y2(:,4);
 I23=Y3(:,4);
 I24=Y4(:,4);
 N=Y(:,5);
 N1=Y1(:,5);
 N2=Y2(:,5);
 N3=Y3(:,5);
 N4=Y4(:,5);
 R=Y(:,6);

 
 % Print the dmensions of Y.More specifically, x is the dimension of rows,
 % or the number of interations, and y is the dimension of columns, which
 % is the number of the equations, that is 7 in this model.
 
 [x,y]=size(Y);
 [x1,y1]=size(Y1);
 [x2,y2]=size(Y2);
 [x3,y3]=size(Y3);
 [x4,y4]=size(Y4);
 % Print the last step of all values using either the following statement
 % or the next one:
 
 Final=[S(x),E(x),I1(x),I11(x1),I12(x2),I13(x3),I14(x4),I21(x1),I22(x2),I23(x3),I24(x4),N(x),N1(x1),N2(x2),N3(x3),N4(x4),R(x)];
 
 %% Write the final steady states in the file endemic. txt
 
 % fid=fopen('endemic.txt','w');
 
 % Plot the solutions in four subwindows. Here the first two digits (2,2)
 % specify that the graph window is to be split into an 2-by-2 grid of
 % smaller windows, and the third digit(i=1,4) specifies the i-the window
 % for the currnet plot:
 
% I1 I11 I12 are mdr-tb with mdr-tb success rate of 0.7, 0.8, 0.9
% I2 I21 I22 are non-mdr-tb with mdr-tb success rate of 0.7, 0.8, 0.9
% N N1 N2 are non-infectious-tb with mdr-tb success rate of 0.7, 0.8, 0.9

% Objective1: under current circumstance predict total-tb, mdr-tb
%  figure(1)
%   plot(t,I1,'-')
%   title('MDR-PTB vs Time') 
% figure(2)
%   plot(t,I1+I2,'-')
%   title('Infectious PTB vs Time')
% figure(3)
%   plot(t,N,'-')
%   title('Non-Infectious PTB vs Time')
% figure(4)
%     plot(t,I1+I2+N,'-')
%     title('Total TB') 
% figure(5)
%    plot(t,I1/(p2*I2),'-')
%    title('Primary/Aquired MDR-TB')



% Objective2: Predict total-TB under 
% scenario0: current;
% scenario1: mdr-tb success rate 0.9, non-mdr-ptb¡¢non-infectious-tb success rate 0.95;
% scenario2: decrease transmission rate(beta);
% scenario3: treat latent tb(v1,v2,v3 *0.9);
% scenario4: decrease acquired resistance(p2*0.9);

 figure(6)
  plot(t,I1+I2+N,'k',t1,I11+I21+N1,'b',t2,I12+I22+N2,'y',t3,I13+I23+N3,'r',t4,I14+I24+N4,'g')
  title('scenario')
  

% Predict total-TB under 
% scenario0: current;
% scenario1: mdr-tb success rate 0.9, non-mdr-ptb¡¢non-infectious-tb success rate 0.95;
% scenario2: decrease transmission rate(beta);
% scenario3: treat latent tb(v1,v2,v3 *90%);
% scenario4: decrease acquired resistance(p2);  

%  figure(6)
%   plot(t,I1,t1,I11,'-',t2,I12,'-')
%   title('MDR-PTB vs Time')

% figure(2)
%   plot(t,I2,t1,I21,'--',t2,I22,'-')
%   title('Non-MDR-PTB vs Time')

%figure(3)
%  plot(t,N,'-')
%  title('Non-infectious TB vs Time')  


