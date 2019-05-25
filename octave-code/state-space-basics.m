# http://ctms.engin.umich.edu/CTMS/index.php?example=Introduction&section=ControlStateSpace

pkg load control;

A = [ 0   1   0
     980  0  -2.8
      0   0  -100 ];

B = [ 0
      0
      100 ];

C = [ 1 0 0 ];

# Eigenvalues of system matrix A are poles of Transfer function
poles = eig(A);

t = 0:0.01:2;
u = zeros(size(t));
x0 = [0.01 0 0];

sys = ss(A,B,C,0);

# Unstable system. watch the ball fall to the ground
[y,t,x] = lsim(sys,u,t,x0);
plot(t,y)
title('Open-Loop Response to Non-Zero Initial Condition')
xlabel('Time (sec)')
ylabel('Ball Position (m)')

# Is the system controllable? (Can you reach any state from some u?)
rank(ctrb(A, B)) == 3

# Is the system observable (Can you determine initial state from data?)
rank(obsv(A, C)) == 3

# Below, we'll do reference control with reference of zero

# Choose poles for a stable system

p1 = -10 + 10i;
p2 = -10 - 10i;
p3 = -50;

K = place(A, B, [p1, p2 p3]);
sys_cl = ss(A - B * K, B, C, 0); # Updated a matrix after controller

lsim(sys_cl, u, t, x0);
xlabel('Time (sec)')
ylabel('Ball Position (m)')

# Now choose poles "closer to the left" to improve transient response

p1 = -20 + 20i;
p2 = -20 - 20i;
p3 = -100;

K = place(A, B, [p1, p2 p3]);
sys_cl = ss(A - B * K, B, C, 0); # Updated a matrix after controller

lsim(sys_cl, u, t, x0);
xlabel('Time (sec)')
ylabel('Ball Position (m)')

# Choose the observer matrix
# Need the observer to be faster than system itself
# Rule of thumb is that poles are 5 times further to the left
op1 = -100;
op2 = -101;
op3 = -102; # need three different poles for place command

# Exploit the Duality Between controllability and observability
L = place(A', C', [op1 op2 op3]);

# Now for the combined system: controlled system plus estimation error:
At = [A - B * K        B * K
      zeros(size(A))   A - L * C];:0

