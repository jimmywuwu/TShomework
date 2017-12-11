#2
p_0=100
eps_0=0
sig_0=1
eps_t=rnorm(1001,0,1)
r=0.02
p=c()
K=100
for (t in 1:1000){
  sig=0.01+0.02*sig_0^2+0.01*eps_0^2
  p=c(p,p_0*exp(r-1/2*sig+sqrt(sig)*rnorm(1,0,1)))
}
exp(-r)*mean((p-K)*((p-K)>0))

#3
mu=c(0.1,0.15,0.12)
sig=matrix(2.5,3,3)
sig[1,1]=0.2
sig[2,2]=0.25
sig[3,3]=0.18
sig[1,2]=0.3*sqrt(0.2)*sqrt(0.25)
sig[2,1]=sig[1,2]
sig[2,3]=0.3*sqrt(0.25)*sqrt(0.18)
sig[3,2]=sig[2,3]
sig[1,3]=0.4*sqrt(0.2)*sqrt(0.18)
sig[3,1]=0.4*sqrt(0.2)*sqrt(0.18)
M=cbind(mu,1)
B=t(M)%*%solve(sig)%*%M
m0_tu_1=c(0.02,1)
m0_tu_2=c(0.15,1)
w_1=solve(sig)%*%M%*%solve(B)%*%t(t(m0_tu_1))
w_2=solve(sig)%*%M%*%solve(B)%*%t(t(m0_tu_2))

mu_1=t(w_1)%*%mu
mu_2=t(w_2)%*%mu
sd_1=t(w_1)%*%sig%*%t(t(w_1))
sd_2=t(w_2)%*%sig%*%t(t(w_2))

x=c()
y=c()
for (alp in seq(-1,1,0.01)){
 w_=alp*w_1+(1-alp)*w_2
 x=c(x,t(w_)%*%mu)
 y=c(y,t(w_)%*%sig%*%t(t(w_)))
}
# efficient frontier
plot(y,x)

alp_1=1*t(c(1,1,1))%*%solve(sig)%*%mu  
alp_2=0.2*t(c(1,1,1))%*%solve(sig)%*%mu

opt_1=(1-alp_1[1,1])*((solve(sig)%*%c(1,1,1))/(t(c(1,1,1))%*%solve(sig)%*%c(1,1,1))[1,1])
      +alp_1[1,1]*((solve(sig)%*%mu)/(t(c(1,1,1))%*%solve(sig)%*%mu)[1,1])
opt_2=(1-alp_2[1,1])*((solve(sig)%*%c(1,1,1))/(t(c(1,1,1))%*%solve(sig)%*%c(1,1,1))[1,1])
      +alp_2[1,1]*((solve(sig)%*%mu)/(t(mu)%*%solve(sig)%*%c(1,1,1))[1,1])



