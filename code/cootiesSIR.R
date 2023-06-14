library(deSolve)
closed.sir.model <- function (t, x, params) {
  ## first extract the state variables
  S <- x[1]
  I <- x[2]
  R <- x[3]
  ## now extract the parameters
  beta <- params["beta"]
  gamma <- params["gamma"]
  N <- S+I+R
  ## now code the model equations
  dSdt <- -beta*S*I/N
  dIdt <- beta*S*I/N-gamma*I
  dRdt <- gamma*I
  ## combine results into a single vector
  dxdt <- c(dSdt,dIdt,dRdt)
  ## return result as a list!
  list(dxdt)
}


parms <- c(beta=1,gamma=1/10)
times <- seq(from=0,to=60,by=1)
xstart <- c(S=999,I=1,R=0)



library(tidyverse)
ode(
  func=closed.sir.model,
  y=xstart,
  times=times,
  parms=parms
) %>%
  as.data.frame() -> out



out %>%
  gather(variable,value,-time) %>%
  ggplot(aes(x=time,y=value,color=variable))+
  geom_line(size=2)+
  theme_classic()+
  labs(x='time (days)',y='number of individuals')



#Now for cooties
closed.sir.model <- function (t, x, params) {
  ## first extract the state variables
  Sg <- x[1]
  Sb <- x[2]
  Ig <- x[3]
  Ib <- x[4]
  ## now extract the parameters
  beta <- params["beta"]
  #gamma <- params["gamma"]
  Nb <- Sb+Ib
  Ng <- Sb+Ig
  ## now code the model equations
  dSgdt <- -beta*Sg*Ib/Nb
  dSbdt <- -beta*Sb*Ig/Ng
  dIgdt <- beta*Sg*Ib/Nb
  dIbdt <- beta*Sb*Ig/Ng
  ## combine results into a single vector
  dxdt <- c(dSgdt,dSbdt,dIgdt,dIbdt)
  ## return result as a list!
  list(dxdt)
}


parms <- c(beta=0.25)#,gamma=365/13)
times <- seq(from=0,to=60,by=1)
xstart <- c(Sg=15, Sb=29,Ig=0, Ib = 1)

library(tidyverse)
ode(
  func=closed.sir.model,
  y=xstart,
  times=times,
  parms=parms
) %>%
  as.data.frame() -> out



out %>%
  gather(variable,value,-time) %>%
  ggplot(aes(x=time,y=value,color=variable))+
  geom_line(size=2)+
  theme_classic()+
  labs(x='time (days)',y='number of individuals')




#Now for cooties with recoveries
closed.sir.model <- function (t, x, params) {
  ## first extract the state variables
  Sg <- x[1]
  Sb <- x[2]
  Ig <- x[3]
  Ib <- x[4]
  Rg <- x[3]
  Rb <- x[4]
  ## now extract the parameters
  beta <- params["beta"]
  gamma <- params["gamma"]
  Nb <- Sb+Ib + Rb
  Ng <- Sb+Ig + Rg
  ## now code the model equations
  dSgdt <- -beta*Sg*Ib/Nb
  dSbdt <- -beta*Sb*Ig/Ng
  dIgdt <- beta*Sg*Ib/Nb - gamma*Ig
  dIbdt <- beta*Sb*Ig/Ng - gamma*Ib
  dRgdt <- gamma*Ig
  dRbdt <- gamma*Ib
  ## combine results into a single vector
  dxdt <- c(dSgdt,dSbdt,dIgdt,dIbdt,dRgdt,dRbdt)
  ## return result as a list!
  list(dxdt)
}


parms <- c(beta=0.25,gamma=1/60)
times <- seq(from=0,to=180,by=1)
xstart <- c(Sg=50, Sb=49,Ig=0, Ib = 1, Rg = 0, Rb=0)

library(tidyverse)
ode(
  func=closed.sir.model,
  y=xstart,
  times=times,
  parms=parms
) %>%
  as.data.frame() -> out



out %>%
  gather(variable,value,-time) %>%
  ggplot(aes(x=time,y=value,color=variable))+
  geom_line(size=2)+
  theme_classic()+
  labs(x='time (days)',y='number of individuals') + 
  ylim(0,50)
