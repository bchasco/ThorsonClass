nS = 10 #number of sites
mu = 2 #mean site effect
sig_s = 1 #the site variance
#Site effects
lam_s = exp(rnorm(nS,mu,sig_s))


#log counts
sig_y = 0.5 #observation error
nObs = 10 #number of observations at each site
yibar = rnorm(nS,lam_s,sig_y) #process error in true lambda

yi = matrix(NA,10,10) #tmp matrix of obsrvations
for(i in 1:nS)
{
	yi[i,] = rpois(nObs,yibar) #observations
}

yi = as.vector(yi)

n_y = nS * nObs #number of observations
n_s = nS #number of sites
s_i = rep(0:(nS-1),each=nObs)  #index for sites
y_i = yi


#Run part 1
library(TMB)
compile( "hw2_part1.cpp")

Data = list(n_y = n_y, n_s = n_s, s_i = s_i, y_i = y_i)
Parameters = list(x0 = c(0))
dyn.load( dynlib("hw2_part1") )
Obj = MakeADFun(data=Data, parameters=Parameters)  #
Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr, control=list("trace"=1) )
# Get reporting and SEs
Report = Obj$report()
SD = sdreport( Obj )

#part 2 - which is site effects
library(TMB)
compile( "hw2_part2.cpp")
Data = list(n_y = n_y, n_s = n_s, s_i = s_i, y_i = y_i)
Parameters = list(x0 = c(0), log_sdz = c(0), z_s = rep(0,nS))

dyn.load( dynlib("hw2_part2") )
Obj = MakeADFun(data=Data, parameters=Parameters)  #
Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr, control=list("trace"=1))
# Get reporting and SEs
Report = Obj$report()
SD = sdreport( Obj )

#part 3 - only over-dispersion
library(TMB)
compile( "hw2_part3.cpp")
Data = list(n_y = n_y, n_s = n_s, s_i = s_i, y_i = y_i)
Parameters = list(x0 = c(0), log_sdz = c(0))

dyn.load( dynlib("hw2_part3") )
Obj = MakeADFun(data=Data, parameters=Parameters, DLL="hw2_part3")  #
Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr, control=list("trace"=1))
# Get reporting and SEs
Report = Obj$report()
SD = sdreport( Obj )
