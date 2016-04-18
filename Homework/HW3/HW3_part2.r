nRep = 100
nt = 100
S_t = rep(NA,nt)
Y_t = rep(NA,nt)
a = 1.2
b = 0.7
sig_S = 0.2
sig_y = 0.5
S0 = 4

par(mfrow=c(1,1))

library(TMB)
compile("gompertz.cpp")
dyn.load(dynlib("gompertz"))

parOut = array(NA,c(9,nRep,3))
modOut = array(NA,c(9,3))
nMod = 1

S_t[1] = S0
Y_t[1] = S0
for(tt in 2:nt)
{
	S_t[tt] = rnorm(1,a + b*S_t[tt-1],sig_S)
	Y_t[tt] = rnorm(1,S_t[tt],sig_y)
}

#Estimate the parameters based on the simulated parameters

#create the data
data = list(Y_t = as.vector(Y_t))
parameters = list(a = c(0), b = c(0), lnsig_S = log(0.5), lnsig_y = log(0.5), u=rep(1,nt))


Obj = MakeADFun( data=data, parameters=parameters, random="u", DLL="gompertz")
Obj$fn( Obj$par )
Obj$gr( Obj$par )

Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr)
Opt$diagnostics = data.frame( "name"=names(Obj$par), "Est"=Opt$par, "final_gradient"=as.vector(Obj$gr(Opt$par)))
Opt$par # estimated parameters
SD = sdreport( Obj ) # standard errors
plot(S_t)
lines(SD$par.random)
