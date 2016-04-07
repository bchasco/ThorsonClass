#Question 2

#Part 1

#dyn.unload( dynlib("delta_model_v1") )
compile( "delta_model_v1.cpp" )
dyn.load( dynlib("delta_model_v1") )

#Step 1 get the MLE parameters
X = cbind( "Intercept"=rep(1,length(CPUE)) )
Data = list( "y_i"=CPUE, "X_ij"=X, "myMod" = 1, predTF_i=rep(0,length(CPUE)))
Params = list("b_j"=rep(0,ncol(X)), "theta_z"=c(0,0))
Obj = MakeADFun( data=Data, parameters=Params, DLL="delta_model_v1")
lnormOpt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr )
SD = sdreport( Obj ) # standard errors

nSim = 100  #number of simulations
nMod = 3 #number of different models
simPar_lnorm = array(NA,c(nSim,nMod))
for(mm in 1:3)
{
	for(i in 1:nSim)
	{
		#Step 2 simulate from the MLE
		#simulate the zeros
		nn = length(CPUE)
		zeros = rbinom(nn,1,1/(1 + exp(-rnorm(nn,SD$par.fixed[2],SD$cov.fixed[2,2]))))
		mu = rnorm(nn,SD$par.fixed[1],SD$cov.fixed[1,1])
		sig = exp(rnorm(nn,SD$par.fixed[3],SD$cov.fixed[3,3]))
		#simulate the CPUE
		simCPUE = zeros * exp(rnorm(nn,mu,sig))

		#Run the model on the simulated CPUE data, but changing the likelihood from log-normal to gamma to normal
		Data = list( "y_i"=simCPUE, "X_ij"=X, "myMod" = mm, predTF_i=rep(0,length(CPUE)))
		Params = list("b_j"=rep(0,ncol(X)), "theta_z"=c(0,0))
		Obj = MakeADFun( data=Data, parameters=Params, DLL="delta_model_v1")
		Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr )
		#save the estimate parameters
		simPar_lnorm[i,mm] = Opt$par[1]
	}#end sim
}#end model


#Part 2

#dyn.unload( dynlib("delta_model_v1") )
compile( "delta_model_v1.cpp" )
dyn.load( dynlib("delta_model_v1") )

#Step 1 get the MLE parameters
X = cbind( "Intercept"=rep(1,length(CPUE)), EBS_pollock_data$year-mean(EBS_pollock_data$year))
Data = list( "y_i"=CPUE, "X_ij"=X, "myMod" = 2, predTF_i=rep(0,length(CPUE)))
Params = list("b_j"=rep(0,ncol(X)), "theta_z"=c(0,0))
Obj = MakeADFun( data=Data, parameters=Params, DLL="delta_model_v1")
gammaOpt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr )
SD = sdreport( Obj ) # standard errors

nSim = 100  #number of simulations
nMod = 3 #number of different models
simPar_gamma = array(NA,c(nSim,nMod))
for(mm in 1:3)
{
	for(i in 1:nSim)
	{
		#Step 2 simulate from the MLE
		#simulate the zeros
		nn = length(CPUE)
		theta = rnorm(nn,SD$par.fixed[3],SD$cov.fixed[3,3])
		zeros = rbinom(nn,1,1/(1 + exp(-theta)))

		scale = exp(rnorm(nn,SD$par.fixed[1],SD$cov.fixed[1,1]) + rnorm(nn,SD$par.fixed[2],SD$cov.fixed[2,2]))
		shape = exp(rnorm(nn,SD$par.fixed[4],SD$cov.fixed[4,4]))
		#simulate the CPUE
		simCPUE = zeros * rgamma(nn,mu,scale=scale/mu)

		#Run the model on the simulated CPUE data, but changing the likelihood from log-normal to gamma to normal
		Data = list( "y_i"=simCPUE, "X_ij"=X, "myMod" = mm, predTF_i=rep(0,length(CPUE)))
		Params = list("b_j"=rep(0,ncol(X)), "theta_z"=c(0,0))
		Obj = MakeADFun( data=Data, parameters=Params, DLL="delta_model_v1")
		Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr )
		#save the estimate parameters
		simPar_gamma[i,mm] = Opt$par[1]
	}#end sim
}#end model

#Part 3

#dyn.unload( dynlib("delta_model_v1") )
compile( "delta_model_v1.cpp" )
dyn.load( dynlib("delta_model_v1") )

#Step 1 get the MLE parameters
X = cbind( "Intercept"=rep(1,length(CPUE)), EBS_pollock_data$year-mean(EBS_pollock_data$year))
Data = list( "y_i"=CPUE, "X_ij"=X, "myMod" = 3, predTF_i=rep(0,length(CPUE)))
Params = list("b_j"=rep(0,ncol(X)), "theta_z"=c(0,0))
Obj = MakeADFun( data=Data, parameters=Params, DLL="delta_model_v1")
normOpt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr )
SD = sdreport( Obj ) # standard errors

nSim = 100  #number of simulations
nMod = 3 #number of different models
simPar_norm = array(NA,c(nSim,nMod))
for(mm in 1:3)
{
	for(i in 1:nSim)
	{
		#Step 2 simulate from the MLE
		#simulate the zeros
		nn = length(CPUE)
		theta = rnorm(nn,SD$par.fixed[3],SD$cov.fixed[3,3])
		zeros = rbinom(nn,1,1/(1 + exp(-theta)))

		mu = rnorm(nn,SD$par.fixed[1],SD$cov.fixed[1,1]) + rnorm(nn,SD$par.fixed[2],SD$cov.fixed[2,2])
		sd = rnorm(nn,SD$par.fixed[4],SD$cov.fixed[4,4])
		#simulate the CPUE
		simCPUE = zeros * rnorm(nn,mu,sd)

		#Run the model on the simulated CPUE data, but changing the likelihood from log-normal to gamma to normal
		Data = list( "y_i"=simCPUE, "X_ij"=X, "myMod" = mm, predTF_i=rep(0,length(CPUE)))
		Params = list("b_j"=rep(0,ncol(X)), "theta_z"=c(0,0))
		Obj = MakeADFun( data=Data, parameters=Params, DLL="delta_model_v1")
		Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr )
		#save the estimate parameters
		simPar_norm[i,mm] = Opt$par[1]
	}#end sim
}#end model


mlePar = c(lnormOpt$par[1], gammaOpt$par[1], normOpt$par[1])
contingencyTable = matrix(c(colMeans(simPar_lnorm),colMeans(simPar_gamma),colMeans(simPar_norm)),nMod,nMod)
sim = round(cbind(mlePar,contingencyTable),2)
row.names(sim) = c("sim_lnorm", "sim_gamma", "sim_norm")
sim = as.data.frame(sim)
names(sim) = c("mlePars", "lnorm", "gamma", "norm")
print(sim)

