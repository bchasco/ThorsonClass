
setwd( "/home/brian/Documents/Classes/2016_Spatio-temporal_models/Week 2 -- mixed-effects/Homework" )

# ############
# # Generalized linear mixed model
# ############
# library(lme4)

# ###### Simulate data
# # Parameters
# set.seed(1000)
# Nsite = 10
# Nobs_per_site = 10
# Site_logMean = 2
# Site_logSd = 1
# var_y = 0.5 # overdispersion

# # Bookkeeping
# s_i = rep( 1:Nsite, each=Nobs_per_site)

# # Simulation
# loglambda_s = rnorm( Nsite, mean=Site_logMean, sd=Site_logSd )
# logmean_i = rnorm(Nsite*Nobs_per_site, mean=loglambda_s[s_i], sd=sqrt(var_y))
# y_i = rpois( Nsite*Nobs_per_site, lambda=exp(logmean_i))

# # Plot data
# library(lattice)
# histogram( ~ y_i | factor(s_i), breaks=seq( min(y_i), max(y_i), length=10), type="density", panel=function(x,...){ panel.histogram(x, ...); panel.mathdensity(dmath=dnorm, col="black", args = list(mean=mean(x),sd=sd(x))) } )      #

# ###### Fit using R
# # No site level (Not recommended)
# GLM1 = glm( y_i ~ 1, family="poisson" )
# print( summary(GLM1) )

# # Using fixed effects (Not recommended)
# GLM2 = glm( y_i ~ 0 + factor(s_i), family="poisson" )
# print( summary(GLM2) )

# # Using mixed effects (Recommended) -- doesn't appear to use REML
# library(lme4)
# GLMM = glmer( y_i ~ 1 + (1 | factor(s_i)), family="poisson" )
# print( summary(GLMM) )

# ####################
# # Fit using TMB
# ####################
# library(TMB)

# # ------------------------------------------------------------
# # Model 1 = GLM with no site effect
# Version = "mod1"
# compile( paste0(Version,".cpp") )

# Data = list( "n_y"=length(y_i), "y_i"=y_i)
# Parameters = list( "x0"=-10 )

# # Build object
# dyn.load( dynlib(Version) )
# Obj = MakeADFun(data=Data, parameters=Parameters)  #

# # Prove that function and gradient calls work
# Obj$fn( Obj$par )
# Obj$gr( Obj$par )

# # Optimize
# start_time = Sys.time()
# Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr, control=list("trace"=1) )
#   Opt[["final_gradient"]] = Obj$gr( Opt$par )
#   Opt[["total_time"]] = Sys.time() - start_time

# # Get reporting and SEs
# Report = Obj$report()
# SD = sdreport( Obj )

# # Compare intercepts
# c( GLM1$coefficients, Report$x0 )

# # --------------------------------------------------------------------
# # Model 2 = random effect of site (no overdispersion)
# dyn.unload(dynlib(Version))
# Version = "mod2"
# compile( paste0(Version,".cpp") )

# Data = list( "n_y"=length(y_i), "n_s"=length(unique(s_i)), "s_i"=s_i-1, "y_i"=y_i)
# Parameters = list( "x0"=-10, "log_sdz"=2, "z_s"=rep(0,Data$n_s) )
# Random = c("z_s")

# # Build object
# dyn.load( dynlib(Version) )
# Obj = MakeADFun(data=Data, parameters=Parameters)  #

# # Prove that function and gradient calls work
# Obj$fn( Obj$par )
# Obj$gr( Obj$par )

# # Optimize
# start_time = Sys.time()
# Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr, control=list("trace"=1) )
#   Opt[["final_gradient"]] = Obj$gr( Opt$par )
#   Opt[["total_time"]] = Sys.time() - start_time

# # Get reporting and SEs
# Report = Obj$report()
# SD = sdreport( Obj )

# # Compare intercepts
# summary(GLMM)
# Report

# # -----------------------------------------------------------
# # Model 3 = overdispersion (no random effect of site)
# dyn.unload(dynlib(Version))
# Version = "mod3"
# compile( paste0(Version,".cpp") )

# Data = list( "n_y"=length(y_i), "y_i"=y_i)
# Parameters = list( "x0"=-10, "log_sdz"=2, "z_i"=rep(0,Data$n_y) )
# Random = c("z_i")

# # Build object
# dyn.load( dynlib(Version) )
# Obj = MakeADFun(data=Data, parameters=Parameters)  #

# # Prove that function and gradient calls work
# Obj$fn( Obj$par )
# Obj$gr( Obj$par )

# # Optimize
# start_time = Sys.time()
# Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr, control=list("trace"=1) )
#   Opt[["final_gradient"]] = Obj$gr( Opt$par )
#   Opt[["total_time"]] = Sys.time() - start_time

# # Get reporting and SEs
# Report = Obj$report()
# SD = sdreport( Obj )

# Report

# # -----------------------------------------------------------
# # Model 4 = random effect of site + overdispersion
# dyn.unload(dynlib(Version))
# Version = "mod4_v1"
# compile( paste0(Version,".cpp") )

# Data = list( "n_y"=length(y_i), "n_s"=length(unique(s_i)), "s_i"=s_i-1, "y_i"=y_i)
# Parameters = list( "x0"=-10, "log_sdz"=2, "log_sdy"=2, "z_s"=rep(0,Data$n_s), "z_i"=rep(0,Data$n_y) )
# Random = c("z_i","z_s")

# # Build object
# dyn.load( dynlib(Version) )
# Obj = MakeADFun(data=Data, parameters=Parameters)  #

# # Prove that function and gradient calls work
# Obj$fn( Obj$par )
# Obj$gr( Obj$par )

# # Optimize
# start_time = Sys.time()
# Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr, control=list("trace"=0,"abs.tol"=1e-20) )
#   Opt[["final_gradient"]] = Obj$gr( Opt$par )
#   Opt[["total_time"]] = Sys.time() - start_time

# # Get reporting and SEs
# Report = Obj$report()
# SD = sdreport( Obj )

# Report

# ----------------------------------------------------------------
# Simulation experiment
library(TMB)

# parameters
set.seed(100)
n.sim = 100
Nsite = 10
Nobs_per_site = 10
Site_logMean = 2
Site_logSd = 1
var_y = 0.5 # overdispersion

# Bookkeeping
s_i = rep( 1:Nsite, each=Nobs_per_site)

# For each Simulation, store estimates of mu and its SE for all 4 models
results <- array(NA,dim=c(n.sim,4,3)) # dim = n.sim, n.model, (mu,SE,captured)
for(sim in 1:n.sim){
	loglambda_s = rnorm( Nsite, mean=Site_logMean, sd=Site_logSd )
	logmean_i = rnorm(Nsite*Nobs_per_site, mean=loglambda_s[s_i], sd=sqrt(var_y))
	y_i = rpois( Nsite*Nobs_per_site, lambda=exp(logmean_i))

	# fit Model 1 = GLM with no site effect
	if(exists("Version")) dyn.unload(dynlib(Version))
	Version = "mod1"
	compile( paste0(Version,".cpp") )
	Data = list( "n_y"=length(y_i), "y_i"=y_i)
	Parameters = list( "x0"=-10 )
	dyn.load( dynlib(Version) )
	Obj = MakeADFun(data=Data, parameters=Parameters)  #
	Obj$fn( Obj$par )
	Obj$gr( Obj$par )
	start_time = Sys.time()
	Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr, control=list("trace"=0) )
	  Opt[["final_gradient"]] = Obj$gr( Opt$par )
	  Opt[["total_time"]] = Sys.time() - start_time
	SD = sdreport( Obj )
	results[sim,1,1] = Obj$report()$x0
	results[sim,1,2] = SD$sd

	# fit Model 2 = random effect of site (no overdispersion)
	dyn.unload(dynlib(Version))
	Version = "mod2"
	compile( paste0(Version,".cpp") )
	Data = list( "n_y"=length(y_i), "n_s"=length(unique(s_i)), "s_i"=s_i-1, "y_i"=y_i)
	Parameters = list( "x0"=-10, "log_sdz"=2, "z_s"=rep(0,Data$n_s) )
	Random = c("z_s")
	dyn.load( dynlib(Version) )
	Obj = MakeADFun(data=Data, parameters=Parameters)  #
	Obj$fn( Obj$par )
	Obj$gr( Obj$par )
	start_time = Sys.time()
	Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr, control=list("trace"=0) )
	  Opt[["final_gradient"]] = Obj$gr( Opt$par )
	  Opt[["total_time"]] = Sys.time() - start_time
	SD = sdreport( Obj )
	results[sim,2,1] = SD$value["x0"]
	results[sim,2,2] = SD$sd[length(SD$sd)]

	# fit Model 3 = overdispersion (no random effect of site)
	dyn.unload(dynlib(Version))
	Version = "mod3"
	compile( paste0(Version,".cpp") )
	Data = list( "n_y"=length(y_i), "y_i"=y_i)
	Parameters = list( "x0"=-10, "log_sdz"=2, "z_i"=rep(0,Data$n_y) )
	Random = c("z_i")
	dyn.load( dynlib(Version) )
	Obj = MakeADFun(data=Data, parameters=Parameters)  #
	Obj$fn( Obj$par )
	Obj$gr( Obj$par )
	start_time = Sys.time()
	Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr, control=list("trace"=0) )
	  Opt[["final_gradient"]] = Obj$gr( Opt$par )
	  Opt[["total_time"]] = Sys.time() - start_time
	SD = sdreport( Obj )	
	results[sim,3,1] = SD$value["x0"]
	results[sim,3,2] = SD$sd[length(SD$sd)]

	# fit Model 4 = random effect of site + overdispersion
	dyn.unload(dynlib(Version))
	Version = "mod4_v1"
	compile( paste0(Version,".cpp") )
	Data = list( "n_y"=length(y_i), "n_s"=length(unique(s_i)), "s_i"=s_i-1, "y_i"=y_i)
	Parameters = list( "x0"=-10, "log_sdz"=2, "log_sdy"=2, "z_s"=rep(0,Data$n_s), "z_i"=rep(0,Data$n_y) )
	Random = c("z_i","z_s")
	dyn.load( dynlib(Version) )
	Obj = MakeADFun(data=Data, parameters=Parameters)  #
	Obj$fn( Obj$par )
	Obj$gr( Obj$par )
	start_time = Sys.time()
	Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr, control=list("trace"=0,"abs.tol"=1e-20) )
	  Opt[["final_gradient"]] = Obj$gr( Opt$par )
	  Opt[["total_time"]] = Sys.time() - start_time
	SD = sdreport( Obj )	
	results[sim,4,1] = SD$value["x0"]
	results[sim,4,2] = SD$sd[length(SD$sd)]	
}

# Determine if each confidence interval captured the true value of mu = 2
for(sim in 1:n.sim){
	for(mod in 1:4){
		lower <- results[sim,mod,1] - 1.96*results[sim,mod,2]
		upper <- results[sim,mod,1] + 1.96*results[sim,mod,2]
		results[sim,mod,3] <- ifelse(lower < 2 && upper > 2,TRUE,FALSE)
	}
}

# Calculate proportion of time each model captures truth
final.table <- array(NA,dim=c(4,2))
for(mod in 1:4){
	final.table[mod,2] <- sum(results[,mod,3],na.rm=TRUE)/n.sim
	final.table[mod,1] <- mean(results[,mod,1])
}
colnames(final.table) <- c("mean.mu","prop.in.CI")
final.table

