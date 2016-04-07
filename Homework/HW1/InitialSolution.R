
library(TMB)
library( SpatialDeltaGLMM )
data( EBS_pollock_data )
CPUE = EBS_pollock_data$catch


HW1_Q1_Table = matrix(NA, 3,3)

#####################Question 1
# -- make and compile template file
################################################################

dyn.unload( dynlib("delta_model_v1") )
compile( "delta_model_v1.cpp" )
dyn.load( dynlib("delta_model_v1") )

#myMod = 1, lognormal
#myMod = 2, normal
#myMod = 3, gamma

###################################################
#  Part 1
###################################################

X = cbind( "Intercept"=rep(1,length(CPUE)) )
Data = list( "y_i"=CPUE, "X_ij"=X, "myMod" = 1, predTF_i=rep(0,length(CPUE)))
Params = list("b_j"=rep(0,ncol(X)), "theta_z"=c(0,0))

Obj = MakeADFun( data=Data, parameters=Params, DLL="delta_model_v1")
Obj$fn( Obj$par )
Obj$gr( Obj$par )

Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr )
Opt$diagnostics = data.frame( "name"=names(Obj$par), "Est"=Opt$par, "final_gradient"=as.vector(Obj$gr(Opt$par)))
Opt$par # estimated parameters
SD = sdreport( Obj ) # standard errors
Report = Obj$report() # Extract stuff

#Save results of likelihood and number of parameters
HW1_Q1_Table[1,1] = sum(Report$jnll_i)
HW1_Q1_Table[2,1] = length(Obj$par)

# Step 1 -- divide into partitions
K = 10
Partition_i = sample( x=1:K, size=length(CPUE), replace=TRUE )
PredNLL_k = rep(NA, K)
# Step 2 --Loop through partitions
for(k in 1:K){
	dyn.load( dynlib("delta_model_v1") )
	Params = list("b_j"=rep(0,ncol(X)), "theta_z"=c(0,0))
	Data = list( "y_i"=CPUE, "X_ij"=X, "myMod" = 1, predTF_i=ifelse(Partition_i==k,1,0) )
	Obj = MakeADFun( data=Data, parameters=Params, DLL="delta_model_v1")
	Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr, iterations=1000)
	Report = Obj$report()
	PredNLL_k[k] = Report$pred_jnll
}

# log-Predictive probability per datum
#Save results of cross-validation
logPredProb_byMod = mean( PredNLL_k / table(Partition_i) )
HW1_Q1_Table[3,1] = logPredProb_byMod


#####################Question 1
# Parts 2 and 3
################################################################

#myMod = 1, lognormal
#myMod = 2, normal
#myMod = 3, gamma

for(mm in 2:3)
{
	X = cbind( "Intercept"=rep(1,length(CPUE)), EBS_pollock_data$year-mean(EBS_pollock_data$year))
	Data = list( "y_i"=CPUE, "X_ij"=X, "myMod" = mm, predTF_i=rep(0,length(CPUE)))
	Params = list("b_j"=rep(0,ncol(X)), "theta_z"=c(0,0))

	Obj = MakeADFun( data=Data, parameters=Params, DLL="delta_model_v1")

	# Step 3 -- test and optimize
	Obj$fn( Obj$par )
	Obj$gr( Obj$par )
	Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr )
	Opt$diagnostics = data.frame( "name"=names(Obj$par), "Est"=Opt$par, "final_gradient"=as.vector(Obj$gr(Opt$par)))
	Opt$par # estimated parameters
	SD = sdreport( Obj ) # standard errors
	Report = Obj$report() # Extract stuff

	HW1_Q1_Table[1,mm] = sum(Report$jnll_i)
	HW1_Q1_Table[2,mm] = length(Obj$par)

	# Step 1 -- divide into partitions
	K = 10
	Partition_i = sample( x=1:K, size=length(CPUE), replace=TRUE )
	PredNLL_k = rep(NA, K)
	# Step 2 --Loop through partitions
	for(k in 1:K){
		dyn.load( dynlib("delta_model_v1") )
		Params = list("b_j"=rep(0,ncol(X)), "theta_z"=c(0,0))
		Data = list( "y_i"=CPUE, "X_ij"=X, "myMod" = mm, predTF_i=ifelse(Partition_i==k,1,0) )
		Obj = MakeADFun( data=Data, parameters=Params, DLL="delta_model_v1")
		Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr, iterations=1000)
		Report = Obj$report()
		PredNLL_k[k] = Report$pred_jnll
	}

	# log-Predictive probability per datum
	logPredProb_byMod = mean( PredNLL_k / table(Partition_i) )
	HW1_Q1_Table[3,mm] = logPredProb_byMod
}
HW1_Q1_Table = as.data.frame(HW1_Q1_Table)
names(HW1_Q1_Table) = c("lognormal", "normal", "gamma")
print(round(HW1_Q1_Table,2))

