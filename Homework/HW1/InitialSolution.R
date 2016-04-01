
library(TMB)

###########
# Nonlinear optimization
###########

library("animation")  # Requires v2.12 or higher, also need to download: http://www.imagemagick.org/script/binary-releases.php

###########
# Delta-model for pollock
###########
library(TMB)
library( SpatialDeltaGLMM )

#
data( EBS_pollock_data )
CPUE = EBS_pollock_data$catch
X = cbind( "Intercept"=rep(1,length(CPUE)) )

# Step 1 -- make and compile template file
dyn.unload( dynlib("delta_model_v1") )
compile( "delta_model_v1.cpp" )

# Step 2 -- build inputs and object
dyn.load( dynlib("delta_model_v1") )
Params = list("b_j"=rep(0,ncol(X)), "theta_z"=c(0,0))

#myMod = 1, lognormal
#myMod = 2, normal
#myMod = 3, gamma

Data = list( "y_i"=CPUE, "X_ij"=X, "myMod" = 1)
Obj = MakeADFun( data=Data, parameters=Params, DLL="delta_model_v1")

# Step 3 -- test and optimize
Obj$fn( Obj$par )
Obj$gr( Obj$par )
Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr )
Opt$diagnostics = data.frame( "name"=names(Obj$par), "Est"=Opt$par, "final_gradient"=as.vector(Obj$gr(Opt$par)))
Opt$par # estimated parameters
SD = sdreport( Obj ) # standard errors

# Extract stuff
Report = Obj$report()

plot(CPUE,Report$linpred_i)

