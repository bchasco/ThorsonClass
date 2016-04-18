nRep = 100
nt = 100
S_t = rep(NA,nt)
Y_t = rep(NA,nt)
a_sim = c(-0.5,0.0,0.5)
sig_S = 0.4
sigy_sim = c(0.5,1.0,2.0)

par(mfrow=c(1,1))

compile("linear_sim.cpp")
dyn.load(dynlib("linear_sim"))

parOut = array(NA,c(9,nRep,3))
modOut = array(NA,c(9,3))
nMod = 1

for(a in a_sim)
{
	for(sig_y in sigy_sim)
	{
		for(ss in 1:nRep)
		{
			S_t[1] = rnorm(1,0,sig_S)
			Y_t[1] = rnorm(1,S_t[1],sig_y)
			for(tt in 2:nt)
			{
				S_t[tt] = rnorm(1,a*S_t[tt-1],sig_S)
				Y_t[tt] = rnorm(1,S_t[tt],sig_y*sig_S)
			}

			#Estimate the parameters based on the simulated parameters

			#create the data
			data = list(Y_t = as.vector(Y_t))
			parameters = list(a = c(0), lnsig_S = log(sig_S), lnsig_y = log(sig_y), u=rep(0,nt))


			Obj = MakeADFun( data=data, parameters=parameters, random="u", DLL="linear_sim")
			Obj$fn( Obj$par )
			Obj$gr( Obj$par )

			Opt = nlminb( start=Obj$par, objective=Obj$fn, gradient=Obj$gr)
			Opt$diagnostics = data.frame( "name"=names(Obj$par), "Est"=Opt$par, "final_gradient"=as.vector(Obj$gr(Opt$par)))
			Opt$par # estimated parameters
			SD = sdreport( Obj ) # standard errors
			parOut[nMod,ss,] = c(Opt$par[1],exp(Opt$par[2:3]))
		}#end reps

		aa = paste(a, "(", round(quantile(parOut[nMod,,1], probs=c(0.025))[1],2), ",", round(quantile(parOut[nMod,,1], probs=c(0.975))[1],2), ")")
		my_sig_S = paste(sig_S, "(", round(quantile(parOut[nMod,,2], probs=c(0.025))[1],2), ",", round(quantile(parOut[nMod,,2], probs=c(0.975))[1],2), ")")
		my_sig_y = paste(sig_y*sig_S, "(", round(quantile(parOut[nMod,,3], probs=c(0.025))[1],2), ",", round(quantile(parOut[nMod,,3], probs=c(0.975))[1],2), ")")
		modOut[nMod,1] = aa
		modOut[nMod,2] = my_sig_S
		modOut[nMod,3] = my_sig_y
		nMod = nMod + 1		
	}
}
