library(kyotil)
library(robustrank)
if(!unix()) setwd("D:/gdrive/wmw/partially_matched_rank/R/")
    

# process arguments
Args <- commandArgs(trailingOnly=TRUE) 
if (length(Args)==0) {
    # batch.size has to be at least 10 b/c the number of simulations run for size and power are different
    # scenario is var|asym|perm|pow. var means we are checking asymptotic distribution, asym and perm controls permutation test or large sample test
    #Args=c(batch.size="2",batch.number="1",  m="50",n.x="50",n.y="50", distr="logistic", params="0,.8,1", scenario="asym")
    #Args=c(batch.size="2",batch.number="1",  m="50",n.x="50",n.y="50", distr="lognormal", params=".3,0,1", scenario="asym") # works for both normal and lognormal
    Args=c(batch.size="2",batch.number="1",  m="20",n.x="5",n.y="0", distr="normal", params="0.3,.8,1", scenario="pow") # works for both normal and lognormal
    #Args=c(batch.size="2",batch.number="1",  m="10",n.x="10",n.y="10", distr="mixnormal", param1=".9",param2=".5",param3="1")
}
myprint(Args)
i=0;
i=i+1; batch.size=as.numeric(Args[i])
i=i+1; batch=as.numeric(Args[i])
i=i+1; m=as.numeric(Args[i])
i=i+1; n.x=as.numeric(Args[i])
i=i+1; n.y=as.numeric(Args[i])
i=i+1; distr=Args[i]
i=i+1; params.str=Args[i]
i=i+1; scenario=Args[i]
    
seeds=1:batch.size+batch.size*(batch-1); names(seeds)=seeds
myprint(batch, batch.size)
    
myprint(params.str)    
paramss=as.numeric(strsplit(params.str,",")[[1]])

# continuity correction
cont.corr=F # previously, by default it is corrected
    
verbose=T
res.len=NA

begin=Sys.time()
res=sapply(seeds, simplify="array", function (seed) {
#seed=491 # gives t.2=0
    myprint(seed)
    
    if (distr %in% c("normal","student","logistic","lognormal")) {
        params=c(loc.2=paramss[1],rho=paramss[2],scale.2=paramss[3])
        if (distr=="lognormal") {
            # rho is not the rho of X and Y and need to be converted empirically
            if(params["rho"]==0.3) params["rho"]=0.37
            if(params["rho"]==0.5) params["rho"]=0.59
            if(params["rho"]==0.8) params["rho"]=0.85
        }
    } else if (distr=="mixnormal") {
        params=c(p.1=paramss[1],p.2=paramss[2],sd.n=paramss[3])
    } else if (distr=="gamma") {
        params=c(loc.2=paramss[1],shape.1=paramss[2],shape.2=paramss[3],rate.1=paramss[4],rate.2=paramss[5],rho=paramss[6])
    }    
    dat=sim.partially.matched(m=m,n.x=n.x,n.y=n.y,distr=distr,params=params,seed=seed)
    X=dat$X; Y=dat$Y; Xprime=dat$Xprime; Yprime=dat$Yprime;# Xmissing=dat$Xmissing; Ymissing=dat$Ymissing    

    # for development use. create an independe dataset for estimating F under null
    dat.2=sim.partially.matched(m=m,n.x=n.x,n.y=n.y,distr=distr,params=params,seed=seed+100)
    F.=with(dat.2, ecdf(c(X,Y,Yprime)))
    
    # this returns the statistics for checking asymptotic variance
    if (scenario=="var") return(pm.wilcox.test(X, Y, Xprime, Yprime, mode="var"))
    
    alter="two.sided"
    
    test.sr=wilcox.test(X, Y, paired=T,conf.int=T, alternative=alter) # sign rank test using only paired data
    if(n.y!=0) test.mw=wilcox.test(X,Yprime,conf.int=T, alternative=alter) # WMW using only X and Yprime
    
    if(n.x==0 & n.y==0) {
        test=pair.wmw.test(X, Y, method="large.0", p.method="asymptotic")
        res=c(
              "SR"= test.sr$p.val
            , "MW"= if(n.y!=0) test.mw$p.val else NA
            , "pair_wmw" = test$p.val
            #, "MW1"= wilcox.test(c(X,Xprime),c(Y,Yprime))$p.val # WMW using all data in an inappropriate way, not admissible compared to BP
        )    
    } else if (n.x>0 | n.y>0) {
        test=pm.wilcox.test(X, Y, Xprime, Yprime, method="all", corr=cont.corr) #, F.=F.
        res=c(
              "SR"= test.sr$p.val
            , "MW"= if(n.y!=0) test.mw$p.val else NA
            , test[,2]    
            #, "MW1"= wilcox.test(c(X,Xprime),c(Y,Yprime))$p.val # WMW using all data in an inappropriate way, not admissible compared to BP
        )    
    }
    
    # Weighted Z test from Kuan2013simple
    if (n.x>0 & n.y>0) {
        test.mw.1=wilcox.test(Xprime,Yprime,conf.int=T, alternative=alter) # WMW using only Xprime and Yprime
        p.1=test.sr$p.val; p.2=test.mw.1$p.val
# one.sided <-> two.sided conversions are commented out because it messes up type 1 error rates
#        if(alter=="two.sided") {
#            # convert to one-sided p values
#            p.1=p.1/2; if(test.sr$estimate<0) p.1=1-p.1
#            p.2=p.1/2; if(test.mw$estimate<0) p.2=1-p.2
#        }
        # weights
        w.1=sqrt(m); w.2=sqrt(n.x*n.y/(n.x+n.y))
        p.c=1-pnorm((w.1*qnorm(1-p.1)+w.2*qnorm(1-p.2))/sqrt(w.1**2+w.2**2))
#        if(alter=="two.sided") {
#            # convert to two-sided p value
#            if(p.c<0.5) p.c=2*p.c else p.c=2*(1-p.c)
#        }
        # return results
        res=c(res,"WZ"=p.c)
    }
    
    eval(eval(substitute(expression( res.len <<- length(res) ))))     # set res.len to be used outside the current environment
    
    res
                
})
names(dimnames(res))=c("test","seed")


# save results
sim=paste(distr,m,n.x,n.y,params.str,sep="_")
if(cont.corr) sim=sim%.%"_corr" else sim=sim%.%"_notcorr" 
foldername="res_"%+%scenario%+%"/"; if(!file.exists(foldername)) dir.create(foldername) # res/
foldername=foldername%+%   sim%+%"/"; if(!file.exists(foldername)) dir.create(foldername) # res/sim/
#foldername=foldername%+%   sim%+%"_"%+%method%+%"_n.x"%+%n.x %+%"/"; if(!file.exists(foldername)) dir.create(foldername) # res/sim/sim_method_n.x
save (res, file=foldername%+%"/batch"%+%formatInt(batch, 3)%+%".Rdata")
# note time passed
done = Sys.time()
body1=format(done-begin)
print(date())
print("time used: "%+%body1)

############################################################################################################
#        # additional tests of potential interest
#        , "sign-mw" = # combine sign with MW
#             sign.mw.test(Y, X, Yprime)
#          "sign,complete"= # sign test on complete data
#            binom.test(sum(X>Y), m)$p.val
#        , "mw,complete1"=
#            wilcox.test(X,Y)$p.val
#        , "sr,all"= # sign rank test on all data
#            wilcox.test(c(X,Xmissing), c(Y,Yprime), paired=T)$p.val
#        , "mw,XYYprime"= # MW disregarding the paired structure
#            wilcox.test(X,c(Y,Yprime))$p.val
#            , "mw new emp"=pchisq((unname(wilcox.test(dat$x, dat$y)$stat)/m/(m+n.x)-0.5)**2/0.00186, df=1, lower=F)
#            , "signed rank.obs"=wilcox.test(dat$x[!is.na(dat$y)], Y, paired=T)$p.val
