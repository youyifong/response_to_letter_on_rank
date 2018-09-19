m.text=c(mw.mw.l0="MW-MW$^l_{0}$",mw.mw.2="MW-MW$_{2}$",mw.mw.2.perm="MW-MW$^{perm}_{2}$",sr.mw.l0="SR-MW$^l_{0}$",sr.mw.2="SR-MW$_{2}$",SR="SR")
m.legend=c(mw.mw.l0=expression(MW-MW[0]^l),mw.mw.2=expression(MW-MW[2]),mw.mw.2.perm=expression(MW-MW[2]^perm),sr.mw.l0=expression(SR-MW[0]^l),sr.mw.2=expression(SR-MW[2]),SR=expression(SR))
 

#############################################################################
# power comparison at sample sizes studied by Guo et al., m=20
    
# continuity correction. with correction, type 1 error becomes slightly more elevated. We prefer not corrected here.
corr=F
m=20    
library(kyotil)
for (method in c("size","pow"))  {
# method="size"    
    rhos=c(0,0.5,0.8); names(rhos)=rhos #0.95
    reses=list()
    #distrs=c("normal"); names(distrs)=distrs
    distrs=c("normal","logistic","gamma","lognormal")
    
    reses = list()
    for (distr in distrs) {
    #distr="normal"
        
        l_n=list(
              #c(lx=5, n=5) 
              c(lx=10, n=5) 
            , c(lx=10, n=10) 
            , c(lx=40, n=5) 
        )        
        idxes=1:length(l_n)
        for (idx in idxes) {
            names(l_n)[idx]="("%.%l_n[[idx]]["lx"]%.%","%.%l_n[[idx]]["n"]%.%")"
        }        
        names(idxes)=names(l_n)
        
        res=sapply(idxes, simplify="array", function (idx){
            out=sapply(rhos, simplify="array", function (rho){
                if(method=="pow") {
                    if(distr=="normal" | distr=="lognormal") {
                        param=(if(m==50) ".3" else ".3")%.%","%.%rho%.%",1"
                    } else if (distr=="logistic") {
                        param=(if(m==50) ".5" else ".6")%.%","%.%rho%.%",1"
                    } else if (distr=="gamma") {
                        param=(if(m==50) "0.4" else "0.36")%.%",3,3,1,1,"%.%rho
                    } else stop("no distr")
                } else {
                    if(distr=="normal" | distr=="lognormal") {
                        param="0,"%.%rho%.%",1"
                    } else if (distr=="logistic") {
                        param="0,"%.%rho%.%",1"
                    } else if (distr=="gamma") {
                        param="0,3,3,1,1,"%.%rho
                    } else stop("no distr")
                }
        
                sim=paste(distr,m,l_n[[idx]]["lx"],l_n[[idx]]["n"],param,sep="_") 
                if(corr) sim=sim%.%"_corr" else sim=sim%.%"_notcorr" 
                print(method%.%" "%.%sim)
                stats=get.sim.res ("res_"%.%method%.%"/"%.%sim, verbose=T)
                apply(stats, 1, function(x) mean(x[1:10e3]<0.05,na.rm=T)) # na should be very few, so the choice of na.action does not matter here
            })  
            out
              
        }) 
        reses[[distr]]=res
        res
                    
        # tables
        if (method=="size") {
            labels=c("sr.mw.l0","sr.mw.2","mw.mw.2","mw.mw.2.perm","mw.mw.l0"); names(labels)=labels; labels=rev(labels)
            col.headers="\\hline\n  $(l,n)$ 
            &  \\multicolumn{"%.%length(rhos)%.%"}{c|}{"%.%m.text[labels[1]]%.%"}  
            &  \\multicolumn{"%.%length(rhos)%.%"}{c|}{"%.%m.text[labels[2]]%.%"}  
            &  \\multicolumn{"%.%length(rhos)%.%"}{c|}{"%.%m.text[labels[3]]%.%"}   
            &  \\multicolumn{"%.%length(rhos)%.%"}{c|}{"%.%m.text[labels[4]]%.%"}  
            &  \\multicolumn{"%.%length(rhos)%.%"}{c}{"%.%m.text[labels[5]]%.%"}  \\\\  \n"
        } else if (method=="pow") {
            labels=c("sr.mw.l0","sr.mw.2","SR","mw.mw.2.perm","mw.mw.l0"); names(labels)=labels; labels=rev(labels)
            col.headers="\\hline\n  $(l,n)$ 
            &  \\multicolumn{"%.%length(rhos)%.%"}{c|}{"%.%m.text[labels[1]]%.%"}  
            &  \\multicolumn{"%.%length(rhos)%.%"}{c|}{"%.%m.text[labels[2]]%.%"}  
            &  \\multicolumn{"%.%length(rhos)%.%"}{c|}{"%.%m.text[labels[3]]%.%"}  
            &  \\multicolumn{"%.%length(rhos)%.%"}{c|}{"%.%m.text[labels[4]]%.%"}  
            &  \\multicolumn{"%.%length(rhos)%.%"}{c}{"%.%m.text[labels[5]]%.%"}  \\\\  \n"
        }
        #labels=c("mw.mw.l0","mw.mw.2","mw.mw.2.perm","sr.mw.l0","sr.mw.2","SR")
        #col.headers="\\hline\n  $(l,n)$ &  \\multicolumn{3}{c|}{MW-MW$^l_{0}$}  &  \\multicolumn{3}{c|}{MW-MW$_{2}$}  &  \\multicolumn{3}{c|}{MW-MW$^{perm}_{2}$}  &  \\multicolumn{3}{c|}{SR-MW$^l_{0}$}  &  \\multicolumn{3}{c|}{SR-MW$_{2}$}  &  \\multicolumn{3}{c}{SR}  \\\\  \n"
        tab=rbind(
              do.call(cbind, lapply(labels, function (i) t(res[i,,]) ))
        )
        names(dimnames(tab))=c("(l,n)","$\\rho$") #$m\\backslash\\rho$
        tab=100*tab
        round(tab, 1)
            
        mytex(tab, file="tables/"%.%method%.%"_"%.%distr%.%"_letter_1_m"%.%m%.%ifelse(corr,"_corr","_notcorr"), sanitize.text.function = function(x) x, include.colnames = T, include.rownames=T, include.dup.rownames=T, digits=ifelse(method=="pow",0,1)
            , col.headers = col.headers
            , align=c("c",rep(c(rep("c",length(rhos)-1),"c|"),ncol(tab)/length(rhos)-1),"c",rep("c",length(rhos)-1))
            #, align=c("c",rep(c("c","c","c|"),ncol(tab)/length(rhos)-1),"c","c","c")
        )
    
    }

    # figures
    if(method=="pow"){
        mypdf(file="pow_all"%.%ifelse(corr,"_corr","_notcorr"), width=15, height=15, mfrow=c(4,4))
            for(distr in distrs){        
                for (i in c("(10,5)","(40,5)")) {
                    lim=range(reses[[distr]][,,i])
                    tmp=t(reses[[distr]][c("sr.mw.l0","sr.mw.2","SR"),,i]) 
                    mymatplot(tmp, xaxt="n", ylab="power", xlab=expression(rho), lwd=2, lty=c(1,2,1), col=c(1,1,"darkgray"), legend=c(expression(SR-MW[0]^l), expression(SR-MW[2]), "SR"), main=i, ylim=lim)
                    axis(1, at=1:length(rhos), labels=rhos)
                    tmp=t(reses[[distr]][c("mw.mw.l0","mw.mw.2.perm","SR"),,i]) 
                    mymatplot(tmp, xaxt="n", ylab="power", xlab=expression(rho), lwd=2, lty=c(1,2,1), col=c(1,1,"darkgray"), legend=c(expression(MW-MW[0]^l), expression(MW-MW[2]^perm), "SR"), main=i, ylim=lim )
                    axis(1, at=1:length(rhos), labels=rhos)
                }
            }
        dev.off()
    
        for(distr in distrs){
            lim=range(reses[[distr]][,,c("(10,5)","(40,5)")])
            mypdf(file="pow_sr_"%.%distr%.%ifelse(corr,"_corr","_notcorr"), width=10, height=5, mfrow=c(1,2))
                for (i in c("(10,5)","(40,5)")) {
                    tmp=t(reses[[distr]][c("sr.mw.l0","sr.mw.2","SR"),,i]) 
                    mymatplot(tmp, xaxt="n", ylab="power", xlab=expression(rho), lwd=c(2,2,1), lty=c(1,1,3), col=c(1,"darkgray",1), legend=c(expression(SR-MW[0]^l), expression(SR-MW[2]), "SR"), main=i, ylim=lim )
                    axis(1, at=1:length(rhos), labels=rhos)
                }
            dev.off()
            
            mypdf(file="pow_mw_"%.%distr%.%ifelse(corr,"_corr","_notcorr"), width=10, height=5, mfrow=c(1,2))
                for (i in c("(10,5)","(40,5)")) {
                    tmp=t(reses[[distr]][c("mw.mw.l0","mw.mw.2.perm","SR"),,i]) 
                    mymatplot(tmp, xaxt="n", ylab="power", xlab=expression(rho), lwd=c(2,2,1), lty=c(1,1,3), col=c(1,"darkgray",1), legend=c(expression(MW-MW[0]^l), expression(MW-MW[2]^perm), "SR"), main=i, ylim=lim )
                    axis(1, at=1:length(rhos), labels=rhos)
                }
            dev.off()
        }
    
        #comparing sr and mw-related tests
        for(distr in distrs){
            mypdf(file="pow_sr_mw_"%.%distr%.%ifelse(corr,"_corr","_notcorr"), width=10, height=5, mfrow=c(1,2))
                lim=range(reses[[distr]][c("sr.mw.l0","sr.mw.2", "mw.mw.l0","mw.mw.2.perm"),,c("(10,5)","(40,5)")])
                for (i in c("(10,5)","(40,5)")) {
                    labels=c("mw.mw.l0","mw.mw.2.perm","sr.mw.l0","sr.mw.2"); names(labels)=labels
                    tmp=t(reses[[distr]][labels,,i]) 
                    mymatplot(tmp, xaxt="n", ylab="power", xlab=expression(rho), lwd=2, lty=c(1,1,2,2), col=c(1,"darkgray",1,"darkgray"), legend=m.legend[labels], main=i, ylim=lim )
                    axis(1, at=1:length(rhos), labels=rhos)
                }
            dev.off()
        }
    
    }# end if pow

} # end for size/pow



#############################################################################
# power comparison when there are unpaired observations from one sample

corr=F
m=20    
library(kyotil)
for (method in c("size","pow"))  {
#method="size"
    rhos=c(0,0.5,0.8); names(rhos)=rhos #0.95
    reses=list()
    #distrs=c("normal"); names(distrs)=distrs
    distrs=c("normal","logistic","gamma","lognormal")
    
    reses = list()
    for (distr in distrs) {
    #distr="normal"
        
        l_n=list(
              #c(lx=5, n=5) 
              c(lx=10, n=0) 
            , c(lx=40, n=0) 
        )        
        idxes=1:length(l_n)
        for (idx in idxes) {
            names(l_n)[idx]="("%.%l_n[[idx]]["lx"]%.%","%.%l_n[[idx]]["n"]%.%")"
        }        
        names(idxes)=names(l_n)
        
        res=sapply(idxes, simplify="array", function (idx){
            out=sapply(rhos, simplify="array", function (rho){
                if(method=="pow") {
                    if(distr=="normal" | distr=="lognormal") {
                        param=(if(m==50) ".3" else ".3")%.%","%.%rho%.%",1"
                    } else if (distr=="logistic") {
                        param=(if(m==50) ".5" else ".6")%.%","%.%rho%.%",1"
                    } else if (distr=="gamma") {
                        param=(if(m==50) "0.4" else "0.36")%.%",3,3,1,1,"%.%rho
                    } else stop("no distr")
                } else {
                    if(distr=="normal" | distr=="lognormal") {
                        param="0,"%.%rho%.%",1"
                    } else if (distr=="logistic") {
                        param="0,"%.%rho%.%",1"
                    } else if (distr=="gamma") {
                        param="0,3,3,1,1,"%.%rho
                    } else stop("no distr")
                }
        
                sim=paste(distr,m,l_n[[idx]]["lx"],l_n[[idx]]["n"],param,sep="_") 
                if(corr) sim=sim%.%"_corr" else sim=sim%.%"_notcorr" 
                print(method%.%" "%.%sim)
                stats=get.sim.res ("res_"%.%method%.%"/"%.%sim, verbose=T)
                apply(stats, 1, function(x) mean(x[1:10e3]<0.05,na.rm=T)) # na should be very few, so the choice of na.action does not matter here
            })  
            out
              
        }) 
        reses[[distr]]=res
        res

        # tables
        if (method=="size") {
            labels=c("sr.mw.l0","mw.mw.l0"); names(labels)=labels; labels=rev(labels)
            col.headers="\\hline\n  $(l,n)$ 
            &  \\multicolumn{"%.%length(rhos)%.%"}{c|}{"%.%m.text[labels[1]]%.%"}  
            &  \\multicolumn{"%.%length(rhos)%.%"}{c}{"%.%m.text[labels[2]]%.%"}  \\\\  \n"
        } else if (method=="pow") {
            labels=c("sr.mw.l0","SR","mw.mw.l0"); names(labels)=labels; labels=rev(labels)
            col.headers="\\hline\n  $(l,n)$ 
            &  \\multicolumn{"%.%length(rhos)%.%"}{c|}{"%.%m.text[labels[1]]%.%"}  
            &  \\multicolumn{"%.%length(rhos)%.%"}{c|}{"%.%m.text[labels[2]]%.%"}  
            &  \\multicolumn{"%.%length(rhos)%.%"}{c}{"%.%m.text[labels[3]]%.%"}  \\\\  \n"
        }
        #labels=c("mw.mw.l0","mw.mw.2","mw.mw.2.perm","sr.mw.l0","sr.mw.2","SR")
        #col.headers="\\hline\n  $(l,n)$ &  \\multicolumn{3}{c|}{MW-MW$^l_{0}$}  &  \\multicolumn{3}{c|}{MW-MW$_{2}$}  &  \\multicolumn{3}{c|}{MW-MW$^{perm}_{2}$}  &  \\multicolumn{3}{c|}{SR-MW$^l_{0}$}  &  \\multicolumn{3}{c|}{SR-MW$_{2}$}  &  \\multicolumn{3}{c}{SR}  \\\\  \n"
        tab=rbind(
              do.call(cbind, lapply(labels, function (i) t(res[i,,]) ))
        )
        names(dimnames(tab))=c("(l,n)","$\\rho$") #$m\\backslash\\rho$
        tab=100*tab
        round(tab, 1)
            
        mytex(tab, file="tables/"%.%method%.%"_"%.%distr%.%"_letter_2_m"%.%m%.%ifelse(corr,"_corr","_notcorr"), sanitize.text.function = function(x) x, include.colnames = T, include.rownames=T
            , include.dup.rownames=T, digits=ifelse(method=="pow",0,1)
            , col.headers = col.headers
            , align=c("c",rep(c(rep("c",length(rhos)-1),"c|"),ncol(tab)/length(rhos)-1),"c",rep("c",length(rhos)-1))
            #, align=c("c",rep(c("c","c","c|"),ncol(tab)/length(rhos)-1),"c","c","c")
        )
        
    } #end for distr
    
    
} # end for size/pow
    
