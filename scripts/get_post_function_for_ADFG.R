# Get Posterior Summaries/Plot
# 
# Function written by:
#
# Ben Staton
# bas0041@auburn.edu
# Updated 3/10/2015
#
# @description This function allows you to pull out single variables from a mcmc.list or matrix and plot if desired
# 
# @param post.samp: an object of class 'mcmc.list' or 'matrix'
# @param var: the variable you wish to view. Must be in "quotes"
# @param do.post: logical. Return the whole posterior along with summary? Default is FALSE
# @param do.plot: logical. Plot the posterior and trace plots? Default is FALSE
# @examples get.post(post, var="N[", do.plot=T)
# @examples get.post(post, var="lnalpha")

# Note: This function assumes there are two chains.  if there are more (or fewer), 
# it will treat it as though there are two chains.  

get.post=function(post.samp, var, do.post=F, do.plot=F){
  require(coda)
  
  #coerce to matrix if mcmc.list
  if (!is.mcmc.list(post.samp) & !is.matrix(post.samp)) stop("post.samp is not of class mcmc.list or matrix")
  if (is.mcmc.list(post.samp)) post.samp=as.matrix(post.samp)
  
  #pull out posteriors for requested variable
  if (substr(var,nchar(var), nchar(var))=="[") post=post.samp[,substr(colnames(post.samp), 1, nchar(var))==var]
  else post=post.samp[,var]
  
  #if it has subscripts, apply
  if (is.matrix(post)) {
    post.est=apply(post, 2, function(x) c(mean=mean(x), sd=sd(x), quantile(x, c(0.5, 0.025, 0.975))))
    if (do.plot==T ){
      n.iter=dim(post)[1]/2
      #windows(record=T)
      par(mfrow=c(4,2), mar=c(2, 2, 1.5, 1.5), oma=c(1,1,1,1))
      
      chain1=post[1:n.iter,]
      chain2=post[(n.iter+1):dim(post)[1],]
      
      for (i in 1:dim(post)[2]){
        plot(density(chain1[,i]), type="l", col="Blue", xlim=c(min(chain1[,i], chain2[,i]), max(chain1[,i], chain2[,i])),
             xlab=" ", ylab="Density", main=paste("Posterior of ", colnames(post)[i], sep=""))
        
        lines(density(chain2[,i]), col="Red")
        
        plot(chain1[,i], type="l", col="Blue", ylim=c(min(chain1[,i], chain2[,i]), max(chain1[,i], chain2[,i])),
             xlab="Iteration", ylab=" ", main=paste("Trace of ", colnames(post)[i], sep=""))
        lines(chain2[,i], col="Red")
      }
    }
  }
    
  if (is.vector(post)) {
    post.est=c(mean=mean(post), sd=sd(post), quantile(post, c(0.5, 0.025, 0.975)))
    
    if (do.plot==T){
      n.iter=length(post)/2
      chain1=post[1:n.iter]
      chain2=post[(n.iter+1):length(post)]
      
      #windows(width=12, height=8)
      par(mfrow=c(1,2))
      
      plot(density(chain1), col="Blue", xlab="", xlim=c(min(chain1, chain2), max(chain1, chain2)), main=paste("Density of ", var, sep=""))
      lines(density(chain2), col="Red")
      
      plot(chain1, type="l", col="Blue",xlab="Iteration", ylab="", ylim=c(min(chain1, chain2), max(chain1, chain2)), main=paste("Trace of ", var, sep=""))
      lines(chain2, col="Red")
    }
  }
  
  if (do.post==T) list(posterior=post, summary=post.est)
  else post.est
}

#can use this to loop over various variables/parameters
#can use this to loop over various models too!: eval(as.name(models[i]))

