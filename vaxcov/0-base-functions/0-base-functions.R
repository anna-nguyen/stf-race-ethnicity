mean_se=function (Y, id=NULL, print = TRUE) {
  if(!is.null(id)){
    mudat <- data.frame(id = id, Y = Y)
    n.orig <- dim(mudat)[1]
    mudat <- mudat[complete.cases(mudat), ]
    n.sub <- dim(mudat)[1]
    if (n.orig > n.sub) 
      cat("\n-----------------------------------------\nDropping", 
          n.orig - n.sub, "observations\ndue to missing values in the outcome\n", 
          "Final sample size:", n.sub, "\n-----------------------------------------\n")
    fit <- glm(Y ~ 1, family = gaussian, data = mudat)
    vcovCL <- sandwichSE(fm = fit, cluster = mudat$id)
    rfit <- coeftest(fit, vcovCL)
    lb <- rfit[1, 1] - 1.96 * rfit[1, 2]
    ub <- rfit[1, 1] + 1.96 * rfit[1, 2]
    pval <- round(rfit[1, 4], 3)
    mean_ci <- matrix(c(n.sub, rfit[1, 1], sd(mudat$Y), rfit[1,2], 
                        lb, ub, pval), nrow = 1, ncol = 7)
  }else{
    mudat <- data.frame(Y = Y)
    n.sub <- length(Y)
    fit <- glm(Y ~ 1, family = gaussian, data = mudat)
    lb <- summary(fit)$coef[1, 1] - 1.96 * summary(fit)$coef[1, 2]
    ub <- summary(fit)$coef[1, 1] + 1.96 * summary(fit)$coef[1, 2]
    pval <- round(summary(fit)$coef[1,4], 3)
    mean_ci <- matrix(c(n.sub, summary(fit)$coef[1, 1], sd(mudat$Y), summary(fit)$coef[1,2], 
                        lb, ub, pval), nrow = 1, ncol = 7)
  }

  colnames(mean_ci) <- c("N", "Mean", "SD", "Robust SE", "Lower 95%CI", 
                         "Upper 95%CI", "pvalue")
  if (print == TRUE) 
    print(mean_ci)
  return(mean_ci)
}

sandwichSE=function (fm, cluster) 
{
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj <- apply(estfun(fm), 2, function(x) tapply(x, cluster, 
                                                sum))
  vcovCL <- dfc * sandwich(fm, meat = crossprod(uj)/N)
  return(vcovCL)
}

# get predicted means for each district 
getpred=function(fit,vcov){
  
  py1=fit$coef[1]+fit$coef[2]
  py0=fit$coef[1]
  
  names(py1)=NULL
  names(py0)=NULL

  # se of linear combination of b0 and b1 = 
  # b0^2*s0^2 + b1^2*s1^2 + 2*b1*b0*cov(b1,b0)
  # based on the delta method
  se.1=sqrt(vcov[1,1]+vcov[2,2]+2*vcov[1,2])
  se.0=sqrt(vcov[1,1])
  
  lb1=py1-(qnorm(0.975)*se.1)
  ub1=py1+(qnorm(0.975)*se.1)
  
  lb0=py0-(qnorm(0.975)*se.0)
  ub0=py0+(qnorm(0.975)*se.0)
  
  out=as.data.frame(rbind(c(py1,lb1,ub1),c(py0,lb0,ub0)))
  colnames(out)=c("mean","lb","ub")
  out$dist=c("WCCUSD", "OUSD")
  
  return(out)
}

#----------------------------------------------------
# Format GLM  results
# inputs: 
# rfit = fit from GLM model
# output: matrix with formatted results
#----------------------------------------------------
format.glm=function(rfit,family){
  pt.est=rfit[2,1]
  se=rfit[2,2]
  lb=pt.est-(qnorm(0.975)*se)
  ub=pt.est+(qnorm(0.975)*se)
  pval=rfit[2,4]
  if(family=="binomial"){
    out=exp(cbind(pt.est,lb,ub,pval))
  }
  else{
    out=cbind(pt.est,lb,ub,pval)
  }
  return(out)
}

#----------------------------------------------------
# Generate Wilson CI
#----------------------------------------------------

calculate_wilson_ci = function(p, se, z = qnorm(0.975)){
  n_eff = (p * (1 - p)) / (se**2)
  numerator_term1 = p + (z**2/(2 * n_eff))
  numerator_term2 = z * sqrt(((p * (1 - p)) / n_eff) 
                             + (z/(2 * n_eff))**2)
  denom = 1 + (z**2/n_eff)
  
  lb = (numerator_term1 - numerator_term2) / denom
  ub = (numerator_term1 + numerator_term2) / denom
  return(c(lb, ub))
}
