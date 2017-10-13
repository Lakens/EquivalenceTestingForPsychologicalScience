###==========================================================================###
################ Daniel: TOSTtwo + BFs from standardised effects ###############
###==========================================================================###

#' TOST function for an independent t-test (Cohen's d)
#' @param m1 mean of group 1
#' @param m2 mean of group 2
#' @param sd1 standard deviation of group 1
#' @param sd2 standard deviation of group 2
#' @param n1 sample size in group 1
#' @param n2 sample size in group 2
#' @param low_eqbound_d lower equivalence bounds (e.g., -0.5) expressed in standardized mean difference (Cohen's d)
#' @param high_eqbound_d upper equivalence bounds (e.g., 0.5) expressed in standardized mean difference (Cohen's d)
#' @param alpha alpha level (default = 0.05)
#' @param var.equal logical variable indicating whether equal variances assumption is assumed to be TRUE or FALSE.  Defaults to FALSE.
#' @return Returns TOST t-value 1, TOST p-value 1, TOST t-value 2, TOST p-value 2, degrees of freedom, low equivalence bound, high equivalence bound, low equivalence bound in Cohen's d, high equivalence bound in Cohen's d, Lower limit confidence interval TOST, Upper limit confidence interval TOST
#' @importFrom stats pnorm pt qnorm qt
#' @importFrom graphics abline plot points segments title
#' @examples
#' ## Eskine (2013) showed that participants who had been exposed to organic
#' ## food were substantially harsher in their moral judgments relative to
#' ## those exposed to control (d = 0.81, 95% CI: [0.19, 1.45]). A
#' ## replication by Moery & Calin-Jageman (2016, Study 2) did not observe
#' ## a significant effect (Control: n = 95, M = 5.25, SD = 0.95, Organic
#' ## Food: n = 89, M = 5.22, SD = 0.83). Following Simonsohn's (2015)
#' ## recommendation the equivalence bound was set to the effect size the
#' ## original study had 33% power to detect (with n = 21 in each condition,
#' ## this means the equivalence bound is d = 0.48, which equals a
#' ## difference of 0.384 on a 7-point scale given the sample sizes and a
#' ## pooled standard deviation of 0.894). Using a TOST equivalence test
#' ## with default alpha = 0.05, not assuming equal variances, and equivalence
#' ## bounds of d = -0.43 and d = 0.43 is significant, t(182) = -2.69,
#' ## p = 0.004. We can reject effects larger than d = 0.43.
#'
#' TOSTtwo(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound_d=-0.43,high_eqbound_d=0.43)
#' @section References:
#' Berger, R. L., & Hsu, J. C. (1996). Bioequivalence Trials, Intersection-Union Tests and Equivalence Confidence Sets. Statistical Science, 11(4), 283-302.
#'
#' Gruman, J. A., Cribbie, R. A., & Arpin-Cribbie, C. A. (2007). The effects of heteroscedasticity on tests of equivalence. Journal of Modern Applied Statistical Methods, 6(1), 133-140, formula for Welch's t-test on page 135
#' @export
#'

TOSTtwo<-function(m1,m2,sd1,sd2,n1,n2,low_eqbound_d, high_eqbound_d, alpha, var.equal, prior_dist, effect_prior, se_prior, df_prior){
  if(missing(alpha)) {
    alpha<-0.05
  }
  if(missing(var.equal)) {
    var.equal<-FALSE
  }
  if(var.equal==TRUE) {
    sdpooled<-sqrt((((n1 - 1)*(sd1^2)) + (n2 - 1)*(sd2^2))/((n1+n2)-2)) #calculate sd pooled
    low_eqbound<-low_eqbound_d*sdpooled
    high_eqbound<-high_eqbound_d*sdpooled
    degree_f<-n1+n2-2
    t1<-((m1-m2)-low_eqbound)/(sdpooled*sqrt(1/n1 + 1/n2))  #students t-test lower bound
    p1<-pt(t1, degree_f, lower.tail=FALSE)
    t2<-((m1-m2)-high_eqbound)/(sdpooled*sqrt(1/n1 + 1/n2)) #students t-test upper bound
    p2<-pt(t2, degree_f, lower.tail=TRUE)
    t<-(m1-m2)/(sdpooled*sqrt(1/n1 + 1/n2))
    pttest<-2*pt(-abs(t), df=degree_f)
    LL90<-(m1-m2)-qt(1-alpha, n1+n2-2)*(sdpooled*sqrt(1/n1 + 1/n2))
    UL90<-(m1-m2)+qt(1-alpha, n1+n2-2)*(sdpooled*sqrt(1/n1 + 1/n2))
    LL95<-(m1-m2)-qt(1-(alpha/2), n1+n2-2)*(sdpooled*sqrt(1/n1 + 1/n2))
    UL95<-(m1-m2)+qt(1-(alpha/2), n1+n2-2)*(sdpooled*sqrt(1/n1 + 1/n2))
  } else {
    sdpooled<-sqrt((sd1^2 + sd2^2)/2) #calculate sd root mean squared for Welch's t-test
    low_eqbound<-low_eqbound_d*sdpooled
    high_eqbound<-high_eqbound_d*sdpooled
    degree_f<-(sd1^2/n1+sd2^2/n2)^2/(((sd1^2/n1)^2/(n1-1))+((sd2^2/n2)^2/(n2-1))) #degrees of freedom for Welch's t-test
    t1<-((m1-m2)-low_eqbound)/sqrt(sd1^2/n1 + sd2^2/n2) #welch's t-test upper bound
    p1<-pt(t1, degree_f, lower.tail=FALSE) #p-value for Welch's TOST t-test
    t2<-((m1-m2)-high_eqbound)/sqrt(sd1^2/n1 + sd2^2/n2) #welch's t-test lower bound
    p2<-pt(t2, degree_f, lower.tail=TRUE) #p-value for Welch's TOST t-test
    t<-(m1-m2)/sqrt(sd1^2/n1 + sd2^2/n2) #welch's t-test NHST
    pttest<-2*pt(-abs(t), df=degree_f) #p-value for Welch's t-test
    LL90<-(m1-m2)-qt(1-alpha, degree_f)*sqrt(sd1^2/n1 + sd2^2/n2) #Lower limit for CI Welch's t-test
    UL90<-(m1-m2)+qt(1-alpha, degree_f)*sqrt(sd1^2/n1 + sd2^2/n2) #Upper limit for CI Welch's t-test
    LL95<-(m1-m2)-qt(1-(alpha/2), degree_f)*sqrt(sd1^2/n1 + sd2^2/n2) #Lower limit for CI Welch's t-test
    UL95<-(m1-m2)+qt(1-(alpha/2), degree_f)*sqrt(sd1^2/n1 + sd2^2/n2) #Upper limit for CI Welch's t-test
  }
  ptost<-max(p1,p2) #Get highest p-value for summary TOST result
  ttost<-ifelse(abs(t1) < abs(t2), t1, t2) #Get lowest t-value for summary TOST result
  dif<-(m1-m2)
  testoutcome<-ifelse(pttest<alpha,"significant","non-significant")
  TOSToutcome<-ifelse(ptost<alpha,"significant","non-significant")
  plot(NA, ylim=c(0,1), xlim=c(min(LL90,low_eqbound)-max(UL90-LL90, high_eqbound-low_eqbound)/10, max(UL90,high_eqbound)+max(UL90-LL90, high_eqbound-low_eqbound)/10), bty="l", yaxt="n", ylab="",xlab="Mean Difference")
  points(x=dif, y=0.5, pch=15, cex=2)
  abline(v=high_eqbound, lty=2)
  abline(v=low_eqbound, lty=2)
  abline(v=0, lty=2, col="grey")
  segments(LL90,0.5,UL90,0.5, lwd=3)
  segments(LL95,0.5,UL95,0.5, lwd=1)
  title(main=paste("Equivalence bounds ",round(low_eqbound,digits=3)," and ",round(high_eqbound,digits=3),"\nMean difference = ",round(dif,digits=3)," \n TOST: ", 100*(1-alpha*2),"% CI [",round(LL90,digits=3),";",round(UL90,digits=3),"] ", TOSToutcome," \n NHST: ", 100*(1-alpha),"% CI [",round(LL95,digits=3),";",round(UL95,digits=3),"] ", testoutcome,sep=""), cex.main=1)
  if(var.equal == TRUE) {
    message(cat("Using alpha = ",alpha," Student's t-test was ",testoutcome,", t(",degree_f,") = ",t,", p = ",pttest,sep=""))
    cat("\n")
    message(cat("Using alpha = ",alpha," the equivalence test based on Student's t-test was ",TOSToutcome,", t(",degree_f,") = ",ttost,", p = ",ptost,sep=""))
  } else {
    message(cat("Using alpha = ",alpha," Welch's t-test was ",testoutcome,", t(",degree_f,") = ",t,", p = ",pttest,sep=""))
    cat("\n")
    message(cat("Using alpha = ",alpha," the equivalence test based on Welch's t-test  was ",TOSToutcome,", t(",degree_f,") = ",ttost,", p = ",ptost,sep=""))
  }
  TOSTresults<-data.frame(t1,p1,t2,p2,degree_f)
  colnames(TOSTresults) <- c("t-value 1","p-value 1","t-value 2","p-value 2","df")
  bound_d_results<-data.frame(low_eqbound_d,high_eqbound_d)
  colnames(bound_d_results) <- c("low bound d","high bound d")
  bound_results<-data.frame(low_eqbound,high_eqbound)
  colnames(bound_results) <- c("low bound raw","high bound raw")
  CIresults<-data.frame(LL90,UL90)
  colnames(CIresults) <- c(paste("Lower Limit ",100*(1-alpha*2),"% CI raw",sep=""),paste("Upper Limit ",100*(1-alpha*2),"% CI raw",sep=""))
  cat("TOST results:\n")
  print(TOSTresults)
  cat("\n")
  cat("Equivalence bounds (Cohen's d):\n")
  print(bound_d_results)
  cat("\n")
  cat("Equivalence bounds (raw scores):\n")
  print(bound_results)
  cat("\n")
  cat("TOST confidence interval:\n")
  print(CIresults)
  #below added BF calc
  bayes<-TRUE #expect to povide bayes
  if(missing(effect_prior)) {
    bayes<-FALSE #if no prior effect size is provided, BF not calculated
  }
  if(bayes==TRUE) {
    if(prior_dist=="normal") {
      if(missing(se_prior)) {
        se_prior<-effect_prior/2 #if not specified otherwise, default SE is effect/2
      }
    }
    if(prior_dist=="halfnormal") {
      if(missing(se_prior)) {
        se_prior<-effect_prior #if not specified otherwise, default SE is effect
        effect_prior<-0 #halfnormal is centered on 0
      }
    }
  }
  if(missing(df_prior)) {
    df_prior<-100000 #if not specified otherwise, default df = 100000 (practically normal)
  }
  theta <- effect_prior - 10 * se_prior
  incr <- se_prior / 200
  theta=seq(from = effect_prior - 10 * se_prior, by = incr, length = 4001)
  dist_theta <- dt(x = (theta-effect_prior)/se_prior, df=df_prior)
  if(prior_dist=="halfnormal"){
    dist_theta[theta <= 0] = 0
  }
  dist_theta_alt = dist_theta/sum(dist_theta)
  likelihood <- dt((abs(dif)-theta)/(abs(dif)/abs(t)), df = degree_f) #use abs(dif) - can be set to d
  likelihood_alt = likelihood/sum(likelihood)
  height <- dist_theta * likelihood
  area <- sum(height * incr)
  normarea <- sum(dist_theta * incr)
  height_alt = dist_theta_alt * likelihood_alt
  height_alt = height_alt/sum(height_alt)
  LikelihoodTheory <- area/normarea
  LikelihoodNull <- dt(abs(dif)/(abs(dif)/abs(t)), df = degree_f)
  BayesFactor <- round(LikelihoodTheory / LikelihoodNull, 2)
  bayes_results <- data.frame(BayesFactor, LikelihoodTheory, LikelihoodNull)
  colnames(bayes_results) <- c("Bayes Factor","Likelihood (alternative)","Likelihood (null)")
  cat("Bayes Results:\n")
  print(bayes_results)
  cat("\n")
  invisible(list(TOST_t1=t1,TOST_p1=p1,TOST_t2=t2,TOST_p2=p2, TOST_df=degree_f,alpha=alpha,low_eqbound=low_eqbound,high_eqbound=high_eqbound,low_eqbound_d=low_eqbound_d,high_eqbound_d=high_eqbound_d, LL_CI_TOST=LL90,UL_CI_TOST=UL90,bf=BayesFactor, ll_theory=LikelihoodTheory, ll_null=LikelihoodNull))
}

TOSTtwo(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound_d=-0.43,high_eqbound_d=0.43, var.equal=TRUE, prior_dist = "halfnormal", effect_prior = 0.5)
TOSTtwo(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound_d=-0.43,high_eqbound_d=0.43, var.equal=TRUE, prior_dist = "normal", effect_prior = 0.5)

###==========================================================================###
############### Neil McLatchie: TOSTtwo + BFs from *raw* effects ###############
###==========================================================================###

TOSTtwo.raw.nm<-function(m1,m2,sd1,sd2,n1,n2,low_eqbound, high_eqbound, alpha, var.equal, prior_dist, effect_prior, se_prior, df_prior)
{
  if(missing(alpha)) {
    alpha<-0.05
  }
  if(missing(var.equal)) {
    var.equal<-FALSE
  }
  if(var.equal==TRUE) {
    sdpooled<-sqrt((((n1 - 1)*(sd1^2)) + (n2 - 1)*(sd2^2))/((n1+n2)-2)) #calculate sd pooled
    t1<-((m1-m2)-low_eqbound)/(sdpooled*sqrt(1/n1 + 1/n2))
    degree_f<-n1+n2-2
    p1<-pt(t1, degree_f, lower.tail=FALSE)
    t2<-((m1-m2)-high_eqbound)/(sdpooled*sqrt(1/n1 + 1/n2))
    p2<-pt(t2, degree_f, lower.tail=TRUE)
    LL90<-(m1-m2)-qt(1-alpha, degree_f)*(sdpooled*sqrt(1/n1 + 1/n2))
    UL90<-(m1-m2)+qt(1-alpha, degree_f)*(sdpooled*sqrt(1/n1 + 1/n2))
    LL95<-(m1-m2)-qt(1-(alpha/2), degree_f)*(sdpooled*sqrt(1/n1 + 1/n2))
    UL95<-(m1-m2)+qt(1-(alpha/2), degree_f)*(sdpooled*sqrt(1/n1 + 1/n2))
    t<-(m1-m2)/(sdpooled*sqrt(1/n1 + 1/n2))
    pttest<-2*pt(-abs(t), df=degree_f)
  } else {
    sdpooled<-sqrt((sd1^2 + sd2^2)/2) #calculate sd root mean squared for Welch's t-test
    t1<-((m1-m2)-low_eqbound)/sqrt(sd1^2/n1 + sd2^2/n2) #welch's t-test lower bound
    degree_f<-(sd1^2/n1+sd2^2/n2)^2/(((sd1^2/n1)^2/(n1-1))+((sd2^2/n2)^2/(n2-1))) #degrees of freedom for Welch's t-test
    p1<-pt(t1, degree_f, lower.tail=FALSE) #p-value for Welch's TOST t-test
    t2<-((m1-m2)-high_eqbound)/sqrt(sd1^2/n1 + sd2^2/n2) #welch's t-test upper bound
    p2<-pt(t2, degree_f, lower.tail=TRUE) #p-value for Welch's TOST t-test
    t<-(m1-m2)/sqrt(sd1^2/n1 + sd2^2/n2) #welch's t-test NHST
    pttest<-2*pt(-abs(t), df=degree_f) #p-value for Welch's t-test
    LL90<-(m1-m2)-qt(1-alpha, degree_f)*sqrt(sd1^2/n1 + sd2^2/n2) #Lower limit for CI Welch's t-test
    UL90<-(m1-m2)+qt(1-alpha, degree_f)*sqrt(sd1^2/n1 + sd2^2/n2) #Upper limit for CI Welch's t-test
    LL95<-(m1-m2)-qt(1-(alpha/2), degree_f)*sqrt(sd1^2/n1 + sd2^2/n2) #Lower limit for CI Welch's t-test
    UL95<-(m1-m2)+qt(1-(alpha/2), degree_f)*sqrt(sd1^2/n1 + sd2^2/n2) #Upper limit for CI Welch's t-test
  }
  ptost<-max(p1,p2) #Get highest p-value for summary TOST result
  ttost<-ifelse(abs(t1) < abs(t2), t1, t2) #Get lowest t-value for summary TOST result
  dif<-(m1-m2)
  testoutcome<-ifelse(pttest<alpha,"significant","non-significant")
  TOSToutcome<-ifelse(ptost<alpha,"significant","non-significant")
  plot(NA, ylim=c(0,1), xlim=c(min(LL90,low_eqbound)-max(UL90-LL90, high_eqbound-low_eqbound)/10, max(UL90,high_eqbound)+max(UL90-LL90, high_eqbound-low_eqbound)/10), bty="l", yaxt="n", ylab="",xlab="Mean Difference")
  points(x=dif, y=0.5, pch=15, cex=2)
  abline(v=high_eqbound, lty=2)
  abline(v=low_eqbound, lty=2)
  abline(v=0, lty=2, col="grey")
  segments(LL90,0.5,UL90,0.5, lwd=3)
  segments(LL95,0.5,UL95,0.5, lwd=1)
  title(main=paste("Equivalence bounds ",round(low_eqbound,digits=3)," and ",round(high_eqbound,digits=3),"\nMean difference = ",round(dif,digits=3)," \n TOST: ", 100*(1-alpha*2),"% CI [",round(LL90,digits=3),";",round(UL90,digits=3),"] ", TOSToutcome," \n NHST: ", 100*(1-alpha),"% CI [",round(LL95,digits=3),";",round(UL95,digits=3),"] ", testoutcome,sep=""), cex.main=1)
  if(var.equal == TRUE) {
    message(cat("Using alpha = ",alpha," Student's t-test was ",testoutcome,", t(",degree_f,") = ",t,", p = ",pttest,sep=""))
    cat("\n")
    message(cat("Using alpha = ",alpha," the equivalence test based on Student's t-test was ",TOSToutcome,", t(",degree_f,") = ",ttost,", p = ",ptost,sep=""))
  } else {
    message(cat("Using alpha = ",alpha," Welch's t-test was ",testoutcome,", t(",degree_f,") = ",t,", p = ",pttest,sep=""))
    cat("\n")
    message(cat("Using alpha = ",alpha," the equivalence test based on Welch's t-test  was ",TOSToutcome,", t(",degree_f,") = ",ttost,", p = ",ptost,sep=""))
  }
  TOSTresults<-data.frame(t1,p1,t2,p2,degree_f)
  colnames(TOSTresults) <- c("t-value 1","p-value 1","t-value 2","p-value 2","df")
  bound_results<-data.frame(low_eqbound,high_eqbound)
  colnames(bound_results) <- c("low bound raw","high bound raw")
  CIresults<-data.frame(LL90,UL90)
  colnames(CIresults) <- c(paste("Lower Limit ",100*(1-alpha*2),"% CI raw",sep=""),paste("Upper Limit ",100*(1-alpha*2),"% CI raw",sep=""))
  cat("TOST results:\n")
  print(TOSTresults)
  cat("\n")
  cat("Equivalence bounds (raw scores):\n")
  print(bound_results)
  cat("\n")
  cat("TOST confidence interval:\n")
  print(CIresults)
  #below added BF calc
  bayes<-TRUE #expect to provide bayes
  if(missing(effect_prior)) {
    bayes<-FALSE #if no prior effect size is provided, BF not calculated
  }
  if(bayes==TRUE) {
    if(prior_dist=="normal") {
      if(missing(se_prior)) {
        se_prior<-effect_prior/2 #if not specified otherwise, default SE is effect/2
      }
    }
    if(prior_dist=="halfnormal") {
      if(missing(se_prior)) {
        se_prior<-effect_prior #if not specified otherwise, default SE is effect
        effect_prior<-0 #halfnormal is centered on 0
      } }
    if(prior_dist=="cauchy") {
      df_prior<-1
      {
        if(missing(se_prior)) 
        {df_prior<-1
        se_prior<-effect_prior/2} #if not specified otherwise, default SE is effect
      }}
    if(prior_dist=="halfcauchy") {
      {df_prior<-1}
      if(missing(se_prior)) {
        df_prior<-1
        se_prior<-effect_prior #if not specified otherwise, default SE is effect
        effect_prior<-0 #halfcauchy is centered on 0
      }}
    
    if(missing(df_prior)) {
      df_prior<-1000 #if not specified otherwise, default df = 100000 (practically normal)
    }
    theta <- effect_prior - 10 * se_prior
    incr <- se_prior / 200
    theta=seq(from = effect_prior - 10 * se_prior, by = incr, length = 4001)
    dist_theta <- dt(x = (theta-effect_prior)/se_prior, df=df_prior)
    if(prior_dist=="halfnormal"){
      dist_theta[theta <= 0] = 0
    }
    if(prior_dist=="halfcauchy"){
      dist_theta[theta <= 0] = 0
    }
    dist_theta_alt = dist_theta/sum(dist_theta)
    likelihood <- dt((abs(dif)-theta)/(abs(dif)/abs(t)), df = degree_f) #use abs(dif) - can be set to d
    likelihood_alt = likelihood/sum(likelihood)
    height <- dist_theta * likelihood
    area <- sum(height * incr)
    normarea <- sum(dist_theta * incr)
    height_alt = dist_theta_alt * likelihood_alt
    height_alt = height_alt/sum(height_alt)
    LikelihoodTheory <- area/normarea
    LikelihoodNull <- dt(abs(dif)/(abs(dif)/abs(t)), df = degree_f)
    BayesFactor <- round(LikelihoodTheory / LikelihoodNull, 2)
    bayes_results <- data.frame(BayesFactor, LikelihoodTheory, LikelihoodNull)
    colnames(bayes_results) <- c("Bayes Factor","Likelihood (alternative)","Likelihood (null)")
    cat("Bayes Results:\n")
    print(bayes_results)
    cat("\n")
    invisible(list(TOST_t1=t1,TOST_p1=p1,TOST_t2=t2,TOST_p2=p2, TOST_df=degree_f,alpha=alpha,low_eqbound=low_eqbound,high_eqbound=high_eqbound,low_eqbound=low_eqbound,high_eqbound=high_eqbound, LL_CI_TOST=LL90,UL_CI_TOST=UL90,bf=BayesFactor, ll_theory=LikelihoodTheory, ll_null=LikelihoodNull))
    
  }}

###==========================================================================###

library(pwr)

###==========================================================================###
###==========================================================================###
###==========================================================================###


### Transfer examples from Dienes & McLatchie (2017)

## example 5: Johnson et al.
# original study: Schnall, Benton, & Harvey (2008)
orig.m1 <- 4.62
orig.sd1 <- 1.53
orig.m2 <- 5.73
orig.sd2 <- 1.28
orig.n1 <- 21
orig.n2 <- 22
orig.raw <- abs(orig.m2-orig.m1)
orig.sdpooled <- sqrt(((orig.n1-1)*orig.sd1^2 + (orig.n2-1)*orig.sd2^2)/(orig.n1+orig.n2-2))
orig.se <- sqrt((orig.sd1^2/orig.n1)+(orig.sd2^2/orig.n2)) #standard error of *differences*
orig.df <- orig.n1+orig.n2-2

# replication study: Johnson, Cheung, & Donnellan (2014)
rep.m1 <- 5.97
rep.sd1 <- 1.34
rep.m2 <- 6.12
rep.sd2 <- 1.36
rep.d <- -0.11
rep.n1 <- 58
rep.n2 <- 68
rep.raw <- abs(rep.m1-rep.m2)
rep.sdpooled <- sqrt(((rep.n1-1)*rep.sd1^2 + (rep.n2-1)*rep.sd2^2)/(rep.n1+rep.n2-2))
rep.se <- sqrt((rep.sd1^2/rep.n1)+(rep.sd2^2/rep.n2)) #standard error of *differences*
rep.df <- rep.n1 + rep.n2 -2

# Calculate critical d: smallest d orig. study had the power to detect
t.crit = qt(1-.05/2, (orig.n1+orig.n2)-2)
d.crit = t.crit*sqrt((1/orig.n1) + 1/(orig.n2))
raw.crit <- d.crit * orig.sdpooled # smallest raw effect orig. study could detect

# Small telescopes: Calculate d for which orig. study had 33% power
power <- pwr.t2n.test(n1 = orig.n1, n2 = orig.n2, d = NULL, sig.level = 0.05, power = 0.33, alternative = "two.sided")
d.33 <- power$d
raw.33 <- d.33 * orig.sdpooled # raw effect for which orig. had 33% power


#### TOST & BF (re-)calculations 1: raw effects, using Neil's TOSTtwo.raw.nm

##Johnson.1.1.1## TOST: small telescopes; BF: halfnormal
# Dienes: BF = 0.37 --> successfully replicated
Johnson.1.1.1 <- TOSTtwo.raw.nm(m1=rep.m1, 
                                m2=rep.m2, 
                                sd1=rep.sd1, 
                                sd2 = rep.sd2, 
                                n1 = rep.n1, 
                                n2=rep.n2, 
                                low_eqbound = -raw.33, 
                                high_eqbound = raw.33, 
                                var.equal = TRUE, 
                                prior_dist = "halfnormal", 
                                effect_prior = orig.raw)

##Johnson.1.1.2## TOST: small telescopes; BF: normal (meaning a t-distribution given df from original study)
# Dienes: BF = 0.09 --> successfully replicated
Johnson.1.1.2 <- TOSTtwo.raw.nm(m1=rep.m1, 
                                m2=rep.m2, 
                                sd1=rep.sd1, 
                                sd2 = rep.sd2, 
                                n1 = rep.n1, 
                                n2=rep.n2, 
                                low_eqbound = -raw.33, 
                                high_eqbound = raw.33, 
                                var.equal = TRUE, 
                                prior_dist = "normal", 
                                effect_prior = orig.raw, 
                                se_prior = orig.se, 
                                df_prior = orig.df)


##Johnson.1.2.1## TOST: critical effect; BF: halfnormal
# Dienes: BF = 0.37 --> successfully replicated
Johnson.1.2.1 <- TOSTtwo.raw.nm(m1=rep.m1, 
                                m2=rep.m2, 
                                sd1=rep.sd1, 
                                sd2 = rep.sd2, 
                                n1 = rep.n1, 
                                n2=rep.n2,
                                low_eqbound = -raw.crit, 
                                high_eqbound = raw.crit, 
                                var.equal = TRUE, 
                                prior_dist = "halfnormal", 
                                effect_prior = orig.raw)

##Johnson.1.2.2## TOST: critical effect; BF: normal (meaning a t-distribution given df from original study)
## Dienes: BF = 0.09 --> successfully replicated
Johnson.1.2.2 <- TOSTtwo.raw.nm(m1=rep.m1, 
                                m2=rep.m2, 
                                sd1=rep.sd1, 
                                sd2 = rep.sd2, 
                                n1 = rep.n1, 
                                n2=rep.n2,
                                low_eqbound = -raw.crit, 
                                high_eqbound = raw.crit, 
                                var.equal = TRUE, 
                                prior_dist = "normal", 
                                effect_prior = orig.raw,
                                se_prior = orig.se, 
                                df_prior = orig.df)


#### TOST & BF (re-)calculations 2: standardised effects, using Daniel's TOSTtwo

##Johnson.2.1## TOST: small telescopes; BF: halfnormal
# Dienes: BF = 0.37 --> successfully replicated
Johnson.2.1 <- TOSTtwo(m1=rep.m1, 
                       m2=rep.m2, 
                       sd1=rep.sd1, 
                       sd2 = rep.sd2, 
                       n1 = rep.n1, 
                       n2=rep.n2, 
                       low_eqbound_d = -d.33, 
                       high_eqbound_d = d.33, 
                       var.equal = TRUE, 
                       prior_dist = "halfnormal", 
                       effect_prior = orig.raw)


##Johnson.2.2## TOST: critical effect; BF: halfnormal
# Dienes: BF = 0.37 --> successfully replicated
Johnson.2.2 <- TOSTtwo(m1=rep.m1, 
                       m2=rep.m2, 
                       sd1=rep.sd1, 
                       sd2 = rep.sd2, 
                       n1 = rep.n1, 
                       n2=rep.n2,
                       low_eqbound = -d.crit, 
                       high_eqbound = d.crit, 
                       var.equal = TRUE, 
                       prior_dist = "halfnormal", 
                       effect_prior = orig.raw)

