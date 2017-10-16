###==========================================================================###
library(TOSTER)
library(pwr)
###==========================================================================###


## Dienes & McLatchie (2017) example 3: grounded cognition 
## (Brandt et al. replicating Banerjee et al.)


####################################################
# Study 1
####################################################

# original study: Banerjee et al., study 1
orig.m1 <- 4.71
orig.m2 <- 5.3
orig.sd1 <- 0.85
orig.sd2 <- 0.97
orig.d <- 0.65
orig.t <- 2.03
orig.N <- 40
orig.n1 <- orig.N/2 #group size unknown, therefore equal n assumed
orig.n2 <- orig.n1 #group size unknown, therefore equal n assumed
orig.raw <- abs(orig.m2-orig.m1)
orig.sdpooled <- sqrt((orig.sd1^2 + orig.sd2^2)/(orig.N-2))
orig.se <- sqrt((orig.sd1^2/orig.n1)+(orig.sd2^2/orig.n2)) #standard error of *differences*
orig.df <- orig.N-2

# replication study: Brandt et al., study 1
rep.m1 <- 4.79
rep.m2 <- 4.66
rep.sd1 <- 1.09
rep.sd2 <- 1.19
rep.d <- -0.11
rep.n1 <- 50 # this is a guess, the paper only reports total N = 120
rep.n2 <- 50 # see previous comment
rep.N <- 120
rep.raw <- abs(rep.m2-rep.m1)
rep.sdpooled <- sqrt((rep.sd1^2 + rep.sd2^2)/(rep.N-2))
rep.se <- sqrt((rep.sd1^2/rep.n1)+(rep.sd2^2/rep.n2)) #standard error of *differences*
rep.df <- rep.N-2

# Calculate critical effect (d and raw): smallest effect orig. study had the power to detect
t.crit = qt(1-.05/2, (orig.N-2))
d.crit = t.crit*sqrt((1/(orig.N/2)) + 1/(orig.N/2))
raw.crit <- d.crit * orig.sdpooled # smallest raw effect orig. study could detect

# Small telescopes: Calculate d for which orig. study had 33% power
power <- pwr.t.test(n = orig.N/2, d = NULL, sig.level = 0.05, power = 0.33, type = "two.sample", alternative = "two.sided")
d.33 <- power$d
raw.33 <- d.33 * orig.sdpooled # raw effect for which orig. had 33% power

####################################################
#### Study 1, raw effects

##Brandt.1.1.1: Study 1, standardised effects, small telescopes
Brandt.1.1.1 <- TOSTtwo(m1=rep.m1, 
                      m2=rep.m2, 
                      sd1=rep.sd1, 
                      sd2 = rep.sd2, 
                      n1 = rep.n1, 
                      n2=rep.n2, 
                      low_eqbound_d = -d.33, 
                      high_eqbound_d = d.33, 
                      var.equal = TRUE)

##Brandt.1.1.2: Study 1, standardised effects, critical effect
Brandt.1.1.2 <- TOSTtwo(m1=rep.m1, 
                      m2=rep.m2, 
                      sd1=rep.sd1, 
                      sd2 = rep.sd2, 
                      n1 = rep.n1, 
                      n2=rep.n2,
                      low_eqbound = -d.crit, 
                      high_eqbound = d.crit, 
                      var.equal = TRUE)


#### Study 1, raw effects

##Brandt.1.2.1: Study 1, raw effects, small telescopes
Brandt.1.2.1 <- TOSTtwo.raw(m1=rep.m1, 
                            m2=rep.m2, 
                            sd1=rep.sd1, 
                            sd2 = rep.sd2, 
                            n1 = rep.n1, 
                            n2=rep.n2, 
                            low_eqbound = -raw.33, 
                            high_eqbound = raw.33, 
                            var.equal = TRUE)


##Brandt.1.2.2: Study 1, raw effects, critical effect
Brandt.1.2.2 <- TOSTtwo.raw(m1=rep.m1, 
                            m2=rep.m2, 
                            sd1=rep.sd1, 
                            sd2 = rep.sd2, 
                            n1 = rep.n1, 
                            n2=rep.n2,
                            low_eqbound = -raw.crit, 
                            high_eqbound = raw.crit, 
                            var.equal = TRUE)


####################################################
# study 2
####################################################
# original study: Banerjee et al., study 2
orig.m1 <- 87.6
orig.m2 <- 74.3
orig.sd1 <- 7.40
orig.sd2 <- 26.85
orig.d <- 0.64
orig.t <- 2.7
orig.N <- 74
orig.n1 <- orig.N/2 #group size unknown, therefore equal n assumed
orig.n2 <- orig.n1 #group size unknown, therefore equal n assumed
orig.raw <- abs(orig.m2-orig.m1)
orig.sdpooled <- sqrt((orig.sd1^2 + orig.sd2^2)/(orig.N-2))
orig.se <- sqrt((orig.sd1^2/orig.n1)+(orig.sd2^2/orig.n2)) #standard error of *differences*
orig.df <- orig.N-2

# replication study: Brandt et al., study 2
rep.m1 <- 135.91
rep.m2 <- 130.44
rep.sd1 <- 192.64
rep.sd2 <- 152.03
rep.d <- .03
rep.n1 <- 60 # this is a guess, the paper only reports total N = 121
rep.n2 <- 61 # see previous comment
rep.N <- 121
rep.raw <- abs(rep.m2-rep.m1)
rep.sdpooled <- sqrt((rep.sd1^2 + rep.sd2^2)/(rep.N-2))
rep.se <- sqrt((rep.sd1^2/rep.n1)+(rep.sd2^2/rep.n2)) #standard error of *differences*
rep.df <- rep.N-2


# Calculate critical effect (d and raw): smallest effect orig. study had the power to detect
t.crit = qt(1-.05/2, (orig.N-2))
d.crit = t.crit*sqrt((1/(orig.N/2)) + 1/(orig.N/2))
raw.crit <- d.crit * orig.sdpooled # smallest raw effect orig. study could detect

# Small telescopes: Calculate d for which orig. study had 33% power
power <- pwr.t.test(n = orig.N/2, d = NULL, sig.level = 0.05, power = 0.33, type = "two.sample", alternative = "two.sided")
d.33 <- power$d
raw.33 <- d.33 * orig.sdpooled # raw effect for which orig. had 33% power

####################################################
#### Study 2, standardised effects (d)

##Brandt.2.1.1: Study 2, standardised effects, small telescopes
Brandt.2.1.1 <- TOSTtwo(m1=rep.m1, 
                        m2=rep.m2, 
                        sd1=rep.sd1, 
                        sd2 = rep.sd2, 
                        n1 = rep.n1, 
                        n2=rep.n2, 
                        low_eqbound_d = -d.33, 
                        high_eqbound_d = d.33, 
                        var.equal = FALSE)

##Brandt.2.1.2: Study 2, standardised effects, critical effect
Brandt.2.1.2 <- TOSTtwo(m1=rep.m1, 
                        m2=rep.m2, 
                        sd1=rep.sd1, 
                        sd2 = rep.sd2, 
                        n1 = rep.n1, 
                        n2=rep.n2,
                        low_eqbound = -d.crit, 
                        high_eqbound = d.crit, 
                        var.equal = FALSE)


#### Study 2, raw effects

##Brandt.2.2.1: Study 2, raw effects, small telescopes
Brandt.2.2.1 <- TOSTtwo.raw(m1=rep.m1, 
                            m2=rep.m2, 
                            sd1=rep.sd1, 
                            sd2 = rep.sd2, 
                            n1 = rep.n1, 
                            n2=rep.n2, 
                            low_eqbound = -raw.33, 
                            high_eqbound = raw.33, 
                            var.equal = FALSE)

##Brandt.2.2.2: Study 2, raw effects, critical effect
Brandt.2.2.2 <- TOSTtwo.raw(m1=rep.m1, 
                            m2=rep.m2, 
                            sd1=rep.sd1, 
                            sd2 = rep.sd2, 
                            n1 = rep.n1, 
                            n2=rep.n2, 
                            low_eqbound = -raw.crit, 
                            high_eqbound = raw.crit, 
                            var.equal = FALSE)


###==========================================================================###
############################# TOST + Bayes factors #############################
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

####################################################
# study 2, Bayes factors
####################################################

##Brandt.1.2.1: Study 2, critical effect; BF: halfnormal
# Dienes: BF = 0.97 --> successfully replicated
Brandt.1.2.1 <- TOSTtwo.raw.nm(m1=rep.m1, 
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

##Brandt.1.2.2: Study 2, critical effect; BF: normal (meaning a t-distribution given df from original study)
## Dienes: BF = 0.97 --> successfully replicated
Brandt.1.2.2 <- TOSTtwo.raw.nm(m1=rep.m1, 
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
