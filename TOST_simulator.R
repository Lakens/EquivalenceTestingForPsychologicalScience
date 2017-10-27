
####  Setup ----------------------------------------

library(TOSTER)
library(purrr)
source("../../TOSTER/R/TOSTtwo.R") #  Source modified TOSTtwo from pederisager Fork in order to get necessary values from TOST
library(ggplot2)

sims <- list()


# Group 1 parameters
mean1 <- 0    # The mean effect
std1  <- 10   # The standard deviation
n1    <- 10   # The number of subjects in each group

# Group 2 parameters
mean2 <- 0    # The mean effect
std2  <- 10   # The standard deviation
n2    <- 10   # The number of subjects in each group

# TOST
alpha <- 0.05
tails <- 2 # 1 = one-tailed, 2 = two-tailed


# Set simulation parameters

nsims <- 10000  # Number of simulations
bound.criterion <- "tost.current.power"  # Choose between "crit.val", "benchmark", "d33", "current.power", "tost.current.power"

#  When using citerion "crit.val", the critical t value based on the parameters of the current sample are used as bounds
crit.t <- qt(p = 1-(alpha/tails), df = n1+n2-2) # Calculate critical t

#  When using criterion "benchmark", bounds = benhmark
benchmark <- 0.5

#  When using criterion "d33", small telescops is applied, group n of previous study is random 10-100, and bounds are set to effect that previous study had tlscop.pwr power to detect. 
tlscop.pwr <- 0.33

#  When using criterion "current.power", effect that current study had  pwr.thrsh power to detect are used as bounds
pwr.thrsh <- 0.80
crnt.pwr  <- pwr::pwr.t2n.test(n1 = n1,
                               n2 = n2, 
                               power = pwr.thrsh, 
                               sig.level = alpha, 
                               alternative = "two.sided")$d # Calculate effect size that original study have 33% power to detect

# When using criterion "tost.current.power", effect is set to bounds that current study has 80% power for
tost.pwr <- 0.80
crnt.tost.pwr <- powerTOSTtwo(alpha = alpha, statistical_power = tost.pwr, N = n1)[2]



####  Run simulation  ----------------------------------------

for (sim in 1:nsims) {
  control <- rnorm(n = n1, mean = mean1, sd = std1)
  exp.group <- rnorm(n = n2, mean = mean2, sd = std2)
  
  m1 = mean(control)
  sd1 = sd(control)
  
  m2 = mean(exp.group)
  sd2 = sd(exp.group)
  
  if (bound.criterion == "crit.val") {  # Run TOST with bounds based on critical t of CURRENT study
    
    sims[[sim]] <- TOSTtwo(m1 = m1, 
                           m2 = m2, 
                           sd1 = sd1, 
                           sd2 = sd2, 
                           n1 = n1, 
                           n2 = n2, 
                           low_eqbound_d = -crit.t, 
                           high_eqbound_d = crit.t, 
                           alpha = alpha, 
                           var.equal = ifelse(test = std1 == std2, yes = TRUE, no = FALSE), #  sets equal variance assumption based on whether the standard deviation in the populations match.
                           side.effects = FALSE)
    
  } else if (bound.criterion == "benchmark") {  # Run TOST with benchmark value
    
    sims[[sim]] <- TOSTtwo(m1 = m1, 
                           m2 = m2, 
                           sd1 = sd1, 
                           sd2 = sd2, 
                           n1 = n1, 
                           n2 = n2, 
                           low_eqbound_d = -benchmark, 
                           high_eqbound_d = benchmark, 
                           alpha = alpha, 
                           var.equal = ifelse(test = std1 == std2, yes = TRUE, no = FALSE), #  sets equal variance assumption based on whether the standard deviation in the populations match.
                           side.effects = FALSE)
    
  } else if (bound.criterion == "d33") {
    
    d33  <- pwr::pwr.t2n.test(n1 = runif(1, 10, 100),  # Assume group n of previous study is random within n=10-100
                              n2 = runif(1, 10, 100), 
                              power = tlscop.pwr, 
                              sig.level = alpha, 
                              alternative = "two.sided")$d # Calculate effect size that original study have 33% power to detect
    
    sims[[sim]] <- TOSTtwo(m1  = m1, 
                           m2  = m2, 
                           sd1 = sd1, 
                           sd2 = sd2, 
                           n1  = n1, 
                           n2  = n2, 
                           low_eqbound_d  = -d33, 
                           high_eqbound_d = d33, 
                           alpha = alpha, 
                           var.equal    = ifelse(test = std1 == std2, yes = TRUE, no = FALSE), #  sets equal variance assumption based on whether the standard deviation in the populations match.
                           side.effects = FALSE)
  } else if (bound.criterion == "current.power") {
    
    
    sims[[sim]] <- TOSTtwo(m1  = m1, 
                           m2  = m2, 
                           sd1 = sd1, 
                           sd2 = sd2, 
                           n1  = n1, 
                           n2  = n2, 
                           low_eqbound_d  = -crnt.pwr, 
                           high_eqbound_d = crnt.pwr, 
                           alpha = alpha, 
                           var.equal    = ifelse(test = std1 == std2, yes = TRUE, no = FALSE), #  sets equal variance assumption based on whether the standard deviation in the populations match.
                           side.effects = FALSE)
    
  } else if (bound.criterion == "tost.current.power") {
    
    sims[[sim]] <- TOSTtwo(m1  = m1, 
                           m2  = m2, 
                           sd1 = sd1, 
                           sd2 = sd2, 
                           n1  = n1, 
                           n2  = n2, 
                           low_eqbound_d  = -crnt.tost.pwr, 
                           high_eqbound_d = crnt.tost.pwr, 
                           alpha = alpha, 
                           var.equal    = ifelse(test = std1 == std2, yes = TRUE, no = FALSE), #  sets equal variance assumption based on whether the standard deviation in the populations match.
                           side.effects = FALSE)
  }
}

pTOST <- map_dbl(sims, "TOST_p")
pNHST <- map_dbl(sims, "NHST_p") 
diff  <- map_dbl(sims, "diff")

plot(pNHST, pTOST)
abline(v=0.05, lty=2)
abline(h = 0.05, lty=2)


#  Plot subset of sims in forestplot 
# es <- diff
# l_eqb <- map_dbl(sims, "low_eqbound")
# u_eqb <- map_dbl(sims, "high_eqbound")
# li90 <- map_dbl(sims, "LL_CI_TOST")
# ui90 <- map_dbl(sims, "UL_CI_TOST")
# li95 <- map_dbl(sims, "LL_CI_TTEST")
# ui95 <- map_dbl(sims, "UL_CI_TTEST")
# label <- as.factor(paste0("study ", 1:length(sims)))
# 
# df <- data.frame(es, li90, ui90, label, li95, ui95)
# 
# sesoi <- 0.1
# 
# forestplot <- ggplot(data=df[900:1000,], aes(x=label, y=es, ymin=li95, ymax=ui95)) +
#   scale_x_discrete(breaks=NULL) +
#   geom_pointrange(size = 0.6) + 
#   geom_pointrange(aes(ymin=li90, ymax=ui90), size = 1) +
#   geom_hline(yintercept=-sesoi, lty=2) +
#   geom_hline(yintercept=sesoi, lty=2) +
#   geom_hline(yintercept=0, lty=1) +
#   coord_flip() +  # flip coordinates (puts labels on y axis)
#   labs(title = "B - Example 2 (Hyde)", y = "Mean diff") +
#   theme_classic() + # use a white background
#   theme(axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank()) # use a white background

