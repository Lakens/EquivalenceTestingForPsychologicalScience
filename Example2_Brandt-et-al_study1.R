library(TOSTER)
library(pwr)

Brandt.data <- read.csv("Example2_Brandt-et-al_study1_data-cleaned.csv", header = TRUE, sep = ",")
Brandt.data <- as.data.frame(Brandt.data)

# original study: Banerjee et al., study 1
orig.m1 <- 4.71
orig.m2 <- 5.3
orig.sd1 <- 0.85
orig.sd2 <- 0.97
orig.d <- 0.65
orig.t <- 2.03
orig.p <- 0.049
orig.N <- 40 #group size unknown, therefore equal n assumed
orig.df <- orig.N-2

# replication study: Brandt et al., study 1
rep.m1 <- mean(Brandt.data$WellLitSca[Brandt.data$ExpCond==-1])
rep.m2 <- mean(Brandt.data$WellLitSca[Brandt.data$ExpCond==1])
rep.sd1 <- sd(Brandt.data$WellLitSca[Brandt.data$ExpCond==-1])
rep.sd2 <- sd(Brandt.data$WellLitSca[Brandt.data$ExpCond==1])
rep.n1 <- sum(Brandt.data$ExpCond==-1)
rep.n2 <- sum(Brandt.data$ExpCond==1)
rep.test <- t.test(Brandt.data$WellLitSca[Brandt.data$ExpCond==-1],Brandt.data$WellLitSca[Brandt.data$ExpCond==1], var.equal=FALSE, paired=FALSE)
rep.t <- rep.test$statistic
rep.p <- rep.test$p.value
rep.df <- rep.test$parameter
rep.d <- (rep.m1 - rep.m2)/sqrt(((rep.n1-1)*rep.sd1^2 + (rep.n2-1)*rep.sd2^2)/(rep.n1+rep.n2-2))


# Calculate small telescopes effect (d) effect orig. study had 33% power to detect
d.33 <- (pwr.t.test(n = orig.N/2, d = NULL, sig.level = 0.05, power = 0.33, type = "two.sample", alternative = "two.sided"))$d

Brandt <- TOSTtwo(m1=rep.m1, m2=rep.m2, sd1=rep.sd1, sd2 = rep.sd2, n1 = rep.n1, n2=rep.n2, low_eqbound_d = -d.33, high_eqbound_d = d.33, var.equal = FALSE)