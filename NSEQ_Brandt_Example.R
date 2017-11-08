library(TOSTER)
library(pwr)

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
rep.m1 <- 4.7857
rep.m2 <- 4.6569
rep.sd1 <- 1.0897
rep.sd2 <- 1.1895
rep.n1 <- 49
rep.n2 <- 51
rep.t <- 0.56
rep.p <- .574
rep.df <- rep.n1 + rep.n2 - 2
rep.d <- 0.11


# Calculate small telescopes effect (d) effect orig. study had 33% power to detect
d.33 <- (pwr.t.test(n = orig.N/2, d = NULL, sig.level = 0.05, power = 0.33, type = "two.sample", alternative = "two.sided"))$d

Brandt <- TOSTtwo(m1=rep.m1, m2=rep.m2, sd1=rep.sd1, sd2 = rep.sd2, n1 = rep.n1, n2=rep.n2, low_eqbound_d = -d.33, high_eqbound_d = d.33, var.equal = FALSE)