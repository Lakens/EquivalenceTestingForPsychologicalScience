# TOST on the values from Gibson, Losee, and Vitello example

#### Data ####

Gibson.m1 <- 0.63
Gibson.m2 <- 0.51
Gibson.sd1 <- 0.22
Gibson.sd2 <- 0.23
Gibson.n1 <- 40
Gibson.n2 <- 43

Shih.m1 <- 0.54
Shih.n2 <- 0.43
Shih.sd1 <- 0.17
Shih.sd2 <- 0.16
Shih.n1 <- 16
Shih.n2 <- 14
Shih.t <- 2.02

#### Calculate subjective bound critera from original study data ####

# Original had 33% Power to detect effect of d=0.57 (G*Power result)
t33 <- 0.57

# Original study smallest detectable effect size

crit_t = qt(1-.05/2, 29) # Critical t for original study
crit_d = crit_t*sqrt(1/16 + 1/14) # Convert critical t to d

# Original study d
Shih.d <- Shih.t*sqrt(1/16 + 1/14)

#### TOST for the different bounds ####

# TOST for 33% Power rule:
TOSTtwo(m1 = Gibson.m1, 
        m2 = Gibson.m2, 
        sd1 = Gibson.sd1, 
        sd2 = Gibson.sd2, 
        n1 = Gibson.n1, 
        n2 = Gibson.n2, 
        low_eqbound_d = -t33, 
        high_eqbound_d = t33, alpha = .05)

# TOST for smallest detectable effect in original study
TOSTtwo(m1 = Gibson.m1, 
        m2 = Gibson.m2, 
        sd1 = Gibson.sd1, 
        sd2 = Gibson.sd2, 
        n1 = Gibson.n1, 
        n2 = Gibson.n2, 
        low_eqbound_d = -crit_d, 
        high_eqbound_d = crit_d, 
        alpha = .05)

# TOST for Dienes recommendaton of bounds of zero and twice the original effect size
TOSTtwo(m1 = Gibson.m1, m2 = Gibson.m2, sd1 = Gibson.sd1, sd2 = Gibson.sd2, n1 = Gibson.n1, n2 = Gibson.n2, low_eqbound_d = 0, high_eqbound_d = Shih.d*2, alpha = .05)
