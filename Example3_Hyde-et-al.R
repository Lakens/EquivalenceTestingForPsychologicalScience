# Hyde, Lindberg, Linn, Ellis, and Williams (2008) meta analysis


# Load TOSTER
library(TOSTER)

# Set smallest effect size of interest and alpha
sesoi <- 0.1
alpha <- 0.005

# Run equivalence test on each grade group
gr2  <- TOSTmeta(ES = 0.06, se = 0.003, low_eqbound_d=-sesoi, high_eqbound_d=sesoi, alpha=alpha, plot = FALSE)
gr3  <- TOSTmeta(ES = 0.04, se = 0.002, low_eqbound_d=-sesoi, high_eqbound_d=sesoi, alpha=alpha, plot = FALSE)
gr4 <- TOSTmeta(ES = -0.01, se = 0.002, low_eqbound_d=-sesoi, high_eqbound_d=sesoi, alpha=alpha, plot = FALSE)
gr5 <- TOSTmeta(ES = -0.01, se = 0.002, low_eqbound_d=-sesoi, high_eqbound_d=sesoi, alpha=alpha, plot = FALSE)
gr6 <- TOSTmeta(ES = -0.01, se = 0.002, low_eqbound_d=-sesoi, high_eqbound_d=sesoi, alpha=alpha, plot = FALSE)
gr7 <- TOSTmeta(ES = -0.02, se = 0.002, low_eqbound_d=-sesoi, high_eqbound_d=sesoi, alpha=alpha, plot = FALSE)
gr8 <- TOSTmeta(ES = -0.02, se = 0.002, low_eqbound_d=-sesoi, high_eqbound_d=sesoi, alpha=alpha, plot = FALSE)
gr9 <- TOSTmeta(ES = -0.01, se = 0.003, low_eqbound_d=-sesoi, high_eqbound_d=sesoi, alpha=alpha, plot = FALSE)
gr10 <- TOSTmeta(ES = 0.04, se = 0.003, low_eqbound_d=-sesoi, high_eqbound_d=sesoi, alpha=alpha, plot = FALSE)
gr11 <- TOSTmeta(ES = 0.06, se = 0.003, low_eqbound_d=-sesoi, high_eqbound_d=sesoi, alpha=alpha, plot = FALSE)


gr3.z <- (gr3$ES/0.002)  # Calculate Z value for grade 3
gr3.p <- 2*pnorm(-abs(gr3.z))  # Calculate p value for grade 3
