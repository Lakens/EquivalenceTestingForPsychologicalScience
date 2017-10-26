#  Plot example effects for AMPPS equivalence paper

####  Setup ----------------------------------------

library(purrr)
library(gridExtra)
library(ggplot2)

library("TOSTER")
download.file(url = "https://raw.githubusercontent.com/Lakens/TOSTER/master/R/TOSTtwo.prop.R", destfile = "TOSTtwo.prop.R")
source(file = "TOSTtwo.prop.R")

plot.scaler = 2

####  Base plot ----------------------------------------

baseplot <- ggplot(data.frame()) +
  scale_x_continuous(breaks=NULL) +
  coord_flip() + # flip coordinates (puts labels on y axis)
  theme_classic() + # use a white background
  geom_hline(yintercept=0, lty=1) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

####  Meta analysis ----------------------------------------

gr2 <- TOSTmeta(ES = 0.06, se = 0.003, low_eqbound_d=-0.1, high_eqbound_d=0.1, alpha=0.005, plot = FALSE)
gr3 <- TOSTmeta(ES = 0.04, se = 0.002, low_eqbound_d=-0.1, high_eqbound_d=0.1, alpha=0.005, plot = FALSE)
gr4 <- TOSTmeta(ES = -0.01, se = 0.002, low_eqbound_d=-0.1, high_eqbound_d=0.1, alpha=0.005, plot = FALSE)
gr5 <- TOSTmeta(ES = -0.01, se = 0.002, low_eqbound_d=-0.1, high_eqbound_d=0.1, alpha=0.005, plot = FALSE)
gr6 <- TOSTmeta(ES = -0.01, se = 0.002, low_eqbound_d=-0.1, high_eqbound_d=0.1, alpha=0.005, plot = FALSE)
gr7 <- TOSTmeta(ES = -0.02, se = 0.002, low_eqbound_d=-0.1, high_eqbound_d=0.1, alpha=0.005, plot = FALSE)
gr8 <- TOSTmeta(ES = -0.02, se = 0.002, low_eqbound_d=-0.1, high_eqbound_d=0.1, alpha=0.005, plot = FALSE)
gr9 <- TOSTmeta(ES = -0.01, se = 0.003, low_eqbound_d=-0.1, high_eqbound_d=0.1, alpha=0.005, plot = FALSE)
gr10 <- TOSTmeta(ES = 0.04, se = 0.003, low_eqbound_d=-0.1, high_eqbound_d=0.1, alpha=0.005, plot = FALSE)
gr11 <- TOSTmeta(ES = 0.06, se = 0.003, low_eqbound_d=-0.1, high_eqbound_d=0.1, alpha=0.005, plot = FALSE)

grades <- list(gr2, gr3, gr4, gr5, gr6, gr7, gr8, gr9, gr10, gr11)

es <- map_dbl(grades, 1)
l_eqb <- map_dbl(grades, 7)
u_eqb <- map_dbl(grades, 8)
li90 <- map_dbl(grades, 9)
ui90 <- map_dbl(grades, 10)
li95 <- map_dbl(grades, 11)
ui95 <- map_dbl(grades, 12)
label <- as.factor(paste0("grade ", 2:11))

df <- data.frame(es, li90, ui90, label)

sesoi <- 0.1


metaplot <- ggplot(data=df, aes(x=label, y=es, ymin=li95, ymax=ui95)) +
  scale_x_discrete(breaks=NULL) +
  geom_pointrange(size = 0.6) + 
  geom_pointrange(aes(ymin=li90, ymax=ui90), size = 1) +
  geom_hline(yintercept=-sesoi, lty=2) +
  geom_hline(yintercept=sesoi, lty=2) +
  geom_hline(yintercept=0, lty=1) +
  coord_flip() +  # flip coordinates (puts labels on y axis)
  labs(title = "B - Example 2 (Hyde)", y = "Effect (Z)") +
  ylim(c(-sesoi*plot.scaler, sesoi*plot.scaler)) +
  theme_classic() + # use a white background
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) # use a white background


####  Brandt ----------------------------------------

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

# Calculate critical effect (d): smallest effect orig. study had the power to detect
t.crit = qt(1-.05/2, (orig.N-2))
d.crit = t.crit*sqrt((1/(orig.N/2)) + 1/(orig.N/2))

Brandt <- TOSTtwo(m1 = rep.m1, m2 = rep.m2, sd1 = rep.sd1, sd2 = rep.sd2, n1 = rep.n1, n2 = rep.n2, low_eqbound_d = -d.crit, high_eqbound_d = d.crit, var.equal = FALSE)

df <- NULL

ggplot(df) + 
  annotate(geom = "pointrange", x = 1, y = 1, ymin = -2, ymax = 2) +
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Label") + ylab("Mean (95% CI)") +
  theme_bw()  # use a white background

brandtplot = baseplot +
  annotate(geom = "pointrange", x = 0.5, y = Brandt$diff, ymin = Brandt$LL_CI_TTEST, ymax = Brandt$UL_CI_TTEST, size = 1) +
  annotate(geom = "pointrange", x = 0.5, y = Brandt$diff, ymin = Brandt$LL_CI_TOST, ymax = Brandt$UL_CI_TOST, size = 2) +
  geom_hline(yintercept=Brandt$low_eqbound, lty=2) +
  geom_hline(yintercept=Brandt$high_eqbound, lty=2) +
  labs(title = "A - Example 1 (Brandt)", y = "Effect (mean difference)") +
  ylim(c(Brandt$low_eqbound*plot.scaler, Brandt$high_eqbound*plot.scaler))

#### Moon ----------------------------------------

#  We first load the relevant datafile into R
moon.data    <- read.csv("NSNEQ_Moon_Example_data.csv") 

#  We then calculate the summary statistics from the data that we need in the TOST calculation below
mean.asian   <- mean(moon.data$accuracy[moon.data$identity_salience == 1])
mean.control <- mean(moon.data$accuracy[moon.data$identity_salience == 2])
sd.asian     <- sd(moon.data$accuracy[moon.data$identity_salience == 1])
sd.control   <- sd(moon.data$accuracy[moon.data$identity_salience == 2])
n.asian      <- sum(moon.data$identity_salience == 1)
n.control    <- sum(moon.data$identity_salience == 2)

#  We can now run the TOST!
#  Performing the TOST using the 'TOSTtwo.raw' function allows us to specify equivalence bounds in raw values
Moon <- TOSTtwo.raw(m1  = mean.asian,
                            m2  = mean.control,
                            sd1 = sd.asian,
                            sd2 = sd.control,
                            n1  = n.asian,
                            n2  = n.control,
                            low_eqbound  = -0.0625,
                            high_eqbound =  0.0625,
                            alpha = .05)

moonplot = baseplot +
  annotate(geom = "pointrange", x = 0.5, y = Moon$diff, ymin = Moon$LL_CI_TTEST, ymax = Moon$UL_CI_TTEST, size = 1) +
  annotate(geom = "pointrange", x = 0.5, y = Moon$diff, ymin = Moon$LL_CI_TOST, ymax = Moon$UL_CI_TOST, size = 2) +
  geom_hline(yintercept=Moon$low_eqbound, lty=2) +
  geom_hline(yintercept=Moon$high_eqbound, lty=2) +
  labs(title = "C - Example 3 (Moon)", y = "Effect (mean difference)") +
  ylim(c(Moon$low_eqbound*plot.scaler, Moon$high_eqbound*plot.scaler))

#### Lynott ----------------------------------------



#  We first load the data...
lynott.data <- read.csv("NSEQ_Lynott_example_data.csv")
#  ... and only include cases that did not meet the authors' exclusion criteria. 
lynott.data <- lynott.data[lynott.data$include1 == 1,]  

#  We then calculate the summary statistics from the data that we need in the TOST calculation below.
n.hot        <- sum(lynott.data$packtype == 1)
n.cold       <- sum(lynott.data$packtype == 0)
selfish.hot  <- sum(lynott.data$selfish[lynott.data$packtype == 1] == 1, na.rm = TRUE) / n.hot
selfish.cold <- sum(lynott.data$selfish[lynott.data$packtype == 0] == 1, na.rm = TRUE) / n.cold

#  Then we set the equivalence bounds (based on critical difference of original study)
se     <- sqrt((0.75 * (1 - 0.75)) / 26 + (0.46 * (1 - 0.46)) / 27)  # Calculate the (unpooled) standard error
crit.z <- qnorm(1 - (0.05 / 2))  # Determine the critical z for a two-tailed test with alpha = 0.05

crit.diff <- crit.z * se  # Convert z to the critical difference

#  We can now run the TOST!
Lynott <- TOSTtwo.prop(prop1 = selfish.cold, 
                     prop2 = selfish.hot, 
                     n1    = n.cold, 
                     n2    = n.hot, 
                     low_eqbound  = -crit.diff, 
                     high_eqbound = crit.diff, 
                     alpha = 0.05)

lynottplot = baseplot +
  annotate(geom = "pointrange", x = 0.5, y = Lynott$dif, ymin = Lynott$LL_CI_ZTEST, ymax = Lynott$UL_CI_ZTEST, size = 1) +
  annotate(geom = "pointrange", x = 0.5, y = Lynott$dif, ymin = Lynott$LL_CI_TOST, ymax = Lynott$UL_CI_TOST, size = 2) +
  geom_hline(yintercept=Lynott$low_eqbound, lty=2) +
  geom_hline(yintercept=Lynott$high_eqbound, lty=2) +
  ylim(c(Lynott$low_eqbound*plot.scaler, Lynott$high_eqbound*plot.scaler)) +
  labs(title = "D - Example 4 (Lynott)", y = "Effect (proportion difference)")


####  Place plots in grid ----------------------------------------

grid.arrange(brandtplot, metaplot, moonplot, lynottplot, ncol = 2)
