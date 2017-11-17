#  Plot example effects for AMPPS equivalence paper

####  Setup ----------------------------------------

library(purrr)
library(gridExtra)
library(ggplot2)
library(lattice)

library("TOSTER")

plot.scaler <- 1.8
nhst.size <- 0.5
tost.size <- 1
point.size <- 1.5

####  Base plot ----------------------------------------

baseplot <- ggplot(data.frame()) +
  scale_x_continuous(breaks=NULL) +
  coord_flip() + # flip coordinates (puts labels on y axis)
  theme_classic(base_size = 10) + # use a white background
  geom_hline(yintercept=0, lty=1) +
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size=rel(0.7), lineheight = 0.5))

####  Meta analysis ----------------------------------------

source("Example3_Hyde-et-al.R")

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
  geom_pointrange(size = nhst.size/2, fatten = point.size/5) + 
  geom_pointrange(aes(ymin=li90, ymax=ui90), size = tost.size/2, fatten = point.size/5) +
  geom_hline(yintercept=-sesoi, lty=2) +
  geom_hline(yintercept=sesoi, lty=2) +
  geom_hline(yintercept=0, lty=1) +
  coord_flip() +  # flip coordinates (puts labels on y axis)
  labs(title = "C: Example 3 (Hyde et al.)", y = "Effect (Z)") +
  ylim(c(-sesoi*plot.scaler, sesoi*plot.scaler)) +
  theme_classic(base_size = 10) + # use a white background
  theme(plot.title = element_text(size = rel(1), face = "bold"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size=rel(0.7), lineheight = 0.5))


####  Brandt ----------------------------------------

source("Example2_Brandt-et-al_study1.R")

brandtplot = baseplot +
  annotate(geom = "pointrange", x = 0.5, y = Brandt$diff, ymin = Brandt$LL_CI_TTEST, ymax = Brandt$UL_CI_TTEST, size = nhst.size, fatten = point.size) +
  annotate(geom = "pointrange", x = 0.5, y = Brandt$diff, ymin = Brandt$LL_CI_TOST, ymax = Brandt$UL_CI_TOST, size = tost.size, fatten = point.size) +
  geom_hline(yintercept=Brandt$low_eqbound, lty=2) +
  geom_hline(yintercept=Brandt$high_eqbound, lty=2) +
  labs(title = "B: Example 2 (Brandt et al.)", y = "Effect (mean difference)") +
  ylim(c(Brandt$low_eqbound*plot.scaler, Brandt$high_eqbound*plot.scaler))

#### Moon ----------------------------------------

source("Example1_MoonRoeder.R")

moonplot = baseplot +
  annotate(geom = "pointrange", x = 0.5, y = Moon$diff, ymin = Moon$LL_CI_TTEST, ymax = Moon$UL_CI_TTEST, size = nhst.size, fatten = point.size) +
  annotate(geom = "pointrange", x = 0.5, y = Moon$diff, ymin = Moon$LL_CI_TOST, ymax = Moon$UL_CI_TOST, size = tost.size, fatten = point.size) +
  geom_hline(yintercept=Moon$low_eqbound, lty=2) +
  geom_hline(yintercept=Moon$high_eqbound, lty=2) +
  labs(title = "A: Example 1 (Moon & Roeder)", y = "Effect (mean difference)") +
  ylim(c(Moon$low_eqbound*plot.scaler, Moon$high_eqbound*plot.scaler))

#### Lynott ----------------------------------------



source("Example4_Lynott-et-al.R")

lynottplot = baseplot +
  annotate(geom = "pointrange", x = 0.5, y = Lynott$dif, ymin = Lynott$LL_CI_ZTEST, ymax = Lynott$UL_CI_ZTEST, size = nhst.size, fatten = point.size) +
  annotate(geom = "pointrange", x = 0.5, y = Lynott$dif, ymin = Lynott$LL_CI_TOST, ymax = Lynott$UL_CI_TOST, size = tost.size, fatten = point.size) +
  geom_hline(yintercept=Lynott$high_eqbound, lty=2) +
  ylim(c(Lynott$low_eqbound*plot.scaler, Lynott$high_eqbound*plot.scaler)) +
  labs(title = "D: Example 4 (Lynott et al.)", y = "Effect (proportion difference)")

#### Kahane ----------------------------------------

source("Example5_Kahane-et-al-study4.R")

kahaneplot = baseplot +
  annotate(geom = "pointrange", x = 0.5, y = Kahane$r, ymin = Kahane$LL_CI_TTEST, ymax = Kahane$UL_CI_TTEST, size = nhst.size, fatten = point.size) +
  annotate(geom = "pointrange", x = 0.5, y = Kahane$r, ymin = Kahane$LL_CI_TOST, ymax = Kahane$UL_CI_TOST, size = tost.size, fatten = point.size) +
  geom_hline(yintercept=Kahane$low_eqbound_r, lty=2) +
  geom_hline(yintercept=Kahane$high_eqbound_r, lty=2) +
  ylim(c(Kahane$low_eqbound*plot.scaler, Kahane$high_eqbound*plot.scaler)) +
  labs(title = "E: Example 5 (Kahane et al.)", y = "Effect (pearson correlation)")

####  Place plots in grid ----------------------------------------

lay <- rbind(c(1,2,5),
             c(3,4,5)) # Define the figure layout. Think of the c()s as a grid/ Plot 1-5 will be alloted the relative space in the grid.

#tiff(file="example_grid.tiff",width=2000,height=1400, units = "px", res = 300)
example.grid <- grid.arrange(moonplot, brandtplot, lynottplot, kahaneplot, metaplot, layout_matrix = lay)
#dev.off()