library(TOSTER)
library(pwr)

## Kahane, Everett, Earp, Farias, & Savulescu (2015)
##
## Study 4, 5.2.1. ii., p. 203: "There  was  no  relationship  between  perceived wrongness in 
## personal dilemmas and in the ‘greater good’ dilemmas (r = -.04, p = .53): that is, people
## who were more ‘utilitarian’ in the personal dilemmas were not more likely to be more truly
## utilitarian in the other dilemmas, and vice versa. This lack of relationship held even when
## controlling for primary psychopathy in a partial second order correlation (r = -.07, p = .32)."

Kahane.data <- read.csv("Example5_Kahane-et-al-study4_data-cleaned.csv", header = TRUE, sep = ",")
Kahane.data <- as.data.frame(Kahane.data)

# sample size
Kahane.N <- min(sum(!is.na(Kahane.data$ClassicSacrificialDilemmas)), sum(!is.na(Kahane.data$GreaterGoodDilemmas)))

# correlation b/w perceived wrongness in personal dilemmas and 'greater good' dilemmas
Kahane.r.GG <- cor.test(Kahane.data$ClassicSacrificialDilemmas, Kahane.data$GreaterGoodDilemmas, method = "pearson")$estimate
Kahane.p <- cor.test(Kahane.data$ClassicSacrificialDilemmas, Kahane.data$GreaterGoodDilemmas, method = "pearson")$p.value

# correlation the study had 80% power to detect
Kahane.r.80 <- (pwr.r.test(n = Kahane.N, r = NULL, sig.level = 0.05, power = 0.8, alternative = "two.sided"))$r

# TOST with bounds set to the correlation the study had 80% power to detect
Kahane <- TOSTr(n = Kahane.N, r = Kahane.r.GG, low_eqbound_r = -Kahane.r.80, high_eqbound_r = Kahane.r.80, alpha = 0.05)