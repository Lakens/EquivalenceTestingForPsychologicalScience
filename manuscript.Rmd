---
title             : "Equivalence Testing for Psychological Science"
shorttitle        : "Equivalence Testing"

author: 
  - name          : "Daniel Lakens"
    affiliation   : "1"
    corresponding : yes    # Define only one corresponding author
    address       : "Den Dolech 1, IPO 1.33, 5600 MB, Eindhoven, The Netherlands"
    email         : "D.Lakens@tue.nl"
  - name          : "Anne M. Scheel"
    affiliation   : "1"
  - name          : "Peder Isager"
    affiliation   : "1"

affiliation:
  - id            : "1"
    institution   : "Eindhoven University of Technology"

author_note: >
  We would like to thank Courtney Soderbergh for creating the first version of the TOST function to test two independent proportions.

abstract: >
  Psychologists  need to be able to test for the absence of an effect. Using the Two-One-Sided Tests (TOST) procedure, researchers can easily test whether the observed effects are too small to be meaningful. By specifying a smallest effect size of interest (SESOI) researchers test whether observed effects are surprisingly closer to zero, assuming there was an effect the consider meaningful. We explain a range of approaches to determine the SESOI in psychological science, and provide detailed examples of how equivalence tests should be performed and reported. 
  
keywords          : "Equivalence Testing, NHST, power, TOST"
wordcount         : "X"

bibliography      : ["equivalence.bib"]

figsintext        : no
figurelist        : no
tablelist         : no
footnotelist      : no
lineno            : yes

lang              : "english"
class             : "man"
output            : papaja::apa6_pdf
---

```{r include = FALSE}
library("papaja")

# papaja:::fetch_zotero_refs(
#   x = "1786197"
#   , bib_name = "equivalence.bib"
#   , API_key = "F7JGNtCxZGT6q87Eeg2zNWiP"
#   , lib_type = "group"
# )
```

Psychologists should be able to falsify predictions. A common prediction in psychological research is that an effect exists in the population that differs from zero. For example, we might predict that priming American Asian women with their Asian identify will increase their performence on a math test compared to women in the control condition who are primed with their female identity. To be able to falsify this hypothesies, and design a study that allows for strong inferences [@platt_strong_1964], it is important to specify which test result would *disprove* the hypothesis.

An equivalence test can be used to test whether the observed effect is surprisingly small, assuming a meaningful effect exists in the population. The test is a simple variation on the widely used null hypothesis significance tests. To understand the idea behind equivalence tests, it is useful to realize the null hypothesis we test against can be any numerical value. When we compare two groups, we often test whether the difference between groups is zero.

Imagine a researcher who who is interested in voluntary participation in a national program to train young infants' motor skills. The researcher wants to test whether more boys than girls are brought in to the training program by their parents. One could test whether the difference betwen boys and girls is zero. However, this would ignore that the human sex ratio of boys to girls is not exactly 1:1, or an odds ratio of 1. On average, slightly more boys than girls are born. Given that a null hypothesis of exactly zero is unlikely, the researcher tests whether the difference between boys and girls is surprising, assuming an odds ratio in the range of 0.93 to 1.07. If the null hypothesis can be rejected (indicating the effect is surprisingly smaller than OR 0.93, or sirprisingly larger than 1.07), the researcher decides to act as if there is a real gender difference in participation in the motor skill training program. This test against is known as a *minimal effect test*.

However, even if the researcher would consider any odds ratio statistically smaller than 0.93 or larger than 1.07 a rejection of the null hypothesis, not every difference is large enough to matter.  After an extensive discussion with experts, the researcher decides that any effect smaller than an odds ratio of 0.8 or larger than an odds ratio of 1.2 is substantial enough to move money away from the core training program, and address the gender imbalance in the voluntary participation rate. To test whether the gender difference is not large enough to matter, the researcher can perform an *equivalence test*, which tests whether the observed gender difference is statistically larger than an odds ratio of 0.8, *and* statistically smaller than an odds ratio of 1.2. If this is the case, the researcher can act as if the gender difference is too small to intervene. 

One can conclude statistical equivalence whenever a 90% confidence interval around the observed estimate does not contain the smallest effect size of interest (SESOI; the values 0.8 and 1.2 in the previous example). When two one-sided tests (i.e., one testing whether the odds ratio is larger than 0.8 the other testing whether the odds ration is smaller than 1.2) are statistically significant, we can conclude statistical equivalence. By combining null-hypothesis tests and equivalence tests, four combinations of results can be observed (see Figure 1). Two outcomes are straightforward to interpret, namely when the effect is equivalent to zero, and not different from zero (an effect that is not meaningful), and when the effect is not equivalent, and different from zero (a meaningful effect). In addition, the effect can be different from zero and too small to be considered meaningful (an effect which is not meaningful) or the effect can neither differ from zero, not be equivalent to zero (an undetermined result, where more data is needed).

Even though equivalence tests are just a small variation of traditional frequentist null-hypothesis significance tests (and test against the SESOI, instead of 0), their use was limited until the availability of user-friendly software to perform the calculations [@lakens_equivalence_2017]. In this article, we provide several examples of equivalence tests which illustrate the procedure. Furthermore, we discuss different approaches to determining the SESOI for psychological research, and provide detailed reproducible examples of how to perform power analyses when designing equivalence tests, and statistical re-analyses of published psychology experiments. 

# Justifying the Smallest Effect Size of Interest

Equivalence tests are performed against a value that is considered the smallest effect size of interest (SESOI). The SESOI can sometimes based on just noticeable differences, which can be objectively determined. Most often, however, it is a subjective decision that varies across individuals and time. The SESOI is ideally based on a cost-benefit analysis. Since both costs and benefits are necessarily relative, the SESOI will depend on the researcher who designs the study. The goal of setting a SESOI is to clearly justify why designing a study that has a high probability of rejecting effects larger than a specified value contributes to our knowledge base. Researchers should not aim to determine a SESOI that is universally valid. The goal is to set a SESOI such that inferences based on it answer a meaningful question. 

##Objective Justifications of a SESOI

An objectively determined SESOI should be based on quantifiable theoretical predictions, such as computational models. Sometimes, the only theoretical prediction is that an effect should be noticeable. In such circumstances, the SESOI can be set based on just noticeable differences. For example, Burriss and colleagues [-@burriss_changes_2015] examined whether women displayed an increase in redness in the face during the fertile phase of their ovulatory cycle. The hypothesis was that a slighly redder skin signals greater attractiveness and physical health, and sending this signal to men yields an evolutionary advantage. This hypothesis requires that the increase in redness can be detected with the naked eye by men. They collected data from 22 women and showed that there was indeed an increase in redness of the facial skin of woman during their fertile period. However, this increase was not large enough to be noticeable with the naken eye by men, thus falsifying their hypothesis. Because the just noticeable difference in redness of the skin can be measured, it is possible to objectively establish the SESOI.

Another example of an objectively determined SESOI can be found in @button_minimal_2015 where the minimal clinically important difference on the Beck Depression Inventory - II was determined by asking 1039 patients when they subjectively felt less depressed (i.e., when they personally noticed an improvement) and relating this to the corresponding difference score on the depression inventory. 

##Subjective justifications of a SESOI

We distinguish between three categories of subjective justifications for SESOI. First, researchers can use benchmarks. For example, one might set the SESOI to a standardized effect size of d = 0.5, which would allow one to reject effect as large or larger than a 'medium' effect size (Cohen, 1988). Similarly, effect sizes smaller than a Cohen's d of 0.1 are sometimes considered trivially small (Maxwell, Lau, & Howard, 2015). Relying on a benchmark is the weakest possible justification of a SESOI, and should be avoided. 

```{r, results="hide"}
library(pwr)
alpha <- 0.05
N <- 100
pwr.t.test(n=N,sig.level=alpha,power=0.33,type="one.sample",alternative="two.sided")$d
```


Second, researchers can determine the SESOI based on the literature. Ideally, researchers would specify the SESOI in their research, but this is not yet common practice. It is thus up to researchers who build on earlier work to decide which effect size is too small to be meaningful, given an earlier study. Simonsohn (2015) recently proposed to set the SESOI to 33% of the effect size an earlier study could detect based on the sample size. For example, consider a study where 100 participants answered a question, which was analyzed with an one-sample t-test. For a two-sided test with an alpha of .05 this test had 33% power to detect a d = `r round(pwr.t.test(n=N,sig.level=alpha,power=0.33,type="one.sample",alternative="two.sided")$d,3)`. 

Another justifiable choice we would like to propose here is to use the smallest observed effect size that could have been statistically significant in the original study as the SESOI in a replication study. Based only on the alpha level and the sample size, we can calculate the criticial test value (e.g., *t*, *F*, *Z*). This critical test value can also be transformed to a standardized effect size (e.g., $d = t \sqrt { \frac { 1} { n _ { 1} } + \frac { 1} { n _ { 2} } }$), which can thus be interpreted as a *critical effect size*. All observed effect sizes smaller than the critical effect size would not be statistically significant in an original study, given the alpha and sample size. By setting the SESOI to the critical effect size, an equivalence test can reject all observed effect sizes that could have been detected in an earlier study. 

```{r, results="hide"}
library(TOSTER)
powerTOSTone(alpha=0.05, statistical_power=0.9, low_eqbound_d=-0.33, high_eqbound_d=0.33)
```


Third, researchers can set the SESOI based on the resources they have available. The amount of data you can collect limits the inferences you can make. Given a an alpha level and a sample size, researchers can calculate the smallest effect size that can be rejected with the desired power in an equivalence test. For example, a researcher who plans to perform a two-sided one-sample t-test using an alpha of 5%, based on data from 100 observations, has 90% power declare equivalence when testing a SESOI of d = 0.33. Whether or not a test against a SESOI based on the available resources contributes to the scientific literature depends. One should not expect a test based on 11 observations, which provides 90% power to reject effects larger than d = 1, is particularly informative, given that most effects in psychology are substantially smaller than d = 1. But by transparently reporting the effects one can detect and reject, based on the study design, researchers can communicate the information their study contributes. 

<!-- It would be nice to adapt the current equivalence testing power functions, such that you can not just calculate N, but also the SESOI -->

```{r}
p <- 0.05
N <- 100
crit_d <- abs(qt(p/2, (N*2)-2))/sqrt(N/2)
```

<!-- This is just an example of a comment, and a formula -->
$$t _ { L } = \frac { M _ { 1} - M _ { 2} - \Delta _ { L } } { \sigma \sqrt { \frac { 1} { n _ { 1} } + \frac { 1} { n _ { 2} } } }, t _ { U } = \frac { M _ { 1} - M _ { 2} - \Delta _ { U } } { \sigma \sqrt { \frac { 1} { n _ { 1} } + \frac { 1} { n _ { 2} } } }
$$

## Examples

#Adressing Trello cards

For the example where the results are inconclusive, explain how adding more data, will, assuming the true effect is 0, lead to a smaller confidence interval, and thus will lead to an informative test. However, with increasing data, the effect could also slightly increase, to above or at the SESOI level, and then the effect will be significant and not equivalent. With massive amounts of data, many results will be significant and equivalent. 

Boundary hacking should go into the discussion.



#Equivalence test for Meta-analysis

Hyde, Lindberg, Linn, Ellis, and Williams (2008) report that effect sizes for gender differences in mathematics tests across the 7 million students in the US represent trivial differences, where a trivial difference is specified as an effect size smaller then d = 0.1. The present a table with Cohen's d and se is reproduced below:

|Grades|d + se|
|---|---|
|Grade 2	|0.06 +/- 0.003|
|Grade 3	|0.04 +/- 0.002|
|Grade 4	|-0.01 +/- 0.002|
|Grade 5	|-0.01 +/- 0.002|
|Grade 6	|-0.01 +/- 0.002|
|Grade 7	|-0.02 +/- 0.002|
|Grade 8	|-0.02 +/- 0.002|
|Grade 9	|-0.01 +/- 0.003|
|Grade 10	|0.04 +/- 0.003|
|Grade 11	|0.06 +/- 0.003|

```{r, fig.width=6}
library("TOSTER")
TOSTmeta(ES = 0.06, se = 0.003, low_eqbound_d=-0.1, high_eqbound_d=0.1, alpha=0.05)
```

We see that indeed, all effect size estimates are measured with such high precision, we can conclude that they fall within the equivalence bound of d = -0.1 and d = 0.1. However, note that all of the effects are also statistically significant - so, the the effects are statistically different from zero, and practically equivalent. 

*Hyde, J. S., Lindberg, S. M., Linn, M. C., Ellis, A. B., & Williams, C. C. (2008). Gender similarities characterize math performance. Science, 321(5888), 494-495.*

&nbsp;

&nbsp;

#Independent Groups Equivalence Test

&nbsp;

##Independent Groups Student's Equivalence Test

Eskine (2013) showed that participants who had been exposed to organic food were substantially harsher in their moral judgments relative to those exposed to control (d = 0.81, 95% CI: [0.19, 1.45]). A replication by Moery & Calin-Jageman (2016, Study 2) did not observe a significant effect (Control: n = 95, M = 5.25, SD = 0.95, Organic Food: n = 89, M = 5.22, SD = 0.83). Following Simonsohn's (2015) recommendation the equivalence bound was set to the effect size the original study had 33% power to detect (with n = 21 in each condition, this means the equivalence bound is d = 0.48, which equals a difference of 0.384 on a 7-point scale given the sample sizes and a pooled standard deviation of 0.894). Using a TOST equivalence test with alpha = 0.05, assuming equal variances, and equivalence bounds of d = -0.43 and d = 0.43 is significant, t(182) = -2.69, p = 0.004. We can reject effects larger than d = 0.43. 

```{r, fig.width=6}
TOSTtwo.raw(m1=5.25,m2=5.22,sd1=0.95,sd2=0.83,n1=95,n2=89,low_eqbound=-0.384, high_eqbound=0.384, alpha = 0.05, var.equal=TRUE)
```

*Moery, E., & Calin-Jageman, R. J. (2016). Direct and Conceptual Replications of Eskine (2013): Organic Food Exposure Has Little to No Effect on Moral Judgments and Prosocial Behavior. Social Psychological and Personality Science, 7(4), 312-319. https://doi.org/10.1177/1948550616639649 *

&nbsp;

##Independent Groups Welch's Equivalence Test

Deary, Thorpe, Wilson, Starr, and Whally (2003) report the IQ scores of 79,376 children in Scotland (39,343 girls and 40,033 boys). The IQ score for girls (M = 100.64, SD = 14.1) girls and boys (100.48, SD = 14.9) was non-significant. With such a huge sample, we can examine whether the data is equivalent using very small equivalence bounds, such as -0.05 and 0.05. Because sample sizes are unequal, and variances are unequal, Welch's t-test is used, and we can conclude IQ scores are indeed equivalent, based on equivalence bounds of -0.05 and 0.05.  


```{r, fig.width=6}
TOSTtwo(m1=100.64,m2=100.48,sd1=14.1,sd2=14.9,n1=39343,n2=40033,low_eqbound_d=-0.05, high_eqbound_d=0.05, alpha = 0.05, var.equal=FALSE)
```

*Deary, I. J., Thorpe, G., Wilson, V., Starr, J. M., & Whalley, L. J. (2003). Population sex differences in IQ at age 11: The Scottish mental survey 1932. Intelligence, 31(6), 533-542.*

&nbsp;

&nbsp;

#One-Sample Equivalence Test

&nbsp;

Lakens, Semin, & Foroni (2012) examined whether the color of Chinese ideographs (white vs. black) would bias whether participants judged that the ideograph correctly translated positive, neutral, or negative words. In Study 4 the prediction was that participants would judge negative words to be correctly translated by black ideographs above guessing average, but no effects were predicted for translations in the other 5 between subject conditions in the 2 (ideograph color: white vs. black) x 3 (word meaning: positive, neutral, negative) design. The table below is a summary of the data in all 6 conditions:

|Color	|Valence	|M	|%	|SD	|t	|df	|p	|d|
|---|---|---|---|---|---|---|---|---|
|White	|Positive	|6.05	|50.42	|1.50	|0.15	|20	|.89	|.03|
|White	|Negative	|6.68	|55.67	|1.70	|1.75	|18	|.10	|.40|
|White	|Neutral	|5.95	|49.58	|1.40	|0.87	|19	|.87	|.04|
|Black	|Positive	|6.45	|53.75	|1.91	|1.06	|19	|.30	|.24|
|Black	|Negative	|6.95	|57.92	|1.15	|3.71	|19	|.00	|.80|
|Black	|Neutral	|5.71	|47.58	|1.79	|0.73	|20	|.47	|.16|

With 19 to 21 participants in each between subject condition, the study had 80% power to detect equivalence with equivalence bounds of -0.68 to d = 0.68.

```{r}
powerTOSTone(alpha=0.05, statistical_power=0.8, low_eqbound_d=-0.68, high_eqbound_d=0.68)
```

When we perform 5 equivalence tests using equivalence bounds of -0.68 and 0.68, using a Holm-Bonferroni sequential procedure to control error rates, we can conclude statistical equivalence for the data in row 1, 3, and 6, but the conclusion is undetermined for tests 2 and 4, where the data is neither significantly different from guessing average, nor statistically equivalent. For example, performing the test for the data in row 6:

```{r, fig.width=6}
TOSTone(m=5.71,mu=6,sd=1.79,n=20,low_eqbound_d=-0.68, high_eqbound_d=0.68, alpha=0.05)
```

It is clear I should have been more tentative in my conclusions in this study. Not only can I not conclude equivalence in some of the conditions, the equivalence bound I had 80% power to detect is very large, meaning the possibility that there are theoretically interesting but smaller effects remains. 

*Lakens, D., Semin, G. R., & Foroni, F. (2012). But for the bad, there would not be good: Grounding valence in brightness through shared relational structures. Journal of Experimental Psychology: General, 141(3), 584-594. https://doi.org/10.1037/a0026468 *

&nbsp;

&nbsp;

#Equivalence test for correlations

&nbsp;

Olson, Fazio, and Hermann (2007) reported correlations between implicit and explicit measures of self-esteem, such as the IAT, Rosenberg's self-esteem scale, a feeling thermometer, and trait ratings. In Study 1 71 participants completed the self-esteem measures. Because no equivalence bounds are mentioned, we can see which equivalence bounds the researchers would have 80% power to detect. 

```{r, fig.width=6}
powerTOSTr(alpha=0.05, statistical_power=0.8, low_eqbound_r=-0.24, high_eqbound_r=0.24)
```
With 71 pairs of observations between measures, the researchers have 80% power for equivalence bounds of r = -0.24 and r = 0.24. 

The correlations observed by Olson et al (2007), Study 1, are presented in the table below (significant correlations are flagged by an asteriks). 

|Measure |IAT |Rosenberg|Feeling thermometer| Trait ratings|
|---|---|---|---|---|
|IAT |- |-.12 |-.09 |-.06|
|Rosenberg| | - |.62*| .09|
|Feeling thermometer| | | - |.29*|
|Trait ratings| | | |-|

We can test each correlation, for example the correlation between the IAT and the Rosenberg self-esteem scale of -0.12, given 71 participants, and using equivalence bounds of r_U = 0.24 and r_L = -0.24. 

```{r, fig.width=6}
TOSTr(n=71, r=-0.06, low_eqbound_r=-0.24, high_eqbound_r=0.24, alpha=0.05)
```
We see that none of the correlations can be declared to be statistically equivalent. Instead, the tests yield undetermined outcomes: the correlations are not significantly different from 0, nor statistically equivalent.  

*Olson, M. A., Fazio, R. H., & Hermann, A. D. (2007). Reporting tendencies underlie discrepancies between implicit and explicit measures of self-esteem. Psychological Science, 18(4), 287-291.*

# Discussion


\newpage

# References

Button, K. S., Kounali, D., Thomas, L., Wiles, N. J., Peters, T. J., Welton, N. J., . Lewis, G. (2015). Minimal clinically important difference on the Beck Depression Inventory - II according to the patient's perspective. Psychological Medicine, 45(15), 3269-3279. https://doi.org/10.1017/S0033291715001270

Burriss, R. P., Troscianko, J., Lovell, P. G., Fulford, A. J. C., Stevens, M., Quigley, R., . Rowland, H. M. (2015). Changes in Women's Facial Skin Color over the Ovulatory Cycle are Not Detectable by the Human Visual System. PLOS ONE, 10(7), e0130093. https://doi.org/10.1371/journal.pone.0130093


```{r create_r-references}
r_refs(file = "r-references.bib")
```

\setlength{\parindent}{-0.5in}
\setlength{\leftskip}{0.5in}
