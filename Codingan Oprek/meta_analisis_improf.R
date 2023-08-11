###############################
## Install and load packages ##
###############################
# Install Packages
# install.packages("meta")
# install.packages("metafor")

# Aktifkan Packages
library(meta)
library(metafor)
library(tidyverse)


######################
## Import Dataset   ##
######################

meta1 <- read.csv(file.choose(), sep = ',')

##################################
##      Star Meta-analysis      ##
##################################

glimpse(meta1)
meta1$n.e
class(meta1$n.e)
mean(meta1$n.e)
meta1$n.e <- as.numeric(meta1$n.e)
meta1$mean.e <- as.numeric(meta1$mean.e)
meta1$sd.e <- as.numeric(meta1$sd.e)
meta1$n.c <- as.numeric(meta1$n.c)
meta1$mean.c <- as.numeric(meta1$mean.c)
meta1$sd.c <- as.numeric(meta1$sd.c)
meta1$n.c <- as.numeric(meta1$n.c)

meta1$age_group <- as.factor(meta1$age_group)
meta1$control <- as.factor(meta1$control)
levels(meta1$age_group)
nlevels(meta1$age_group)
new.factor.levels <- c("gen", "older")
new.factor.levels
levels(meta1$age_group) <- new.factor.levels
meta1$age_group
meta1$pubyear
as.logical(meta1$pubyear >= 2010)
meta1[2,]
meta1[2, 1]
meta1[c(2,3), c(4,6)]
meta1[, c("author", "control")]
filter(meta1, n.e <= 50)
filter(meta1, !author %in% c("Aminof et al", 
                             "Perri et al",
                             "Hanani et al",
                             "Nicol et al",
                             "Zemestani and Fazeli Nikoo",
                             "Shaygan et al"))

meta1[2, "pubyear"] <- 2018
meta1[2, "pubyear"]

meta1$mean.e + 5
meta1$mean.e - meta1$mean.c

md <- meta1$mean.e - meta1$mean.c

meta1$md <- meta1$mean.e - 
  meta1$mean.c

meta1$n.c %>% mean()

meta1 %>% 
  filter(pubyear > 2009) %>% 
  pull(n.c) %>% 
  mean() %>% 
  sqrt()

save(meta1, file = "meta1.rda")

write.csv(meta1, file = "meta1.csv")

#-------------------------------------------------------
set.seed(123)
sample <- rnorm(n = 50, mean = 10, sd = 2)

mean(sample)

sd(sample)/sqrt(50)

# Set seed of 123 for reproducibility 
# and take a random sample (n=50).
set.seed(123)
sample <- rnorm(n = 50, mean = 20, sd = 5)

# Calculate the mean
mean(sample)


# Calculate the standard error
sd(sample)/sqrt(50)

# We define the following values for k and n:
k <- 25
n <- 125

# Calculate the proportion
p <- k/n
p


# Calculate the standard error
sqrt((p*(1-p))/n)


# Calculate the standard error
sqrt((p*(1-p))/n)


# Simulate two continuous variables x and y
set.seed(12345)
x <- rnorm(20, 50, 10)
y <- rnorm(20, 10, 3)

# Calculate the correlation between x and y
r <- cor(x,y)
r
# Generate two random variables with different population means
set.seed(123)
x1 <- rnorm(n = 20, mean = 10, sd = 3)
x2 <- rnorm(n = 20, mean = 15, sd = 3)

# Calculate values we need for the formulas
s1 <- sd(x1)
s2 <- sd(x2)
n1 <- 20
n2 <- 20


# Calculate the mean difference
MD <- mean(x1) - mean(x2)
MD

# Calculate s_pooled
s_pooled <- sqrt(
  (((n1-1)*s1^2) + ((n2-1)*s2^2))/
    ((n1-1)+(n2-1))
)

# Calculate the standard error
se <- s_pooled*sqrt((1/n1)+(1/n2))
se


# Load esc package
library(esc)

# Define the data we need to calculate SMD/d
# This is just some example data that we made up
grp1m <- 50   # mean of group 1
grp2m <- 60   # mean of group 2
grp1sd <- 10  # sd of group 1
grp2sd <- 10  # sd of group 2
grp1n <- 100  # n of group1
grp2n <- 100  # n of group2

# Calculate effect size
esc_mean_sd(grp1m = grp1m, grp2m = grp2m, 
            grp1sd = grp1sd, grp2sd = grp2sd, 
            grp1n = grp1n, grp2n = grp2n)



# Define example data needed for effect size calculation
x1 <- 20    # mean at t1
x2 <- 30    # mean at t2
sd1 <- 13   # sd at t1
n <- 80     # sample size
r <- 0.5    # correlation between t1 and t2

# Caclulate the raw mean difference
md_within <- x2 - x1

# Calculate the smd:
# Here, we use the standard deviation at t1
# to standardize the mean difference
smd_within <- md_within/sd1
smd_within
# Calculate standard error
se_within <- sqrt(((2*(1-r))/n) + 
                    (smd_within^2/(2*n)))
se_within
# Define data
a <- 46         # events in the treatment group
c <- 77         # events in the control group
n_treat <- 248  # sample size treatment group
n_contr <- 251  # sample size control group

# Calculate the risks
p_treat <- a/n_treat
p_contr <- c/n_contr

# Calculate the risk ratio
rr <- p_treat/p_contr
rr
# Calculate the log-risk ratio and its standard error
log_rr <- log(rr)
log_rr
se_log_rr <- sqrt((1/a) + (1/c) - (1/n_treat) - (1/n_contr))
se_log_rr

library(esc)

# Define data
grp1yes <- 45  # events in the treatment group
grp1no <- 98   # non-events in the treatment group
grp2yes <- 67  # events in the control group
grp2no <- 76   # non-events in the control group

# Calculate OR by setting es.type to "or"
esc_2x2(grp1yes = grp1yes, grp1no = grp1no,
        grp2yes = grp2yes, grp2no = grp2no,
        es.type = "or")

# Calculate logOR by setting es.type to "logit"
esc_2x2(grp1yes = grp1yes, grp1no = grp1no,
        grp2yes = grp2yes, grp2no = grp2no,
        es.type = "logit")

# Define Data
e_treat <- 28    # Number of events in the treatment group
e_contr <- 28    # Number of events in the control group
t_treat <- 3025  # Person-time in the treatment group
t_contr <- 2380  # Person-time in the control group

# Calculate IRR
irr <- (e_treat/t_treat)/(e_contr/t_contr)
irr
# Calculate log-IRR
log_irr <- log(irr)

# Calculate standard error
se_log_irr <- sqrt((1/e_treat)+(1/e_contr))

# Load esc package
library(esc)

# Define uncorrected SMD and sample size n
SMD <- 0.5
n <- 30

# Convert to Hedges g
g <- hedges_g(SMD, n)
g


# Define uncorrected correlation and SMD with their standard error
r_xy <- 0.34
se_r_xy <- 0.09
smd <- 0.65
se_smd <- 0.18

# Define reliabilities of x and y
r_xx <- 0.8
r_yy <- 0.7

# Correct SMD for unreliability in x
smd_c <- smd/sqrt(r_xx)
smd_c

se_c <- se_smd/sqrt(r_xx)
se_c




# Correct correlation for unreliability in x and y
r_xy_c <- r_xy/(sqrt(r_xx)*sqrt(r_yy))
r_xy_c



se_c <- se_r_xy/(sqrt(r_xx)*sqrt(r_yy))
se_c

# Define correlation to correct
r_xy <- 0.34
se_r_xy <- 0.09

# Define restricted and unrestricted SD
sd_restricted <- 11
sd_unrestricted <- 18

# Calculate U
U <- sd_unrestricted/sd_restricted

# Correct the correlation
r_xy_c <- (U*r_xy)/sqrt((U^2-1)*r_xy^2+1)
r_xy_c

# Correct the standard error
se_r_xy_c <- (r_xy_c/r_xy)*se_r_xy
se_r_xy_c
#----------------------------------------------------

# Load dmetar, esc and tidyverse (for pipe)
library(dmetar)
library(esc)
library(tidyverse)

# Load data set from dmetar
data(meta1)

# Calculate Hedges' g and the Standard Error
# - We save the study names in "study".
# - We use the pmap_dfr function to calculate the effect size
#   for each row.
SP_calc <- pmap_dfr(meta1, 
                    function(mean.e, sd.e, n.e, mean.c,
                             sd.c, n.c, author, ...){
                      esc_mean_sd(grp1m = mean.e,
                                  grp1sd = sd.e,
                                  grp1n = n.e,
                                  grp2m = mean.c,
                                  grp2sd = sd.c,
                                  grp2n = n.c,
                                  study = author,
                                  es.type = "g") %>% 
                        as.data.frame()}) 

# Let us catch a glimpse of the data
# The data set contains Hedges' g ("es") and standard error ("se")
glimpse(SP_calc)

# Calculate the inverse variance-weights for each study
SP_calc$w <- 1/SP_calc$se^2

# Then, we use the weights to calculate the pooled effect
pooled_effect <- sum(SP_calc$w*SP_calc$es)/sum(SP_calc$w)
pooled_effect



library(tidyverse) # needed for 'glimpse'
library(dmetar)
library(meta)

data(ThirdWave)
glimpse(ThirdWave)


m.gen <- metagen(TE = TE,
                 seTE = seTE,
                 studlab = Author,
                 data = ThirdWave,
                 sm = "SMD",
                 fixed = FALSE,
                 random = TRUE,
                 method.tau = "REML",
                 hakn = TRUE,
                 title = "Third Wave Psychotherapies")















# Menghitung summary effect
m1 <- metacont(n.e,	mean.e,	sd.e,	n.c,	mean.c,	sd.c,	studlab=author, data = meta1, subset = NULL,
               exclude = NULL, sm = "SMD")

# Forest Plots Meta Analysis
m1
forest(m1, common = F, 
       digital.sd = 2, 
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = FALSE,
       fontsize = 10)

# Forest Plots RiskOfBias
m1 <- metacont(n.e,	mean.e,	sd.e,	n.c,	mean.c,	sd.c,	studlab=author, data = meta1, sm = "SMD")

m1
forest.meta(m1, sortvar = TE,
            prediction = TRUE, 
            print.tau2 = FALSE,
            leftcols = c("studlab", "TE", "seTE", "RiskOfBias"),
            leftlabs = c("Author", "g", "SE", "Risk of Bias"))

# Analisis  group
# Country
mu1 <- update(m1, subgroup = location, bylab = "Country", common = F)

forest(m1)
mu2 <- update(m1, subgroup = range_age, bylab = "Range Age", common = F)
#mu3 <- update(m1, subgroup = psychosocial_condition, bylab = "kondisi Psikososial", common = F)
#mu4 <- update(m1, subgroup = assessment, bylab = "Assessment", common = F)
#mu5 <- update(m1, subgroup = lamanterapi, bylab = "Lama Terapi", common = F)
#mb1 <- metabind(mu1, mu2, mu3, mu4, mu5)
mb1 <- metabind(mu1, mu2)
forest(mb1, overall=T, fontsize = 10, spacing = 0.7)

mu4 <- update(m1, subgroup = type_of_assessment, bylab = "Type of Assessment", common = F)
mu5 <- update(m1, subgroup = length_of_therapy, bylab = "length of Therapy", common = F)
mb1 <- metabind(mu4, mu5)
forest(mb1, overall=T, fontsize = 10, spacing = 0.7)

#Evaluasi bias publikasi
funnel(m1, common = F, level = 0.95)
funnel_test <- metabias(m1, method.bias = "Egger")
funnel_test
metafor::fsn(m1$TE, m1$seTE^2) #fail safe-N