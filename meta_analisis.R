###############################
## Install and load packages ##
###############################
# Install Packages
# install.packages("meta")
# install.packages("metafor")

# Aktifkan Packages
library(meta)
library(metafor)

######################
## Import Dataset   ##
######################

meta1 <- read.csv(file.choose(), sep = ',')

##################################
##      Star Meta-analysis      ##
##################################


# Menghitung summary effect
m1 <- metacont(n.e,	mean.e,	sd.e,	n.c,	mean.c,	sd.c,	studlab=author, data = meta1, subset = NULL,
               exclude = NULL, sm = "SMD")
m1
forest(m1, common = F, digital.sd = 2, sortvar = TE, fontsize = 10)

# Analisa SubGroup Country
#mu1 <- update(m1, subgroup = location, bylab = "Country", common = F)
#mb1 <- metabind(mu1)
#forest(mb1, overall=T, fontsize = 10, spacing = 0.7)

# Analisa SubGroup Range Age
#mu2 <- update(m1, subgroup = range_age, bylab = "Range Age", common = F)
#mb1 <- metabind(mu2)
#forest(mb1, overall=T, fontsize = 10, spacing = 0.7)

# Analisa SubGroup Type of Assessment
#mu4 <- update(m1, subgroup = type_of_assessment, bylab = "Type of Assessment", common = F)
#mb1 <- metabind(mu4)
#forest(mb1, overall=T, fontsize = 10, spacing = 0.7)

# Analisa SubGroup length_of_therapy
#mu5 <- update(m1, subgroup = length_of.therapy, bylab = "length of Therapy", common = F)
#mb1 <- metabind(mu5)
#forest(mb1, overall=T, fontsize = 10, spacing = 0.7)


# Analisa SubGroup treatment_duration_based 
#mu7 <- update(m1, subgroup = treatment_duration_based, bylab = "Treatment Duration Based", common = F)
#mb1 <- metabind(mu7)
#forest(mb1, overall=T, fontsize = 10, spacing = 0.7)

# Analisa SubGroup Moderate_to_Severe_Depression 
#mu10 <- update(m1, subgroup = settings, bylab = "Settings", common = F)
#mb1 <- metabind(mu10)
#forest(mb1, overall=T, fontsize = 10, spacing = 0.7)

# Analisa SubGroup Country
mu1 <- update(m1, subgroup = location, bylab = "Country", common = F)
mb1 <- metabind(mu1)
forest(mb1,
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = FALSE,
       leftlabs = c("Author", "g", "SE"))

# Analisa SubGroup Age Based 
mu2 <- update(m1, subgroup = age_based, bylab = "Age Based ", common = F)
mb1 <- metabind(mu2)
forest(mb1,
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = FALSE,
       leftlabs = c("Author", "g", "SE"))

# Analisa SubGroup Treatment Duration Based
mu3 <- update(m1, subgroup = treatment_duration_based, bylab = "Treatment Duration Based", common = F)
mb1 <- metabind(mu3)
forest(mb1,
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = FALSE,
       leftlabs = c("Author", "g", "SE"))

# Analisa SubGroup Type of Assessment
mu4 <- update(m1, subgroup = type_of_assessment, bylab = "Type of Assessment", common = F)
mb1 <- metabind(mu4)
forest(mb1,
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = FALSE,
       leftlabs = c("Author", "g", "SE"))

# Analisa SubGroup Gender(Male) 
mu5 <- update(m1, subgroup = Gender_Male, bylab = "Gender_Male", common = F)
mb1 <- metabind(mu5)
forest(mb1,
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = FALSE,
       leftlabs = c("Author", "g", "SE"))

# Analisa SubGroup Gender(Female) 
mu6 <- update(m1, subgroup = Gender_Female, bylab = "Gender Female", common = F)
mb1 <- metabind(mu6)
forest(mb1,
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = FALSE,
       leftlabs = c("Author", "g", "SE"))

# Analisa SubGroup Severity Mild to Moderate Depression 
mu7 <- update(m1, subgroup = Severity_Mild_to_Moderate_Depression, bylab = "Severity Mild to Moderate Depression", common = F)
mb1 <- metabind(mu7)
forest(mb1,
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = FALSE,
       leftlabs = c("Author", "g", "SE"))

# Analisa SubGroup Severity Moderate to Severe Depression 
mu8 <- update(m1, subgroup = Severity_Moderate_to_Severe_Depression, bylab = "Severity Moderate to Severe Depression", common = F)
mb1 <- metabind(mu8)
forest(mb1,
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = FALSE,
       leftlabs = c("Author", "g", "SE"))


# Analisa SubGroup Moderate_to_Severe_Depression 
mu9 <- update(m1, subgroup = settings, bylab = "Settings", common = F)
mb1 <- metabind(mu9)
forest(mb1,
       sortvar = TE,
       prediction = TRUE, 
       print.tau2 = FALSE,
       leftlabs = c("Author", "g", "SE"))

#Evaluasi bias publikasi
funnel(m1, common = F, level = 0.95)
funnel_test <- metabias(m1, method.bias = "Egger")
funnel_test
metafor::fsn(m1$TE, m1$seTE^2) #fail safe-N

