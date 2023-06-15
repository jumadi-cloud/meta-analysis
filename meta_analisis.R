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
forest(m1, common = F, digital.sd = 2, sortvar = TE, fontsize = 8)

# Analisis  group
mu1 <- update(m1, subgroup = location, bylab = "Country", common = F)
#mu2 <- update(m1, subgroup = range_age, bylab = "Range Age", common = F)
#mu3 <- update(m1, subgroup = psychosocial_condition, bylab = "kondisi Psikososial", common = F)
#mu4 <- update(m1, subgroup = assessment, bylab = "Assessment", common = F)
mu5 <- update(m1, subgroup = lamanterapi, bylab = "Lama Terapi", common = F)
#mb1 <- metabind(mu1, mu2, mu3, mu4, mu5)
mb1 <- metabind(mu1, mu5)
forest(mb1, overall=T, fontsize = 8, spacing = 0.7)

#Evaluasi bias publikasi
funnel(m1, common = F, level = 0.95)
funnel_test <- metabias(m1, method.bias = "Egger")
funnel_test
metafor::fsn(m1$TE, m1$seTE^2) #fail safe-N

