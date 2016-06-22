# LOAD
cat("\014")
rm(list=ls())
#setwd("/Users/hectorbahamonde/RU/research/Clientelism_paper/Paper_Presentation")

# Load the data
library(foreign) # install.packages("foreign") 
dat <- read.dta("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/clientelism.dta")

# Recoding vars
dat <- na.omit(dat)
dat$ed <- as.numeric(dat$ed)
dat$vb3 <- as.numeric(dat$vb3)
dat$exc7 <- as.numeric(dat$exc7)
dat$polinv <- as.numeric(dat$polinv)
dat$polinv <- as.numeric(dat$polinv)
dat$polinv1 <- as.numeric(dat$polinv1)
dat$polinv2 <- as.numeric(dat$polinv2)
dat$polinv3 <- as.numeric(dat$polinv3)
dat$polinv4 <- as.numeric(dat$polinv4)
dat$polinv5 <- as.numeric(dat$polinv5)
dat$ing4 <- as.numeric(dat$ing4)


# constructing relative wealth index (Cordova 2009)
library(car) # install.packages("car") 
dat$wealth1 = recode(as.numeric(dat$wealth1), "1 = 0 ; 2 = 1")
dat$wealth2 = recode(as.numeric(dat$wealth2), "1 = 0 ; 2 = 1")
dat$wealth3 = recode(as.numeric(dat$wealth3), "1 = 0 ; 2 = 1")
dat$wealth4 = recode(as.numeric(dat$wealth4), "1 = 0 ; 2 = 1")
dat$wealth5 = recode(as.numeric(dat$wealth5), "1 = 0 ; 2 = 1 ; 3 = 2 ; 4 = 3")
dat$wealth6 = recode(as.numeric(dat$wealth6), "1 = 0 ; 2 = 1")
dat$wealth7 = recode(as.numeric(dat$wealth7), "1 = 0 ; 2 = 1")
dat$wealth8 = recode(as.numeric(dat$wealth8), "1 = 0 ; 2 = 1")
dat$wealth9 = recode(as.numeric(dat$wealth9), "1 = 0 ; 2 = 1")
dat$wealth10 = recode(as.numeric(dat$wealth10), "1 = 0 ; 2 = 1")

## splitting df in two
dat.ur <- subset(dat, dat$urban == "Ur")
dat.ru <- subset(dat, dat$urban == "Ru")



## PCA for RURAL and URBAN
wealth.dat.ur = data.frame(dat.ur$wealth1,dat.ur$wealth2,dat.ur$wealth3, dat.ur$wealth4, dat.ur$wealth5, dat.ur$wealth6, dat.ur$wealth7, dat.ur$wealth8, dat.ur$wealth9, dat.ur$wealth10)
wealth.dat.ru = data.frame(dat.ru$wealth1,dat.ru$wealth2,dat.ru$wealth3, dat.ru$wealth4, dat.ru$wealth5, dat.ru$wealth6, dat.ru$wealth7, dat.ru$wealth8, dat.ru$wealth9, dat.ru$wealth10)


dat.ur$wealth = 
        (princomp(wealth.dat.ur, scores = T)$scores[,1] * (dat.ur$wealth1-mean(dat.ur$wealth1) / sd(dat.ur$wealth1))) + 
        (princomp(wealth.dat.ur, scores = T)$scores[,2] * (dat.ur$wealth1-mean(dat.ur$wealth2) / sd(dat.ur$wealth2))) + 
        (princomp(wealth.dat.ur, scores = T)$scores[,3] * (dat.ur$wealth1-mean(dat.ur$wealth3) / sd(dat.ur$wealth3))) + 
        (princomp(wealth.dat.ur, scores = T)$scores[,4] * (dat.ur$wealth1-mean(dat.ur$wealth4) / sd(dat.ur$wealth4))) + 
        (princomp(wealth.dat.ur, scores = T)$scores[,5] * (dat.ur$wealth1-mean(dat.ur$wealth5) / sd(dat.ur$wealth5))) + 
        (princomp(wealth.dat.ur, scores = T)$scores[,6] * (dat.ur$wealth1-mean(dat.ur$wealth6) / sd(dat.ur$wealth6))) + 
        (princomp(wealth.dat.ur, scores = T)$scores[,7] * (dat.ur$wealth1-mean(dat.ur$wealth7) / sd(dat.ur$wealth7))) + 
        (princomp(wealth.dat.ur, scores = T)$scores[,8] * (dat.ur$wealth1-mean(dat.ur$wealth8) / sd(dat.ur$wealth8))) + 
        (princomp(wealth.dat.ur, scores = T)$scores[,9] * (dat.ur$wealth1-mean(dat.ur$wealth9) / sd(dat.ur$wealth9))) + 
        (princomp(wealth.dat.ur, scores = T)$scores[,10] * (dat.ur$wealth1-mean(dat.ur$wealth10) / sd(dat.ur$wealth10)))        



dat.ru$wealth = 
        (princomp(wealth.dat.ru, scores = T)$scores[,1] * (dat.ru$wealth1-mean(dat.ru$wealth1) / sd(dat.ru$wealth1))) + 
        (princomp(wealth.dat.ru, scores = T)$scores[,2] * (dat.ru$wealth1-mean(dat.ru$wealth2) / sd(dat.ru$wealth2))) + 
        (princomp(wealth.dat.ru, scores = T)$scores[,3] * (dat.ru$wealth1-mean(dat.ru$wealth3) / sd(dat.ru$wealth3))) + 
        (princomp(wealth.dat.ru, scores = T)$scores[,4] * (dat.ru$wealth1-mean(dat.ru$wealth4) / sd(dat.ru$wealth4))) + 
        (princomp(wealth.dat.ru, scores = T)$scores[,5] * (dat.ru$wealth1-mean(dat.ru$wealth5) / sd(dat.ru$wealth5))) + 
        (princomp(wealth.dat.ru, scores = T)$scores[,6] * (dat.ru$wealth1-mean(dat.ru$wealth6) / sd(dat.ru$wealth6))) + 
        (princomp(wealth.dat.ru, scores = T)$scores[,7] * (dat.ru$wealth1-mean(dat.ru$wealth7) / sd(dat.ru$wealth7))) + 
        (princomp(wealth.dat.ru, scores = T)$scores[,8] * (dat.ru$wealth1-mean(dat.ru$wealth8) / sd(dat.ru$wealth8))) + 
        (princomp(wealth.dat.ru, scores = T)$scores[,9] * (dat.ru$wealth1-mean(dat.ru$wealth9) / sd(dat.ru$wealth9))) + 
        (princomp(wealth.dat.ru, scores = T)$scores[,10] * (dat.ru$wealth1-mean(dat.ru$wealth10) / sd(dat.ru$wealth10)))  

## combining the two DF's
dat = rbind(dat.ur, dat.ru)


# municipal population from census
pop.municipalities = data.frame(
        municipality=c( "Acopiara", "Aloandia", "Aparecida de Goiania", "Belo Horizonte", "Belem", "Blumenau", "Branquinha",  "Brasilia", # 1
                        "Capela",  "Coronel Ezequiel", "Cuiaba", "Curitibanos", "Duque de Caxias", "Embu Guacu", "Fortaleza", "Franca",  # 2
                        "Itagiba", "Itaguaje", "Itumbiara", "Itupeva", "Jaboatao dos Guararapes", "Jaciara", "Jaragua do Sul", "Ji Parana", # 3
                        "Jijoca de Jericoacoara", "Juazeiro", "Lontra", "Marilia", "Minacu", "Mogi das Cruzes", "Mossoro", "Narandiba", # 4
                        "Pacaja", "Passos", "Pelotas", "Ponta Grossa", "Porecatu", "Porto Esperidiao", "Porto Velho", "Pocoes", "Progresso", # 5
                        "Redencao", "Rio Bonito", "Rio Branco", "Rio de Janeiro", "Senador Guiomard", "Sao Jose dos Campos", "Sao Joao del Rei", # 6
                        "Sao Lourenco", "Sao Paulo", "Timbauba", "Uaua", "Vera Cruz", "Vilhena"),  # 7
        pop=c(51160, 2051, 455657, 2375151, 4551, 309011, 10583, 2570160, # 1
              17077, 5405, 551098, 37748, 855048, 62769, 2452185, 318640, # 2
              15193, 4568, 92883, 44859, 644620, 25647, 143123, 116610, # 3
              17002, 197965, 8397, 216745, 31154, 387779, 259815, 4288, # 4
              39979, 106290, 328275, 311611, 14189, 11031, 428527, 44701, 6163, # 5
              26415, 55551, 336038, 6320446, 20179, 629921, 84469, # 6
              41657, 11253503, 53825, 24294, 23983, 76202) # 7 CORREGIR VERA CRUZ Y VER QUE ESTADO SON LAS OBS QUE TENGO, HAY COMO 4 VERA CRUCES
        )

## merge datasets
dat = merge(dat, pop.municipalities, by=c("municipality"), all.x =T)


# create variable large
library(foreign)
wagehalf.d <- read.dta("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/proportionofpoorcensus.dta")

wagehalf.d = data.frame(cbind(municipality = c( "Acopiara", "Aloandia", "Aparecida de Goiania", "Belo Horizonte", "Belem", "Blumenau", "Branquinha",  "Brasilia", # 1
                                   "Capela",  "Coronel Ezequiel", "Cuiaba", "Curitibanos", "Duque de Caxias", "Embu Guacu", "Fortaleza", "Franca",  # 2
                                   "Itagiba", "Itaguaje", "Itumbiara", "Itupeva", "Jaboatao dos Guararapes", "Jaciara", "Jaragua do Sul", "Ji Parana", # 3
                                   "Jijoca de Jericoacoara", "Juazeiro", "Lontra", "Marilia", "Minacu", "Mogi das Cruzes", "Mossoro", "Narandiba", # 4
                                   "Pacaja", "Passos", "Pelotas", "Ponta Grossa", "Porecatu", "Porto Esperidiao", "Porto Velho", "Pocoes", "Progresso", # 5
                                   "Redencao", "Rio Bonito", "Rio Branco", "Rio de Janeiro", "Senador Guiomard", "Sao Jose dos Campos", "Sao Joao del Rei", # 6
                                   "Sao Lourenco", "Sao Paulo", "Timbauba", "Uaua", "Vera Cruz", "Vilhena"), 
                 large = as.numeric(wagehalf.d$wagehalf >= median(wagehalf.d$wagehalf))
                 )
           )
dat = merge(dat, wagehalf.d, by=c("municipality"), all.x =T)

## recode large
library(car) # install.packages("car") 
dat$large <- as.numeric(dat$large)
dat$large <- recode(dat$large, "1 = 0 ; 2 = 1")

# save unmatched dataset
save(dat, file = "/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/dat.RData")


# Constructing Matched Set
set.seed(604)
library(MatchIt) # install.packages("MatchIt", dependencies=TRUE)
# wealth + urban + munopp + polinv,  
m.out <- matchit(large ~ wealth + urban + munopp + polinv,
                 discard = "hull.both", 
                 method = "cem",
                 data = dat,
                 verbose = F
                 )


#print. <- print(m.out)
sum.match = summary(m.out)

# Match Data
m.data <- match.data(m.out)


# Recode client1dummy after matching
library(car) # install.packages("car") 
m.data$clien1dummy <- as.numeric(m.data$clien1dummy)
m.data$clien1dummy <- recode(m.data$clien1dummy, "1 = 0 ; 2 = 1")


#
# Transform the continuous "pop" variable in ten segments // mostly for simulation purposes
pop.10.m = cut(m.data$pop, breaks = c(0,
                                      quantile(m.data$pop, .10), 
                                      quantile(m.data$pop, .20), 
                                      quantile(m.data$pop, .30), 
                                      quantile(m.data$pop, .40),
                                      quantile(m.data$pop, .50),
                                      quantile(m.data$pop, .60),
                                      quantile(m.data$pop, .70),
                                      quantile(m.data$pop, .80),
                                      quantile(m.data$pop, .90),
                                      quantile(m.data$pop, 1)
), 
include.lowest=T, labels=c(
  "0-10","11-20","21-30","31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100")
)


pop.10.r = cut(dat$pop, breaks = c(0,
                                   quantile(dat$pop, .10), 
                                   quantile(dat$pop, .20), 
                                   quantile(dat$pop, .30), 
                                   quantile(dat$pop, .40),
                                   quantile(dat$pop, .50),
                                   quantile(dat$pop, .60),
                                   quantile(dat$pop, .70),
                                   quantile(dat$pop, .80),
                                   quantile(dat$pop, .90),
                                   quantile(dat$pop, 1)
), 
include.lowest=T, labels=c(
  "0-10","11-20","21-30","31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-100")
)

## attaching
m.data$pop.10.m = as.numeric(pop.10.m)
dat$pop.10.r = as.numeric(pop.10.r)

# save matched dataset
save(m.data, file = "/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")


#####################################################################
### PARAMETRIC models
#####################################################################


### GEE: In gee there is no quasipossion, because gee is in a way already quasi.
### With GEE we do not fit a poisson glm, but use in the construction of the sandwich covariance 
### matrix the variance function of the poisson family. In Gee always an 'overdispersion' is estimated.

# 1
################################################
# gee.dich.m.X: DICH // MATCHED // GEE
################################################


set.seed(602); options(scipen=999)


# TABLES

# extract function for texreg
library(texreg)
extract.geepack <- function(model) {
  s <- summary(model)
  names <- rownames(s$coef)
  co <- s$coef[, 1]
  se <- s$coef[, 2]
  pval <- s$coef[, 4]
  
  n <- nrow(model.frame(model))
  nclust <- length(s$geese$clusz)
  
  gof = c(n, nclust)
  gof.names = c("Num. obs.", "Num. clust.")
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = rep(FALSE, length(gof))
  )
  return(tr)
}


# load data
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")


####### formulas
#### MATCHED SAMPLE
# m1: economic
m1.m = formula(clien1dummy ~ large + wealth + large:wealth + ed)
# m2: contextual
m2.m = formula(clien1dummy ~ wealth*munopp*large + pop.10.m)
# m3: political
m3.m = formula(clien1dummy ~ polinv*wealth + polinv*large + ing4 + vb3 + exc7)
#### 
m1.r = formula(clien1dummy ~ as.numeric(wagehalf.4) + wealth + as.numeric(wagehalf.4):wealth + ed + weights)
# m2: contextual
m2.r = formula(clien1dummy ~ wealth*munopp*as.numeric(wagehalf.4) + pop.10.m + weights)
# m3: political
m3.r = formula(clien1dummy ~ polinv*wealth + polinv*as.numeric(wagehalf.4) + ing4 + vb3 + exc7 + weights)
####### formulas


# models
library(geepack) # install.packages("geepack")
gee.dich.m.1.t = extract.geepack(gee.dich.m.1 <- geeglm(m1.m,
                                                        family = binomial(link = "logit"), 
                                                        id = municipality, 
                                                        weights = wt,
                                                        std.err = "san.se",
                                                        corstr = "independence",
                                                        data = m.data))

library(geepack) # install.packages("geepack")
gee.dich.m.2.t = extract.geepack(gee.dich.m.2 <- geeglm(m2.m, 
                                                        family = binomial(link = "logit"), 
                                                        id = municipality, 
                                                        weights = wt,
                                                        std.err = "san.se",
                                                        corstr = "independence",
                                                        data = m.data))

library(geepack) # install.packages("geepack")
gee.dich.m.3.t = extract.geepack(gee.dich.m.3 <- geeglm(m3.m,
                                                        family = binomial(link = "logit"), 
                                                        id = municipality, 
                                                        weights = wt,
                                                        std.err = "san.se",
                                                        corstr = "independence",
                                                        data = m.data))
# 2
################################################
# gee.cont.rgps.X: CONT // RAW GPS // GEE
################################################

set.seed(602); options(scipen=999)


# extract function for texreg
library(texreg)
extract.geepack <- function(model) {
  s <- summary(model)
  names <- rownames(s$coef)
  co <- s$coef[, 1]
  se <- s$coef[, 2]
  pval <- s$coef[, 4]
  
  n <- nrow(model.frame(model))
  nclust <- length(s$geese$clusz)
  
  gof = c(n, nclust)
  gof.names = c("Num. obs.", "Num. clust.")
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = rep(FALSE, length(gof))
  )
  return(tr)
}

# load data
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/dat.RData")

## transform the wagehalf variable
dat$wagehalf = round(dat$wagehalf, digits=0)


# Transform the continuous "treatment" variable in four segments
wagehalf.4 = cut(dat$wagehalf, breaks = c(0,
  quantile(dat$wagehalf, .25), 
  quantile(dat$wagehalf, .50), 
  quantile(dat$wagehalf, .75), 
  quantile(dat$wagehalf, 1)), include.lowest=T, labels=c("0-25","25-50","50-75","75-100")
  )

dat$wagehalf.4 = wagehalf.4




## Generating the Propensity Score 
library(CBPS, quietly = T) # install.packages("CBPS")
fit <- CBPS(as.factor(wagehalf.4) ~ wealth,# + polinv,# + munopp + polinv + ing4,  # wealth + munopp + polinv
            data = dat, 
            iterations = 25000, 
            twostep = TRUE, # F
            method = "over", # EXACT
            ATT = 0, # 2
            standardize = F) # F


## transform the weight var. // Attaching weights to DF // sorting for GEE models
dat$weights = fit$weights




## Recode Before modeling
dat$clien1dummy <- as.numeric(dat$clien1dummy)
library(car)
dat$clien1dummy <- recode(dat$clien1dummy, "1 = 0 ; 2 = 1")


# models
set.seed(602); options(scipen=999)


gee.cont.rgps.1.t = extract.geepack(gee.cont.rgps.1 <- geeglm(m1.r,
                                                        family = binomial(link = "logit"), 
                                                        id = municipality, 
                                                        weights = wt,
                                                        std.err = "san.se",
                                                        corstr = "independence",
                                                        data = dat))


gee.cont.rgps.2.t = extract.geepack(gee.cont.rgps.2 <- geeglm(m2.r,
                                                        family = binomial(link = "logit"), 
                                                        id = municipality, 
                                                        weights = wt,
                                                        std.err = "san.se",
                                                        corstr = "independence",
                                                        data = dat))


gee.cont.rgps.3.t = extract.geepack(gee.cont.rgps.3 <- geeglm(m3.r,
                                                        family = binomial(link = "logit"), 
                                                        id = municipality, 
                                                        weights = wt,
                                                        std.err = "san.se",
                                                        corstr = "independence",
                                                        data = dat))





#####################################################################
### C O E F F I C I E N T   P L O T 
#####################################################################



# 90% CIs: 1.645
# 95% CIs: 1.96



cem.plot = data.frame(
  Coefficients = as.numeric(c(gee.dich.m.1$"coefficients", gee.dich.m.2$"coefficients", gee.dich.m.3$"coefficients")),
  Covariate = as.character(c(
    "Intercept", "Poverty Density", "Wealth Index", "Education Years", "Poverty Density * \n Wealth Index", 
    "Intercept", "Poverty Density", "Wealth Index", "Urban", "Municipal Population", "Municipal Opposition", "Wealth Index * \n Municipal Opposition", "Poverty Density * \n Municipal Opposition", 
    "Intercept", "Political Involvement Index", "Wealth Index", "Poverty Density", "Support for Democracy", "Party Id.", "Perception of Corruption", "Political Involvement *\n Wealth Index", "Political Involvement *\n Poverty Density")),
  Model = as.character(c(rep("Economic", 5), rep("Contextual", 8), rep("Political", 9))),
  se = c(as.numeric(c(sqrt(diag(gee.dich.m.1$geese$vbeta)))), as.numeric(sqrt(diag(gee.dich.m.2$geese$vbeta))), as.numeric(sqrt(diag(gee.dich.m.3$geese$vbeta)))),
  upper = c(as.numeric(gee.dich.m.1$"coefficients")  + 1.96*sqrt(diag(gee.dich.m.1$geese$vbeta)),
            as.numeric(gee.dich.m.2$"coefficients") + 1.96*sqrt(diag(gee.dich.m.2$geese$vbeta)),
            as.numeric(gee.dich.m.3$"coefficients") + 1.96*sqrt(diag(gee.dich.m.3$geese$vbeta))),
  lower = c(as.numeric(gee.dich.m.1$"coefficients")  - 1.96*sqrt(diag(gee.dich.m.1$geese$vbeta)),
            as.numeric(gee.dich.m.2$"coefficients") - 1.96*sqrt(diag(gee.dich.m.2$geese$vbeta)),
            as.numeric(gee.dich.m.3$"coefficients") - 1.96*sqrt(diag(gee.dich.m.3$geese$vbeta))),
  Balancing = rep(as.character("CEM Matching"), as.numeric(5+8+9))
)
#cem.plot<-cem.plot[!(cem.plot$Covariate=="Wealth Index" | cem.plot$Covariate=="Poverty Density" | cem.plot$Covariate=="Political Involvement Index"),]




gps.plot = data.frame(
  Coefficients = as.numeric(c(gee.cont.rgps.1$"coefficients", gee.cont.rgps.2$"coefficients", gee.cont.rgps.3$"coefficients")),
  Covariate = as.character(c(
    "Intercept", "Poverty Density", "Wealth Index", "Education Years", "Weights", "Poverty Density * \n Wealth Index", 
    "Intercept", "Poverty Density", "Wealth Index", "Urban", "Municipal Population", "Municipal Opposition", "Wealth Index * \n Municipal Opposition", "Weights", "Poverty Density * \n Municipal Opposition", 
    "Intercept", "Political Involvement Index", "Wealth Index", "Poverty Density", "Support for Democracy", "Party Id.", "Perception of Corruption", "Weights", "Political Involvement *\n Wealth Index", "Political Involvement *\n Poverty Density")),
  Model = as.character(c(rep("Economic", 6), rep("Contextual", 9), rep("Political", 10))),
  se = c(as.numeric(c(sqrt(diag(gee.cont.rgps.1$geese$vbeta)))), as.numeric(sqrt(diag(gee.cont.rgps.2$geese$vbeta))), as.numeric(sqrt(diag(gee.cont.rgps.3$geese$vbeta)))),
  upper = c(as.numeric(gee.cont.rgps.1$"coefficients")  + 1.96*sqrt(diag(gee.cont.rgps.1$geese$vbeta)),
            as.numeric(gee.cont.rgps.2$"coefficients") + 1.96*sqrt(diag(gee.cont.rgps.2$geese$vbeta)),
            as.numeric(gee.cont.rgps.3$"coefficients") + 1.96*sqrt(diag(gee.cont.rgps.3$geese$vbeta))),
  lower = c(as.numeric(gee.cont.rgps.1$"coefficients")  - 1.96*sqrt(diag(gee.cont.rgps.1$geese$vbeta)),
            as.numeric(gee.cont.rgps.2$"coefficients") - 1.96*sqrt(diag(gee.cont.rgps.2$geese$vbeta)),
            as.numeric(gee.cont.rgps.3$"coefficients") - 1.96*sqrt(diag(gee.cont.rgps.3$geese$vbeta))),
  Balancing = rep(as.character("GPS Weighting"), as.numeric(6+9+10))
)

#gps.plot<-gps.plot[!(gps.plot$Covariate=="Wealth Index" | gps.plot$Covariate=="Poverty Density" | gps.plot$Covariate=="Political Involvement Index"),]


# cbind these two datasets
gee.plot = rbind(cem.plot, gps.plot)

# delete "non" important estimations
gee.plot<-gee.plot[!(gee.plot$Covariate=="Intercept" | gee.plot$Covariate=="Weights"),]




# Plot
library(ggplot2)
ggplot(gee.plot, aes(
  x = Covariate, 
  y = Coefficients, 
  ymin = upper, 
  ymax = lower,
  #colour = Model,
  shape = Balancing,
  position="dodge"
)) +
  geom_pointrange(position=position_dodge(width=0.8), fill = NA) + 
  geom_hline(yintercept = 0, alpha = 1/3, colour = gray(1/2), lty = 2) +
  coord_flip() + 
  xlab("") + 
  ylab("Coefficients (logit scale)") +
  ggtitle("Likelihood of Clientelism") +
  #guides(colour=FALSE) +
  #theme(legend.position="none") + 
  theme_bw() +
  facet_grid(Model ~ .) +
  #labs(colour = "Sample") +
  theme(legend.key = element_rect(colour = NA, fill = NA, size = 0.5)) 


## TEST AREA BELOW
economic.d = subset(gee.plot, gee.plot$Model=="Economic")
contextual.d = subset(gee.plot, gee.plot$Model=="Contextual")
political.d = subset(gee.plot, gee.plot$Model=="Political")

# Plot
library(ggplot2)

### ecomomic
economic.plot = ggplot(economic.d, aes(
  x = Covariate, 
  y = Coefficients, 
  ymin = upper, 
  ymax = lower,
  #colour = Model,
  colour = Balancing,
  position="dodge"
)) +
  geom_pointrange(position=position_dodge(width=0.3), fill = NA) + 
  geom_hline(yintercept = 0, alpha = 1/3, colour = gray(1/2), lty = 2) +
  coord_flip() + 
  xlab("") + 
  ylab("Coefficients (logit scale)") +
  ggtitle("Likelihood of Clientelism: Economic Model") +
  #guides(colour=FALSE) +
  #theme(legend.position="none") + 
  theme_bw() +
  #labs(colour = "Sample") +
  theme(legend.key = element_rect(colour = NA, fill = NA, size = 0.5)) 

### contextual
contextual.plot = ggplot(contextual.d, aes(
  x = Covariate, 
  y = Coefficients, 
  ymin = upper, 
  ymax = lower,
  #colour = Model,
  colour = Balancing,
  position="dodge"
)) +
  geom_pointrange(position=position_dodge(width=0.3), fill = NA) + 
  geom_hline(yintercept = 0, alpha = 1/3, colour = gray(1/2), lty = 2) +
  coord_flip() + 
  xlab("") + 
  ylab("Coefficients (logit scale)") +
  ggtitle("Likelihood of Clientelism: Contextual Model") +
  #guides(colour=FALSE) +
  #theme(legend.position="none") + 
  theme_bw() +
  #labs(colour = "Sample") +
  theme(legend.key = element_rect(colour = NA, fill = NA, size = 0.5)) 


### political
political.plot = ggplot(political.d, aes(
  x = Covariate, 
  y = Coefficients, 
  ymin = upper, 
  ymax = lower,
  #colour = Model,
  colour = Balancing,
  position="dodge"
)) +
  geom_pointrange(position=position_dodge(width=0.3), fill = NA) + 
  geom_hline(yintercept = 0, alpha = 1/3, colour = gray(1/2), lty = 2) +
  coord_flip() + 
  xlab("") + 
  ylab("Coefficients (logit scale)") +
  ggtitle("Likelihood of Clientelism: Political Model") +
  #guides(colour=FALSE) +
  #theme(legend.position="none") + 
  theme_bw() +
  #labs(colour = "Sample") +
  theme(legend.key = element_rect(colour = NA, fill = NA, size = 0.5)) 


library(cowplot) # install.packages("cowplot")
plot_grid(political.plot, economic.plot, contextual.plot,  nrow = 3)

#####################################################################
### T A B L E S :   G P S   A N D   M A T C H E D   S A M P L E S 
#####################################################################




# TABLE
library(texreg)
screenreg(list(gee.dich.m.1.t,gee.dich.m.2.t,gee.dich.m.3.t), # screenreg / texreg
          custom.coef.names = c(# this gotta be before OMIT.COEFF
          "(Intercept)",
          "Size of the Poor",
          "Wealth Index",
          "Education Years",
          "Size of the Poor * Wealth Index",
          "Urban",
          "Municipal Opposition",
          "Political Involvement",
          "Support for Democracy",
          "Perception of Corruption",
          "Party Id.",
          "Political Involvement * Wealth Index",
          "Political Involvement * Size of the Poor"),
          caption = "Likelihood of Clientelism: Logit GEE Models with Coarsened Exact Match Sample",
          label = "gee:cem:dich:1",
          ci.force = T,
          override.ci.low = list(
            c(cem.plot$lower[cem.plot$Balancing=="CEM Matching"][1],
              cem.plot$lower[cem.plot$Balancing=="CEM Matching"][2],
              cem.plot$lower[cem.plot$Balancing=="CEM Matching"][3],
              cem.plot$lower[cem.plot$Balancing=="CEM Matching"][4], 
              cem.plot$lower[cem.plot$Balancing=="CEM Matching"][5]),
            c(cem.plot$lower[cem.plot$Balancing=="CEM Matching"][6],
              cem.plot$lower[cem.plot$Balancing=="CEM Matching"][7],
              cem.plot$lower[cem.plot$Balancing=="CEM Matching"][8],
              cem.plot$lower[cem.plot$Balancing=="CEM Matching"][9],
              cem.plot$lower[cem.plot$Balancing=="CEM Matching"][10]),
            c(cem.plot$lower[cem.plot$Balancing=="CEM Matching"][11],
              cem.plot$lower[cem.plot$Balancing=="CEM Matching"][12],
              cem.plot$lower[cem.plot$Balancing=="CEM Matching"][13],
              cem.plot$lower[cem.plot$Balancing=="CEM Matching"][14],
              cem.plot$lower[cem.plot$Balancing=="CEM Matching"][15],
              cem.plot$lower[cem.plot$Balancing=="CEM Matching"][16],
              cem.plot$lower[cem.plot$Balancing=="CEM Matching"][17],
              cem.plot$lower[cem.plot$Balancing=="CEM Matching"][18],
              cem.plot$lower[cem.plot$Balancing=="CEM Matching"][19],
              cem.plot$lower[cem.plot$Balancing=="CEM Matching"][20])),
          override.ci.up =  list(
            c(cem.plot$upper[cem.plot$Balancing=="CEM Matching"][1],
              cem.plot$upper[cem.plot$Balancing=="CEM Matching"][2],
              cem.plot$upper[cem.plot$Balancing=="CEM Matching"][3],
              cem.plot$upper[cem.plot$Balancing=="CEM Matching"][4],
              cem.plot$upper[cem.plot$Balancing=="CEM Matching"][5]),
            c(cem.plot$upper[cem.plot$Balancing=="CEM Matching"][6],
              cem.plot$upper[cem.plot$Balancing=="CEM Matching"][7],
              cem.plot$upper[cem.plot$Balancing=="CEM Matching"][8],
              cem.plot$upper[cem.plot$Balancing=="CEM Matching"][9],
              cem.plot$upper[cem.plot$Balancing=="CEM Matching"][10]),
            c(cem.plot$upper[cem.plot$Balancing=="CEM Matching"][11],
              cem.plot$upper[cem.plot$Balancing=="CEM Matching"][12],
              cem.plot$upper[cem.plot$Balancing=="CEM Matching"][13],
              cem.plot$upper[cem.plot$Balancing=="CEM Matching"][14],
              cem.plot$upper[cem.plot$Balancing=="CEM Matching"][15],
              cem.plot$upper[cem.plot$Balancing=="CEM Matching"][16],
              cem.plot$upper[cem.plot$Balancing=="CEM Matching"][17],
              cem.plot$upper[cem.plot$Balancing=="CEM Matching"][18],
              cem.plot$upper[cem.plot$Balancing=="CEM Matching"][19],
              cem.plot$upper[cem.plot$Balancing=="CEM Matching"][20])),
          stars = numeric(0),
          custom.model.names = c("Economic", "Contextual", "Political"),
          digits = 2,
          custom.note = "Logit GEE models with clustered std. errors at the municipality level. \n Matched sample using the CEM algorithm. \n Dichotomous treatment variable (cutoff at the median).\n 95% Confidence intervals in parentheses.",
          fontsize = "scriptsize",
          float.pos = "h"
          )




# TABLE
library(texreg)
screenreg(list(gee.cont.rgps.1.t,gee.cont.rgps.2.t,gee.cont.rgps.3.t), # screenreg / texreg
          custom.coef.names = c(# this gotta be before OMIT.COEFF
          "(Intercept)",
          "Size of the Poor",
          "Wealth Index",
          "Education Years",
           "weights",
             "Size of the Poor * Wealth Index",
             "Urban",
             "Municipal Opposition",
             "Political Involvement",
             "Support for Democracy",
             "Perception of Corruption",
          "Party Id.",
          "Political Involvement * Wealth Index",
          "Political Involvement * Size of the Poor"),
          caption = "Likelihood of Clientelism: Generalized Propensity Score Weighted Logit GEE Models",
          omit.coef = "weights",
          ci.force = T,
          #override.pvalues = list(c(1,1,1,1), c(1,1,1,1,1), c(1,1,1,1,1,1)),
          #override.se = list(c(1,1,1,1), c(1,1,1,1,1), c(1,1,1,1,1,1)),
          override.ci.low = list(
            c(gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][1],
              gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][2],
              gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][3],
              gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][4],
              gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][5],
              gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][6]),
            c(gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][7],
              gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][8],
              gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][9],
              gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][10],
              gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][11],
              gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][12]),
            c(gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][13],
              gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][14],
              gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][15],
              gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][16],
              gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][17],
              gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][18],
              gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][19],
              gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][20],
              gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][21],
              gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][22],
              gps.plot$lower[gps.plot$Balancing=="GPS Weighting"][23])),
          override.ci.up =  list(
            c(gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][1],
              gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][2],
              gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][3],
              gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][4],
              gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][5],
              gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][6]),
            c(gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][7],
              gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][8],
              gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][9],
              gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][10],
              gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][11],
              gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][12]),
            c(gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][13],
              gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][14],
              gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][15],
              gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][16],
              gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][17],
              gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][19],
              gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][19],
              gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][20],
              gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][21],
              gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][22],
              gps.plot$upper[gps.plot$Balancing=="GPS Weighting"][23])),
          label = "gee:gps:cont:1",
          digits = 2,
          custom.note = "Logit GEE models with clustered std. errors at the municipality level. \n Raw sample weighted by the generalized propensity score. GPS vector omited. \n Continuous treatment variable (no cutoffs were used).\n 95% Confidence intervals in parentheses.",
          fontsize = "scriptsize",
          stars = as.numeric(0),
          custom.model.names = c("Economic", "Contextual", "Political"),
          float.pos = "h"
)





#####################################################################
### S I M U L A T I O N S:                            M  O D E L S
#####################################################################

# load data
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")


# METHOD 2
# library(devtools) # install.packages("devtools")
# install_github('IQSS/Zelig')
#library(Zelig) # install.packages("Zelig", dependencies=TRUE) # Models

library(Zelig)
gee.dich.m.1.s = zelig(m1.m, 
                       model = "logit.gee",
                       id = "municipality", 
                       weights = "wt",
                       std.err = "san.se",
                       corstr = "independence",
                       data = m.data,
                       cite = F)

gee.dich.m.2.s = zelig(m2.m, 
                       model = "logit.gee",
                       id = "municipality", 
                       weights = "wt",
                       std.err = "san.se",
                       corstr = "independence",
                       data = m.data,
                       cite = F)

gee.dich.m.3.s = zelig(m3.m, 
              model = "logit.gee",
              id = "municipality", 
              weights = "wt",
              std.err = "san.se",
              corstr = "independence",
              data = m.data,
              cite = F)




#####################################################################
### S I M U L A T I O N S:      D  I S T R I B U T I O N   P L O T S 
#####################################################################

set.seed(602); options(scipen=999)


# simulation DISTRIBUTION PLOTS
## m1 
gee.1.m.zelig.low = data.frame(Low = sim(x = setx(gee.dich.m.1.s, cond = T, large = min(m.data$large)), num=10000)$getqi(qi="ev"))
gee.1.m.zelig.high = data.frame(High = sim(x = setx(gee.dich.m.1.s, cond = T, large = max(m.data$large)), num=10000)$getqi(qi="ev"))
## m2
gee.2.m.zelig.low = data.frame(Low = sim(x = setx(gee.dich.m.2.s, cond = T, large = min(m.data$large)), num=10000)$getqi(qi="ev"))
gee.2.m.zelig.high = data.frame(High = sim(x = setx(gee.dich.m.2.s, cond = T, large = max(m.data$large)), num=10000)$getqi(qi="ev"))
## m3
gee.3.m.zelig.low = data.frame(Low = sim(x = setx(gee.dich.m.3.s, cond = T, large = min(m.data$large)), num=10000)$getqi(qi="ev"))
gee.3.m.zelig.high = data.frame(High = sim(x = setx(gee.dich.m.3.s, cond = T, large = max(m.data$large)), num=10000)$getqi(qi="ev"))





# plot
library(ggplot2);library(grid)

large.m1=ggplot() + geom_density(aes(x=Low, fill="Low"), data= gee.1.m.zelig.low, alpha = .2) + 
  geom_density(aes(x=High, fill="High"), data= gee.1.m.zelig.high, alpha = .2) + 
  xlab("Expected Value \n of Clientelism") + ylab("Estimated Density") + 
  theme_bw() + 
  ggtitle("Economic") +
  theme(
    legend.key = 
      element_rect(colour = NA, fill = NA, size = 0.5), 
    panel.margin = unit(0, "lines"), 
    axis.title.x = element_text(colour = "white")) + 
  scale_fill_discrete(guide = guide_legend(title = "Density of the Poor")) +
  xlim(0, .4) +  ylim(0,20) + coord_fixed(ratio = 0.4/45) +
  guides(fill=FALSE)

large.m2=ggplot() + geom_density(aes(x=Low, fill="Low"), data= gee.2.m.zelig.low, alpha = .2) + 
  geom_density(aes(x=High, fill="High"), data= gee.2.m.zelig.high, alpha = .2) + 
  ylab("") + xlab("Expected Value \n of Clientelism") +
  theme_bw() + 
  ggtitle("Contextual") +
  theme(
    legend.key = element_rect(colour = NA, fill = NA, size = 0.5),
    panel.margin = unit(0, "lines"),
    axis.title.x = element_text(colour = "black")) + 
  scale_fill_discrete(guide = guide_legend(title = "Density of the Poor")) + 
  xlim(0, .4) +  ylim(0,20) + coord_fixed(ratio = 0.4/45) +
  guides(fill=FALSE)

large.m3= ggplot() + geom_density(aes(x=Low, fill="Low"), data= gee.3.m.zelig.low, alpha = .2) + 
  geom_density(aes(x=High, fill="High"), data= gee.3.m.zelig.high, alpha = .2) + 
  ylab("") + xlab("Expected Value \n of Clientelism") +
  theme_bw() + 
  ggtitle("Political") +
  xlim(0, .4) +  ylim(0,20) + coord_fixed(ratio = 0.4/45) +
  theme(
    legend.key = element_rect(colour = NA, fill = NA, size = 0.5), 
    legend.position=c(-.9, -.85),
    legend.key.height=unit(0.5,"line"),
    legend.key.width=unit(0.5,"line"),
    panel.margin = unit(0, "lines"),
    axis.title.x = element_text(colour = "white"),
    legend.justification="center") + 
  scale_fill_discrete(guide = guide_legend(title = "Density of the Poor",
                                           title.position = "left",
                                           direction = "horizontal"))


library(cowplot) # install.packages("cowplot")
plot_grid(large.m1,large.m2,large.m3, nrow = 1, align = "v", scale = 1)


##########################################################################
### S I M U L A T I O N S:      D  I S T R I B U T I O N   P L O T S  I I 
##########################################################################



##########################
##### BY INCOME
##########################


set.seed(602); options(scipen=999)


# simulation DISTRIBUTION PLOTS
high.poor = data.frame(group = rep("Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.1.s, cond = TRUE,large = max(m.data$large), wealth= min(m.data$wealth)), num=1000000)$getqi(qi="ev"))
high.rich = data.frame(group = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.1.s, cond = TRUE,large = max(m.data$large), wealth= max(m.data$wealth)), num=1000000)$getqi(qi="ev"))
low.poor = data.frame(group = rep("Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.1.s, cond = TRUE,large = min(m.data$large), wealth= min(m.data$wealth)), num=1000000)$getqi(qi="ev"))
low.rich = data.frame(group = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.1.s, cond = TRUE,large = min(m.data$large), wealth= max(m.data$wealth)), num=1000000)$getqi(qi="ev"))

# plot
ggplot() + 
  geom_density(aes(x=x, fill="High Density"), data= high.poor, alpha = .2) + 
  geom_density(aes(x=x, fill="High Density"), data= high.rich, alpha = .2) + 
  geom_density(aes(x=x, fill="Low Density"), data= low.poor, alpha = .2) + 
  geom_density(aes(x=x, fill="Low Density"), data= low.rich, alpha = .2) + 
  ylab("Expected Density") + xlab("Expected Value of Clientelism") +
  theme_bw() +
  scale_fill_discrete(guide = guide_legend(title = "Density of the Poor")) +
  facet_grid(.~group)
  


## t test on these distributions
t.test(high.poor$x, low.poor$x,conf.level = 0.95) # significative pvalue = significantly different
t.test(high.rich$x, low.rich$x, conf.level = 0.95) # significative pvalue = significantly different


##########################
##### BY Competition and Income
##########################

set.seed(602); options(scipen=999)

# simulation DISTRIBUTION PLOTS
high.poor.lowcomp = data.frame(competition = rep("Low Competition", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = max(m.data$large), wealth= quantile(m.data$wealth, .25), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.poor.highcomp = data.frame(competition = rep("High Competition", 1000000),income = rep("Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = max(m.data$large), wealth= quantile(m.data$wealth, .25), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.lowcomp = data.frame(competition = rep("Low Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = max(m.data$large), wealth= quantile(m.data$wealth, .75), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.highcomp = data.frame(competition = rep("High Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = max(m.data$large), wealth= quantile(m.data$wealth, .75), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.lowcomp = data.frame(competition = rep("Low Competition", 1000000),income = rep("Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = min(m.data$large), wealth= quantile(m.data$wealth, .25), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.highcomp = data.frame(competition = rep("High Competition", 1000000),income = rep("Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = min(m.data$large), wealth= quantile(m.data$wealth, .25), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.lowcomp = data.frame(competition = rep("Low Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = min(m.data$large), wealth= quantile(m.data$wealth, .75), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.highcomp = data.frame(competition = rep("High Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = min(m.data$large), wealth= quantile(m.data$wealth, .75), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))


# plot
library(ggplot2)
ggplot() + 
  geom_density(aes(x=x, fill="High Density"), data= high.poor.lowcomp, alpha = .2) + 
  geom_density(aes(x=x, fill="High Density"), data= high.poor.highcomp, alpha = .2) + 
  geom_density(aes(x=x, fill="High Density"), data= high.rich.lowcomp, alpha = .2) + 
  geom_density(aes(x=x, fill="High Density"), data= high.rich.highcomp, alpha = .2) + 
  geom_density(aes(x=x, fill="Low Density"), data= low.poor.lowcomp, alpha = .2) + 
  geom_density(aes(x=x, fill="Low Density"), data= low.poor.highcomp, alpha = .2) + 
  geom_density(aes(x=x, fill="Low Density"), data= low.rich.lowcomp, alpha = .2) + 
  geom_density(aes(x=x, fill="Low Density"), data= low.rich.highcomp, alpha = .2) + 
  ylab("Estimated Density") + xlab("Expected Value of Clientelism") +
  theme_bw() +
  scale_fill_discrete(guide = guide_legend(title = "Density of the Poor")) +
  facet_grid(competition~income, scales ="free")

## t test on these distributions
### 1
t.test(high.poor.lowcomp$x, low.poor.lowcomp$x,conf.level = 0.99) # significative pvalue = significantly different
### 2
t.test(high.rich.lowcomp$x, low.rich.lowcomp$x,conf.level = 0.99) # significative pvalue = significantly different
### 3
t.test(high.poor.lowcomp$x, low.poor.lowcomp$x,conf.level = 0.99) # significative pvalue = significantly different
### 4
t.test(high.rich.highcomp$x, low.rich.highcomp$x,conf.level = 0.99) # significative pvalue = significantly different






### MAKE A TABLE IN RNW using this sequence.
t = t.test(high.rich.highcomp$x, low.rich.highcomp$x,conf.level = 0.99) # significative pvalue = significantly different
as.numeric(t$estimate[2])



##########################
###### By Density, Income, COmpetition and Pop Size //
##########################
### PUT IN APPENDIX: SAY THAT I DIDNT FIND SUPPORT //
### ACTUALLY LARGER POPULATION< MORE CLIENTELISM // ATTENTION THIS IS **NOT** RUEDA'S argument

set.seed(602); options(scipen=999)


# simulation DISTRIBUTION PLOTS // low pop
high.poor.lowcomp.lowpop = data.frame(competition = rep("Low Competition", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = max(m.data$large), pop.10.m = min(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .25), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.poor.highcomp.lowpop = data.frame(competition = rep("High Competition", 1000000),income = rep("Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = max(m.data$large), pop.10.m = min(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .25), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.lowcomp.lowpop = data.frame(competition = rep("Low Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = max(m.data$large), pop.10.m = min(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .75), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.highcomp.lowpop = data.frame(competition = rep("High Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = max(m.data$large), pop.10.m = min(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .75), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.lowcomp.lowpop = data.frame(competition = rep("Low Competition", 1000000),income = rep("Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = min(m.data$large), pop.10.m = min(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .25), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.highcomp.lowpop = data.frame(competition = rep("High Competition", 1000000),income = rep("Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = min(m.data$large), pop.10.m = min(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .25), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.lowcomp.lowpop = data.frame(competition = rep("Low Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = min(m.data$large), pop.10.m = min(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .75), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.highcomp.lowpop = data.frame(competition = rep("High Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = min(m.data$large), pop.10.m = min(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .75), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))



# simulation DISTRIBUTION PLOTS // high pop
high.poor.lowcomp.highpop = data.frame(competition = rep("Low Competition", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = max(m.data$large), pop.10.m = max(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .25), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.poor.highcomp.highpop = data.frame(competition = rep("High Competition", 1000000),income = rep("Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = max(m.data$large), pop.10.m = max(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .25), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.lowcomp.highpop = data.frame(competition = rep("Low Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = max(m.data$large), pop.10.m = max(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .75), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.highcomp.highpop = data.frame(competition = rep("High Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = max(m.data$large), pop.10.m = max(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .75), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.lowcomp.highpop = data.frame(competition = rep("Low Competition", 1000000),income = rep("Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = min(m.data$large), pop.10.m = max(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .25), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.highcomp.highpop = data.frame(competition = rep("High Competition", 1000000),income = rep("Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = min(m.data$large), pop.10.m = max(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .25), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.lowcomp.highpop = data.frame(competition = rep("Low Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = min(m.data$large), pop.10.m = max(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .75), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.highcomp.highpop = data.frame(competition = rep("High Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(gee.dich.m.2.s, cond = TRUE,large = min(m.data$large), pop.10.m = max(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .75), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))



# plot 1 //
library(ggplot2)
p1 = ggplot() + 
  geom_density(aes(x=x, fill="High Density"), data= high.poor.lowcomp.lowpop, alpha = .2) +
  geom_density(aes(x=x, fill="High Density"), data= high.poor.highcomp.lowpop, alpha = .2) +
  geom_density(aes(x=x, fill="High Density"), data= high.rich.lowcomp.lowpop, alpha = .2) +
  geom_density(aes(x=x, fill="High Density"), data= high.rich.highcomp.lowpop, alpha = .2) +
  geom_density(aes(x=x, fill="Low Density"), data= low.poor.lowcomp.lowpop, alpha = .2) +
  geom_density(aes(x=x, fill="Low Density"), data= low.poor.highcomp.lowpop, alpha = .2) +
  geom_density(aes(x=x, fill="Low Density"), data= low.rich.lowcomp.lowpop, alpha = .2) +
  geom_density(aes(x=x, fill="Low Density"), data= low.rich.highcomp.lowpop, alpha = .2) +
  ylab("Expected Density: Large Pop. Size") + xlab("Expected Value of Clientelism") +
  theme_bw() +
  scale_fill_discrete(guide = guide_legend(title = "Density of the Poor")) +
  facet_grid(competition~income)

# plot 2 //
library(ggplot2)
p2 = ggplot() + 
  geom_density(aes(x=x, fill="High Density"), data= high.poor.lowcomp.highpop, alpha = .2) +
  geom_density(aes(x=x, fill="High Density"), data= high.poor.highcomp.highpop, alpha = .2) +
  geom_density(aes(x=x, fill="High Density"), data= high.rich.lowcomp.highpop, alpha = .2) +
  geom_density(aes(x=x, fill="High Density"), data= high.rich.highcomp.highpop, alpha = .2) +
  geom_density(aes(x=x, fill="Low Density"), data= low.poor.lowcomp.highpop, alpha = .2) +
  geom_density(aes(x=x, fill="Low Density"), data= low.poor.highcomp.highpop, alpha = .2) +
  geom_density(aes(x=x, fill="Low Density"), data= low.rich.lowcomp.highpop, alpha = .2) +
  geom_density(aes(x=x, fill="Low Density"), data= low.rich.highcomp.highpop, alpha = .2) +
  ylab("Expected Density: Small Pop. Size") + xlab("Expected Value of Clientelism") +
  theme_bw() +
  scale_fill_discrete(guide = guide_legend(title = "Density of the Poor")) +
  facet_grid(competition~income)


library(cowplot) # install.packages("cowplot")
plot_grid(p1,p2, ncol = 1, align = "v", scale = 1)



##########################
###### By Density, Income, COmpetition and Dem Values //
##########################

gee.dich.m.3.s.TEST = zelig(clien1dummy ~  wealth + large + munopp + ing4 + vb3 + exc7 + large*wealth*ing4,
                       model = "logit.gee",
                       id = "municipality", 
                       weights = "wt",
                       std.err = "san.se",
                       corstr = "independence",
                       data = m.data,
                       cite = F)


set.seed(602); options(scipen=999)
# simulation DISTRIBUTION PLOTS //
high.poor.highcomp.dem =  data.frame(dem = rep("Democratic", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.3.s.TEST, cond = TRUE, large = max(m.data$large), wealth= quantile(m.data$wealth, .25), ing4 = max(m.data$ing4),munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.highcomp.dem =  data.frame(dem = rep("Democratic", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.3.s.TEST, cond = TRUE, large = min(m.data$large), wealth= quantile(m.data$wealth, .25), ing4 = max(m.data$ing4),munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.highcomp.dem =  data.frame(dem = rep("Democratic", 1000000), income = rep("Non-Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.3.s.TEST, cond = TRUE, large = max(m.data$large), wealth= quantile(m.data$wealth, .75), ing4 = max(m.data$ing4),munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.highcomp.dem =  data.frame(dem = rep("Democratic", 1000000), income = rep("Non-Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.3.s.TEST, cond = TRUE, large = min(m.data$large), wealth= quantile(m.data$wealth, .75), ing4 = max(m.data$ing4),munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.poor.highcomp.nondem =  data.frame(dem = rep("Non-Democratic", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.3.s.TEST, cond = TRUE, large = max(m.data$large), wealth= quantile(m.data$wealth, .25), ing4 = min(m.data$ing4),munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.highcomp.nondem =  data.frame(dem = rep("Non-Democratic", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.3.s.TEST, cond = TRUE, large = min(m.data$large), wealth= quantile(m.data$wealth, .25), ing4 = min(m.data$ing4),munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.highcomp.nondem =  data.frame(dem = rep("Non-Democratic", 1000000), income = rep("Non-Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.3.s.TEST, cond = TRUE, large = max(m.data$large), wealth= quantile(m.data$wealth, .75), ing4 = min(m.data$ing4),munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.highcomp.nondem =  data.frame(dem = rep("Non-Democratic", 1000000), income = rep("Non-Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.3.s.TEST, cond = TRUE, large = min(m.data$large), wealth= quantile(m.data$wealth, .75), ing4 = min(m.data$ing4),munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))


high.poor.lowcomp.dem =  data.frame(dem = rep("Democratic", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.3.s.TEST, cond = TRUE, large = max(m.data$large), wealth= quantile(m.data$wealth, .25), ing4 = max(m.data$ing4),munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.lowcomp.dem =  data.frame(dem = rep("Democratic", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.3.s.TEST, cond = TRUE, large = min(m.data$large), wealth= quantile(m.data$wealth, .25), ing4 = max(m.data$ing4),munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.lowcomp.dem =  data.frame(dem = rep("Democratic", 1000000), income = rep("Non-Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.3.s.TEST, cond = TRUE, large = max(m.data$large), wealth= quantile(m.data$wealth, .75), ing4 = max(m.data$ing4),munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.lowcomp.dem =  data.frame(dem = rep("Democratic", 1000000), income = rep("Non-Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.3.s.TEST, cond = TRUE, large = min(m.data$large), wealth= quantile(m.data$wealth, .75), ing4 = max(m.data$ing4),munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.poor.lowcomp.nondem =  data.frame(dem = rep("Non-Democratic", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.3.s.TEST, cond = TRUE, large = max(m.data$large), wealth= quantile(m.data$wealth, .25), ing4 = min(m.data$ing4),munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.lowcomp.nondem =  data.frame(dem = rep("Non-Democratic", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.3.s.TEST, cond = TRUE, large = min(m.data$large), wealth= quantile(m.data$wealth, .25), ing4 = min(m.data$ing4),munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.lowcomp.nondem =  data.frame(dem = rep("Non-Democratic", 1000000), income = rep("Non-Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.3.s.TEST, cond = TRUE, large = max(m.data$large), wealth= quantile(m.data$wealth, .75), ing4 = min(m.data$ing4),munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.lowcomp.nondem =  data.frame(dem = rep("Non-Democratic", 1000000), income = rep("Non-Poor Individuals", 1000000), x = sim(x = setx(gee.dich.m.3.s.TEST, cond = TRUE, large = min(m.data$large), wealth= quantile(m.data$wealth, .75), ing4 = min(m.data$ing4),munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))



# plot 1 //
library(ggplot2)
p1=ggplot() + 
  geom_density(aes(x=x, fill="High Density"), data= high.poor.highcomp.dem, alpha = .2) +
  geom_density(aes(x=x, fill="High Density"), data= high.rich.highcomp.dem, alpha = .2) +
  geom_density(aes(x=x, fill="High Density"), data= high.poor.highcomp.nondem, alpha = .2) +
  geom_density(aes(x=x, fill="High Density"), data= high.rich.highcomp.nondem, alpha = .2) +
  geom_density(aes(x=x, fill="Low Density"), data= low.poor.highcomp.dem, alpha = .2) +
  geom_density(aes(x=x, fill="Low Density"), data= low.poor.highcomp.nondem, alpha = .2) +
  geom_density(aes(x=x, fill="Low Density"), data= low.rich.highcomp.dem, alpha = .2) +
  geom_density(aes(x=x, fill="Low Density"), data= low.rich.highcomp.nondem, alpha = .2) +
  ylab("Expected Density: High Competition") + xlab("Expected Value of Clientelism") +
  theme_bw() +
  scale_fill_discrete(guide = guide_legend(title = "Density of the Poor")) +
  facet_grid(dem~income)

# plot 2 //
library(ggplot2)
p2=ggplot() + 
  geom_density(aes(x=x, fill="High Density"), data= high.poor.lowcomp.dem, alpha = .2) +
  geom_density(aes(x=x, fill="High Density"), data= high.poor.lowcomp.nondem, alpha = .2) +
  geom_density(aes(x=x, fill="High Density"), data= high.rich.lowcomp.dem, alpha = .2) +
  geom_density(aes(x=x, fill="High Density"), data= high.rich.lowcomp.nondem, alpha = .2) +
  geom_density(aes(x=x, fill="Low Density"), data= low.poor.lowcomp.nondem, alpha = .2) +
  geom_density(aes(x=x, fill="Low Density"), data= low.poor.lowcomp.dem, alpha = .2) +
  geom_density(aes(x=x, fill="Low Density"), data= low.rich.lowcomp.dem, alpha = .2) +
  geom_density(aes(x=x, fill="Low Density"), data= low.rich.lowcomp.nondem, alpha = .2) +
  ylab("Expected Density: Low Competition") + xlab("Expected Value of Clientelism") +
  theme_bw() +
  scale_fill_discrete(guide = guide_legend(title = "Density of the Poor")) +
  facet_grid(dem~income)

library(cowplot) # install.packages("cowplot")
plot_grid(p1,p2, ncol = 1, align = "v", scale = 1)


#####################################################################
### S I M U L A T I O N S:      I N T E R A C T I O N   P L O T S 
#####################################################################


##########################
#  LARGE * POP: gee.dich.m.2.s
##########################




## low 
gee.dich.m.2.s.low = data.frame(
  sim(x = setx(gee.dich.m.2.s, cond = TRUE,
               large = min(m.data$large), 
               pop.10.m = min(m.data$pop.10.m):max(m.data$pop.10.m)), 
      num=300)$getqi(qi="ev", xvalue="range"))
colnames(gee.dich.m.2.s.low) <- seq(1:ncol(as.data.frame(t(min(m.data$pop.10.m):max(m.data$pop.10.m)))))  # low


## high
gee.dich.m.2.high = data.frame(
  sim(x = setx(gee.dich.m.2.s, cond = TRUE,
               large = max(m.data$large), 
               pop.10.m = min(m.data$pop.10.m):max(m.data$pop.10.m)),num=300)$getqi(qi="ev", xvalue="range"))
colnames(gee.dich.m.2.high) <- seq(1:ncol(as.data.frame(t(min(m.data$pop.10.m):max(m.data$pop.10.m)))))  # high


## pop.10.m
gee.dich.m.2.pop.10.m = data.frame(
  sim(x = setx(gee.dich.m.2.s, cond = TRUE,
               pop.10.m = min(m.data$pop.10.m):max(m.data$pop.10.m)),num=300)$getqi(qi="ev", xvalue="range"))
colnames(gee.dich.m.2.pop.10.m) <- seq(1:ncol(as.data.frame(t(min(m.data$pop.10.m):max(m.data$pop.10.m)))))  # high



library(Rmisc) # install.packages("Rmisc")

### low
df.low = data.frame(
  mean = c(mean(gee.dich.m.2.s.low$`1`),mean(gee.dich.m.2.s.low$`2`),mean(gee.dich.m.2.s.low$`3`),mean(gee.dich.m.2.s.low$`4`),mean(gee.dich.m.2.s.low$`5`),mean(gee.dich.m.2.s.low$`6`),mean(gee.dich.m.2.s.low$`7`),mean(gee.dich.m.2.s.low$`8`), mean(gee.dich.m.2.s.low$`9`),mean(gee.dich.m.2.s.low$`10`)),
  Population = min(m.data$pop.10.m):max(m.data$pop.10.m),
  Poverty = rep("Low Density", ncol(gee.dich.m.2.s.low)),
  Upper = c(as.numeric(CI(gee.dich.m.2.s.low$`1`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`2`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`3`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`4`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`5`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`6`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`7`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`8`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`9`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`10`)[1])),
  Lower =c(as.numeric(CI(gee.dich.m.2.s.low$`1`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`2`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`3`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`4`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`5`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`6`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`7`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`8`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`9`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`10`)[3]))
)



### high
df.high = data.frame(
  mean = c(mean(gee.dich.m.2.high$`1`),mean(gee.dich.m.2.high$`2`),mean(gee.dich.m.2.high$`3`),mean(gee.dich.m.2.high$`4`),mean(gee.dich.m.2.high$`5`),mean(gee.dich.m.2.high$`6`),mean(gee.dich.m.2.high$`7`),mean(gee.dich.m.2.high$`8`), mean(gee.dich.m.2.high$`9`),mean(gee.dich.m.2.high$`10`)),
  Population = min(m.data$pop.10.m):max(m.data$pop.10.m),
  Poverty = rep("High Density", ncol(gee.dich.m.2.high)),
  Upper = c(as.numeric(CI(gee.dich.m.2.high$`1`)[1]), as.numeric(CI(gee.dich.m.2.high$`2`)[1]), as.numeric(CI(gee.dich.m.2.high$`3`)[1]), as.numeric(CI(gee.dich.m.2.high$`4`)[1]), as.numeric(CI(gee.dich.m.2.high$`5`)[1]), as.numeric(CI(gee.dich.m.2.high$`6`)[1]), as.numeric(CI(gee.dich.m.2.high$`7`)[1]), as.numeric(CI(gee.dich.m.2.high$`8`)[1]), as.numeric(CI(gee.dich.m.2.high$`9`)[1]), as.numeric(CI(gee.dich.m.2.high$`10`)[1])),
  Lower =c(as.numeric(CI(gee.dich.m.2.high$`1`)[3]), as.numeric(CI(gee.dich.m.2.high$`2`)[3]), as.numeric(CI(gee.dich.m.2.high$`3`)[3]), as.numeric(CI(gee.dich.m.2.high$`4`)[3]), as.numeric(CI(gee.dich.m.2.high$`5`)[3]), as.numeric(CI(gee.dich.m.2.high$`6`)[3]), as.numeric(CI(gee.dich.m.2.high$`7`)[3]), as.numeric(CI(gee.dich.m.2.high$`8`)[3]), as.numeric(CI(gee.dich.m.2.high$`9`)[3]), as.numeric(CI(gee.dich.m.2.high$`10`)[3]))
)

### pop
df.pop.alone = data.frame(
  mean = c(mean(gee.dich.m.2.pop.10.m$`1`),mean(gee.dich.m.2.pop.10.m$`2`),mean(gee.dich.m.2.pop.10.m$`3`),mean(gee.dich.m.2.pop.10.m$`4`),mean(gee.dich.m.2.pop.10.m$`5`),mean(gee.dich.m.2.pop.10.m$`6`),mean(gee.dich.m.2.pop.10.m$`7`),mean(gee.dich.m.2.pop.10.m$`8`), mean(gee.dich.m.2.pop.10.m$`9`),mean(gee.dich.m.2.pop.10.m$`10`)),
  Population = min(m.data$pop.10.m):max(m.data$pop.10.m),
  Poverty = rep("Population Size", ncol(gee.dich.m.2.pop.10.m)),
  Upper = c(as.numeric(CI(gee.dich.m.2.pop.10.m$`1`)[1]), as.numeric(CI(gee.dich.m.2.pop.10.m$`2`)[1]), as.numeric(CI(gee.dich.m.2.pop.10.m$`3`)[1]), as.numeric(CI(gee.dich.m.2.pop.10.m$`4`)[1]), as.numeric(CI(gee.dich.m.2.pop.10.m$`5`)[1]), as.numeric(CI(gee.dich.m.2.pop.10.m$`6`)[1]), as.numeric(CI(gee.dich.m.2.pop.10.m$`7`)[1]), as.numeric(CI(gee.dich.m.2.pop.10.m$`8`)[1]), as.numeric(CI(gee.dich.m.2.pop.10.m$`9`)[1]), as.numeric(CI(gee.dich.m.2.pop.10.m$`10`)[1])),
  Lower =c(as.numeric(CI(gee.dich.m.2.pop.10.m$`1`)[3]), as.numeric(CI(gee.dich.m.2.pop.10.m$`2`)[3]), as.numeric(CI(gee.dich.m.2.pop.10.m$`3`)[3]), as.numeric(CI(gee.dich.m.2.pop.10.m$`4`)[3]), as.numeric(CI(gee.dich.m.2.pop.10.m$`5`)[3]), as.numeric(CI(gee.dich.m.2.pop.10.m$`6`)[3]), as.numeric(CI(gee.dich.m.2.pop.10.m$`7`)[3]), as.numeric(CI(gee.dich.m.2.pop.10.m$`8`)[3]), as.numeric(CI(gee.dich.m.2.pop.10.m$`9`)[3]), as.numeric(CI(gee.dich.m.2.pop.10.m$`10`)[3]))
)


### combined two df's
pop.d= rbind(df.high, df.low,df.pop.alone)



### plot
library(ggplot2)
ggplot(pop.d, aes(x=Population, y=mean, colour=Poverty)) + 
  stat_smooth() + 
  geom_ribbon(aes(ymin=Lower, ymax=Upper, linetype=NA), alpha=0.2) +
  stat_smooth(aes(x=Population,y=mean)) +
  xlab("Municipal Population Size") + ylab("Expected Value of Clientelism") + 
  theme_bw() + 
  theme(legend.position="top", legend.title=element_blank(), legend.key = element_rect())


##########################
#  LARGE * WEALTH: gee.dich.m.1.s
##########################


# METHOD 2
# library(devtools) # install.packages("devtools")
# install_github('IQSS/Zelig')
#library(Zelig) # install.packages("Zelig", dependencies=TRUE) # Models

# simulation
library(Zelig)
set.seed(602); options(scipen=999)



## low 
gee.dich.m.1.s.low = data.frame(
  sim(x = setx(gee.dich.m.1.s, cond = TRUE,
             large = min(m.data$large), 
             large:wealth,
             wealth = min(m.data$wealth):max(m.data$wealth)), 
    num=300)$getqi(qi="ev", xvalue="range"))
colnames(gee.dich.m.1.s.low) <- seq(1:ncol(as.data.frame(t(min(m.data$wealth):max(m.data$wealth)))))  # high


## high
gee.dich.m.1.s.high = data.frame(
  sim(x = setx(gee.dich.m.1.s, cond = TRUE,
               large = max(m.data$large), 
               large:wealth,
               wealth = min(m.data$wealth):max(m.data$wealth)),num=300)$getqi(qi="ev", xvalue="range"))
colnames(gee.dich.m.1.s.high) <- seq(1:ncol(as.data.frame(t(min(m.data$wealth):max(m.data$wealth)))))  # high



## wealth
gee.dich.m.1.s.wealth = data.frame(
  sim(x = setx(gee.dich.m.1.s, cond = TRUE,
               large:wealth,
               wealth = min(m.data$wealth):max(m.data$wealth)), 
      num=300)$getqi(qi="ev", xvalue="range"))
colnames(gee.dich.m.1.s.wealth) <- seq(1:ncol(as.data.frame(t(min(m.data$wealth):max(m.data$wealth)))))  # high


# to compute confidence intervals
library(Rmisc) # install.packages("Rmisc")

### low
df.low = data.frame(
  mean = c(mean(gee.dich.m.1.s.low$`1`),mean(gee.dich.m.1.s.low$`2`),mean(gee.dich.m.1.s.low$`3`),mean(gee.dich.m.1.s.low$`4`),mean(gee.dich.m.1.s.low$`5`),mean(gee.dich.m.1.s.low$`6`),mean(gee.dich.m.1.s.low$`7`),mean(gee.dich.m.1.s.low$`8`), mean(gee.dich.m.1.s.low$`9`),mean(gee.dich.m.1.s.low$`10`), mean(gee.dich.m.1.s.low$`11`), mean(gee.dich.m.1.s.low$`12`),mean(gee.dich.m.1.s.low$`13`),mean(gee.dich.m.1.s.low$`14`),mean(gee.dich.m.1.s.low$`15`),mean(gee.dich.m.1.s.low$`16`),mean(gee.dich.m.1.s.low$`17`),mean(gee.dich.m.1.s.low$`18`),mean(gee.dich.m.1.s.low$`19`),mean(gee.dich.m.1.s.low$`20`),mean(gee.dich.m.1.s.low$`21`),mean(gee.dich.m.1.s.low$`22`),mean(gee.dich.m.1.s.low$`23`),mean(gee.dich.m.1.s.low$`24`),mean(gee.dich.m.1.s.low$`25`)),Wealth = min(m.data$wealth):max(m.data$wealth),
  Poverty = rep("Low Density", ncol(gee.dich.m.1.s.low)),
  Upper = c(as.numeric(CI(gee.dich.m.1.s.low$`1`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`2`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`3`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`4`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`5`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`6`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`7`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`8`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`9`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`10`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`11`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`12`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`13`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`14`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`15`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`16`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`17`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`18`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`19`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`20`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`21`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`22`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`23`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`24`)[1]), as.numeric(CI(gee.dich.m.1.s.low$`25`)[1])),
  Lower =c(as.numeric(CI(gee.dich.m.1.s.low$`1`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`2`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`3`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`4`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`5`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`6`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`7`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`8`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`9`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`10`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`11`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`12`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`13`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`14`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`15`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`16`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`17`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`18`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`19`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`20`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`21`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`22`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`23`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`24`)[3]), as.numeric(CI(gee.dich.m.1.s.low$`25`)[3]))
  )



### high
df.high = data.frame(
  mean = c(mean(gee.dich.m.1.s.high$`1`),mean(gee.dich.m.1.s.high$`2`),mean(gee.dich.m.1.s.high$`3`),mean(gee.dich.m.1.s.high$`4`),mean(gee.dich.m.1.s.high$`5`),mean(gee.dich.m.1.s.high$`6`),mean(gee.dich.m.1.s.high$`7`),mean(gee.dich.m.1.s.high$`8`),mean(gee.dich.m.1.s.high$`9`),mean(gee.dich.m.1.s.high$`10`),mean(gee.dich.m.1.s.high$`11`),mean(gee.dich.m.1.s.high$`12`),mean(gee.dich.m.1.s.high$`13`),mean(gee.dich.m.1.s.high$`14`),mean(gee.dich.m.1.s.high$`15`),mean(gee.dich.m.1.s.high$`16`),mean(gee.dich.m.1.s.high$`17`),mean(gee.dich.m.1.s.high$`18`),mean(gee.dich.m.1.s.high$`19`),mean(gee.dich.m.1.s.high$`20`),mean(gee.dich.m.1.s.high$`21`),mean(gee.dich.m.1.s.high$`22`),mean(gee.dich.m.1.s.high$`23`),mean(gee.dich.m.1.s.high$`24`),mean(gee.dich.m.1.s.high$`25`)),
  Wealth = min(m.data$wealth):max(m.data$wealth),
  Poverty = rep("High Density", ncol(gee.dich.m.1.s.high)),
  Upper = c(as.numeric(CI(gee.dich.m.1.s.high$`1`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`2`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`3`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`4`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`5`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`6`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`7`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`8`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`9`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`10`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`11`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`12`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`13`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`14`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`15`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`16`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`17`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`18`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`19`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`20`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`21`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`22`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`23`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`24`)[1]), as.numeric(CI(gee.dich.m.1.s.high$`25`)[1])),
  Lower =c(as.numeric(CI(gee.dich.m.1.s.high$`1`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`2`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`3`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`4`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`5`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`6`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`7`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`8`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`9`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`10`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`11`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`12`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`13`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`14`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`15`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`16`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`17`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`18`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`19`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`20`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`21`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`22`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`23`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`24`)[3]), as.numeric(CI(gee.dich.m.1.s.high$`25`)[3]))
  )

### wealth
df.wealth.alone = data.frame(
  mean = c(mean(gee.dich.m.1.s.wealth$`1`),mean(gee.dich.m.1.s.wealth$`2`),mean(gee.dich.m.1.s.wealth$`3`),mean(gee.dich.m.1.s.wealth$`4`),mean(gee.dich.m.1.s.wealth$`5`),mean(gee.dich.m.1.s.wealth$`6`),mean(gee.dich.m.1.s.wealth$`7`),mean(gee.dich.m.1.s.wealth$`8`),mean(gee.dich.m.1.s.wealth$`9`),mean(gee.dich.m.1.s.wealth$`10`),mean(gee.dich.m.1.s.wealth$`11`),mean(gee.dich.m.1.s.wealth$`12`),mean(gee.dich.m.1.s.wealth$`13`),mean(gee.dich.m.1.s.wealth$`14`),mean(gee.dich.m.1.s.wealth$`15`),mean(gee.dich.m.1.s.wealth$`16`),mean(gee.dich.m.1.s.wealth$`17`),mean(gee.dich.m.1.s.wealth$`18`),mean(gee.dich.m.1.s.wealth$`19`),mean(gee.dich.m.1.s.wealth$`20`),mean(gee.dich.m.1.s.wealth$`21`),mean(gee.dich.m.1.s.wealth$`22`),mean(gee.dich.m.1.s.wealth$`23`),mean(gee.dich.m.1.s.wealth$`24`),mean(gee.dich.m.1.s.wealth$`25`)),
  Wealth = min(m.data$wealth):max(m.data$wealth),
  Poverty = rep("Wealth Index", ncol(gee.dich.m.1.s.wealth)),
  Upper = c(as.numeric(CI(gee.dich.m.1.s.wealth$`1`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`2`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`3`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`4`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`5`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`6`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`7`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`8`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`9`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`10`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`11`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`12`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`13`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`14`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`15`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`16`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`17`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`18`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`19`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`20`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`21`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`22`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`23`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`24`)[1]), as.numeric(CI(gee.dich.m.1.s.wealth$`25`)[1])),
  Lower =c(as.numeric(CI(gee.dich.m.1.s.wealth$`1`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`2`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`3`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`4`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`5`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`6`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`7`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`8`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`9`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`10`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`11`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`12`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`13`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`14`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`15`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`16`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`17`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`18`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`19`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`20`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`21`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`22`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`23`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`24`)[3]), as.numeric(CI(gee.dich.m.1.s.wealth$`25`)[3]))
)



### combined two df's
wealth.d= rbind(df.high, df.low,df.wealth.alone)


### plot
library(ggplot2)
p1=ggplot(wealth.d, aes(x=Wealth, y=mean, colour=Poverty)) + 
  stat_smooth() + 
  geom_ribbon(aes(ymin=Lower, ymax=Upper, linetype=NA), alpha=0.2) +
  stat_smooth(aes(x=Wealth,y=mean)) +
  xlab("Wealth Index") + ylab("Expected Value of Clientelism") + 
  theme_bw() + 
  theme(legend.position="top", legend.title=element_blank(), legend.key = element_rect())



##########################
#  LARGE * MUNOPP: gee.dich.m.2.s
##########################


# simulation
library(Zelig)
set.seed(602); options(scipen=999)


# low 
gee.dich.m.2.s.low = data.frame(
  sim(
    x = setx(gee.dich.m.2.s, cond = TRUE,
             large = min(m.data$large), 
             large:munopp,
             munopp = min(m.data$munopp):max(m.data$munopp)), 
    num=200)$getqi(qi="ev", xvalue="range"))

colnames(gee.dich.m.2.s.low) <- seq(1:ncol(as.data.frame(t(min(m.data$munopp):max(m.data$munopp)))))  # low



# high 
gee.dich.m.2.s.high = data.frame(
  sim(
    x = setx(gee.dich.m.2.s, cond = TRUE,
             large = max(m.data$large), 
             large:munopp,
             munopp = min(m.data$munopp):max(m.data$munopp)), 
    num=200)$getqi(qi="ev", xvalue="range"))

colnames(gee.dich.m.2.s.high) <- seq(1:ncol(as.data.frame(t(min(m.data$munopp):max(m.data$munopp)))))  # low



# munopp 
gee.dich.m.2.s.munopp = data.frame(
  sim(x = setx(gee.dich.m.2.s, cond = TRUE,
               large:munopp,
               munopp = min(m.data$munopp):max(m.data$munopp)), 
      num=300)$getqi(qi="ev", xvalue="range"))
colnames(gee.dich.m.2.s.munopp) <- seq(1:ncol(as.data.frame(t(min(m.data$munopp):max(m.data$munopp)))))  # high


### df's
### low
df.low = data.frame(
  mean = c(mean(gee.dich.m.2.s.low$`1`),mean(gee.dich.m.2.s.low$`2`),mean(gee.dich.m.2.s.low$`3`),mean(gee.dich.m.2.s.low$`4`),mean(gee.dich.m.2.s.low$`5`),mean(gee.dich.m.2.s.low$`6`),mean(gee.dich.m.2.s.low$`7`),mean(gee.dich.m.2.s.low$`8`), mean(gee.dich.m.2.s.low$`9`),mean(gee.dich.m.2.s.low$`10`), mean(gee.dich.m.2.s.low$`11`), mean(gee.dich.m.2.s.low$`12`),mean(gee.dich.m.2.s.low$`13`),mean(gee.dich.m.2.s.low$`14`),mean(gee.dich.m.2.s.low$`15`),mean(gee.dich.m.2.s.low$`16`),mean(gee.dich.m.2.s.low$`17`),mean(gee.dich.m.2.s.low$`18`),mean(gee.dich.m.2.s.low$`19`),mean(gee.dich.m.2.s.low$`20`), mean(gee.dich.m.2.s.low$`21`),mean(gee.dich.m.2.s.low$`22`),mean(gee.dich.m.2.s.low$`23`),mean(gee.dich.m.2.s.low$`24`),mean(gee.dich.m.2.s.low$`25`),mean(gee.dich.m.2.s.low$`26`),mean(gee.dich.m.2.s.low$`27`),mean(gee.dich.m.2.s.low$`28`), mean(gee.dich.m.2.s.low$`29`),mean(gee.dich.m.2.s.low$`30`), mean(gee.dich.m.2.s.low$`31`), mean(gee.dich.m.2.s.low$`32`),mean(gee.dich.m.2.s.low$`33`),mean(gee.dich.m.2.s.low$`34`),mean(gee.dich.m.2.s.low$`35`),mean(gee.dich.m.2.s.low$`36`),mean(gee.dich.m.2.s.low$`37`),mean(gee.dich.m.2.s.low$`38`),mean(gee.dich.m.2.s.low$`39`),mean(gee.dich.m.2.s.low$`40`),mean(gee.dich.m.2.s.low$`41`)),
  Type = rep("Low Density", ncol(gee.dich.m.2.s.low)),
  Opposition = min(m.data$munopp):max(m.data$munopp),
  Upper = c(as.numeric(CI(gee.dich.m.2.s.low$`1`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`2`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`3`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`4`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`5`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`6`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`7`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`8`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`9`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`10`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`11`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`12`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`13`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`14`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`15`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`16`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`17`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`18`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`19`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`20`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`21`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`22`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`23`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`24`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`25`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`26`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`27`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`28`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`29`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`30`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`31`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`32`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`33`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`34`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`35`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`36`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`37`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`38`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`39`)[1]), as.numeric(CI(gee.dich.m.2.s.low$`40`)[1]),as.numeric(CI(gee.dich.m.2.s.low$`41`)[1])),
  Lower = c(
    as.numeric(CI(gee.dich.m.2.s.low$`1`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`2`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`3`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`4`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`5`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`6`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`7`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`8`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`9`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`10`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`11`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`12`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`13`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`14`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`15`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`16`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`17`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`18`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`19`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`20`)[3]),as.numeric(CI(gee.dich.m.2.s.low$`21`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`22`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`23`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`24`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`25`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`26`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`27`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`28`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`29`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`30`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`31`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`32`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`33`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`34`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`35`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`36`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`37`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`38`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`39`)[3]), as.numeric(CI(gee.dich.m.2.s.low$`40`)[3]),as.numeric(CI(gee.dich.m.2.s.low$`41`)[3]))
  )

### high
df.high = data.frame(
  mean = c(mean(gee.dich.m.2.s.high$`1`),mean(gee.dich.m.2.s.high$`2`),mean(gee.dich.m.2.s.high$`3`),mean(gee.dich.m.2.s.high$`4`),mean(gee.dich.m.2.s.high$`5`),mean(gee.dich.m.2.s.high$`6`),mean(gee.dich.m.2.s.high$`7`),mean(gee.dich.m.2.s.high$`8`), mean(gee.dich.m.2.s.high$`9`),mean(gee.dich.m.2.s.high$`10`), mean(gee.dich.m.2.s.high$`11`), mean(gee.dich.m.2.s.high$`12`),mean(gee.dich.m.2.s.high$`13`),mean(gee.dich.m.2.s.high$`14`),mean(gee.dich.m.2.s.high$`15`),mean(gee.dich.m.2.s.high$`16`),mean(gee.dich.m.2.s.high$`17`),mean(gee.dich.m.2.s.high$`18`),mean(gee.dich.m.2.s.high$`19`),mean(gee.dich.m.2.s.high$`20`), mean(gee.dich.m.2.s.high$`21`),mean(gee.dich.m.2.s.high$`22`),mean(gee.dich.m.2.s.high$`23`),mean(gee.dich.m.2.s.high$`24`),mean(gee.dich.m.2.s.high$`25`),mean(gee.dich.m.2.s.high$`26`),mean(gee.dich.m.2.s.high$`27`),mean(gee.dich.m.2.s.high$`28`), mean(gee.dich.m.2.s.high$`29`),mean(gee.dich.m.2.s.high$`30`), mean(gee.dich.m.2.s.high$`31`), mean(gee.dich.m.2.s.high$`32`),mean(gee.dich.m.2.s.high$`33`),mean(gee.dich.m.2.s.high$`34`),mean(gee.dich.m.2.s.high$`35`),mean(gee.dich.m.2.s.high$`36`),mean(gee.dich.m.2.s.high$`37`),mean(gee.dich.m.2.s.high$`38`),mean(gee.dich.m.2.s.high$`39`),mean(gee.dich.m.2.s.high$`40`),mean(gee.dich.m.2.s.high$`41`)),
  Type = rep("High Density", ncol(gee.dich.m.2.s.high)),
  Opposition = min(m.data$munopp):max(m.data$munopp),
  Upper = c(as.numeric(CI(gee.dich.m.2.s.high$`1`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`2`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`3`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`4`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`5`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`6`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`7`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`8`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`9`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`10`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`11`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`12`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`13`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`14`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`15`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`16`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`17`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`18`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`19`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`20`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`21`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`22`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`23`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`24`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`25`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`26`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`27`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`28`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`29`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`30`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`31`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`32`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`33`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`34`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`35`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`36`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`37`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`38`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`39`)[1]), as.numeric(CI(gee.dich.m.2.s.high$`40`)[1]),as.numeric(CI(gee.dich.m.2.s.high$`41`)[1])),
  Lower =c(as.numeric(CI(gee.dich.m.2.s.high$`1`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`2`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`3`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`4`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`5`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`6`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`7`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`8`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`9`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`10`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`11`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`12`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`13`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`14`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`15`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`16`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`17`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`18`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`19`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`20`)[3]),as.numeric(CI(gee.dich.m.2.s.high$`21`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`22`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`23`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`24`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`25`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`26`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`27`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`28`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`29`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`30`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`31`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`32`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`33`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`34`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`35`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`36`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`37`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`38`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`39`)[3]), as.numeric(CI(gee.dich.m.2.s.high$`40`)[3]),as.numeric(CI(gee.dich.m.2.s.high$`41`)[3]))
)

### munopp
df.munopp.alone = data.frame(
  mean = c(mean(gee.dich.m.2.s.munopp$`1`),mean(gee.dich.m.2.s.munopp$`2`),mean(gee.dich.m.2.s.munopp$`3`),mean(gee.dich.m.2.s.munopp$`4`),mean(gee.dich.m.2.s.munopp$`5`),mean(gee.dich.m.2.s.munopp$`6`),mean(gee.dich.m.2.s.munopp$`7`),mean(gee.dich.m.2.s.munopp$`8`), mean(gee.dich.m.2.s.munopp$`9`),mean(gee.dich.m.2.s.munopp$`10`), mean(gee.dich.m.2.s.munopp$`11`), mean(gee.dich.m.2.s.munopp$`12`),mean(gee.dich.m.2.s.munopp$`13`),mean(gee.dich.m.2.s.munopp$`14`),mean(gee.dich.m.2.s.munopp$`15`),mean(gee.dich.m.2.s.munopp$`16`),mean(gee.dich.m.2.s.munopp$`17`),mean(gee.dich.m.2.s.munopp$`18`),mean(gee.dich.m.2.s.munopp$`19`),mean(gee.dich.m.2.s.munopp$`20`), mean(gee.dich.m.2.s.munopp$`21`),mean(gee.dich.m.2.s.munopp$`22`),mean(gee.dich.m.2.s.munopp$`23`),mean(gee.dich.m.2.s.munopp$`24`),mean(gee.dich.m.2.s.munopp$`25`),mean(gee.dich.m.2.s.munopp$`26`),mean(gee.dich.m.2.s.munopp$`27`),mean(gee.dich.m.2.s.munopp$`28`), mean(gee.dich.m.2.s.munopp$`29`),mean(gee.dich.m.2.s.munopp$`30`), mean(gee.dich.m.2.s.munopp$`31`), mean(gee.dich.m.2.s.munopp$`32`),mean(gee.dich.m.2.s.munopp$`33`),mean(gee.dich.m.2.s.munopp$`34`),mean(gee.dich.m.2.s.munopp$`35`),mean(gee.dich.m.2.s.munopp$`36`),mean(gee.dich.m.2.s.munopp$`37`),mean(gee.dich.m.2.s.munopp$`38`),mean(gee.dich.m.2.s.munopp$`39`),mean(gee.dich.m.2.s.munopp$`40`),mean(gee.dich.m.2.s.munopp$`41`)),
  Type = rep("Municipal Opposition", ncol(gee.dich.m.2.s.munopp)),
  Opposition = min(m.data$munopp):max(m.data$munopp),
  Upper = c(as.numeric(CI(gee.dich.m.2.s.munopp$`1`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`2`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`3`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`4`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`5`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`6`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`7`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`8`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`9`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`10`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`11`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`12`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`13`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`14`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`15`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`16`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`17`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`18`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`19`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`20`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`21`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`22`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`23`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`24`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`25`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`26`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`27`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`28`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`29`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`30`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`31`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`32`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`33`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`34`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`35`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`36`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`37`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`38`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`39`)[1]), as.numeric(CI(gee.dich.m.2.s.munopp$`40`)[1]),as.numeric(CI(gee.dich.m.2.s.munopp$`41`)[1])),
  Lower =c(as.numeric(CI(gee.dich.m.2.s.munopp$`1`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`2`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`3`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`4`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`5`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`6`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`7`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`8`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`9`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`10`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`11`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`12`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`13`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`14`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`15`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`16`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`17`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`18`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`19`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`20`)[3]),as.numeric(CI(gee.dich.m.2.s.munopp$`21`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`22`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`23`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`24`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`25`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`26`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`27`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`28`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`29`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`30`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`31`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`32`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`33`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`34`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`35`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`36`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`37`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`38`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`39`)[3]), as.numeric(CI(gee.dich.m.2.s.munopp$`40`)[3]),as.numeric(CI(gee.dich.m.2.s.munopp$`41`)[3]))
)

### combined two df's
munopp.d= rbind(df.high, df.low,df.munopp.alone)


### plot
library(ggplot2)
p2=ggplot(munopp.d, aes(x=Opposition, y=mean, colour=Type)) + 
  stat_smooth() + 
  geom_ribbon(aes(ymin=Lower, ymax=Upper, linetype=NA), alpha=0.2) +
  stat_smooth(aes(x=Opposition,y=mean)) +
  xlab("Municipal Opposition") + ylab("Expected Value of Clientelism") + 
  theme_bw() + 
  theme(legend.position="top", legend.title=element_blank(), legend.key = element_rect())



##########################
#  LARGE * POLINV: gee.dich.m.3.s
##########################


# simulation
library(Zelig)
set.seed(602); options(scipen=999)


# low 
gee.dich.m.3.s.low = data.frame(
  sim(
    x = setx(gee.dich.m.3.s, cond = TRUE,
             large = min(m.data$large), 
             polinv:large,
             polinv = min(m.data$polinv):max(m.data$polinv)), 
    num=700)$getqi(qi="ev", xvalue="range"))

colnames(gee.dich.m.3.s.low) <- seq(1:ncol(as.data.frame(t(min(m.data$polinv):max(m.data$polinv)))))  # low

# high
gee.dich.m.3.s.high = data.frame(
  sim(
    x = setx(gee.dich.m.3.s, cond = TRUE,
               large = max(m.data$large), 
             polinv:large,
               polinv = min(m.data$polinv):max(m.data$polinv)), 
      num=700)$getqi(qi="ev", xvalue="range")) ; 
colnames(gee.dich.m.3.s.high) <- seq(1:ncol(as.data.frame(t(min(m.data$polinv):max(m.data$polinv)))))  # high


# polinv 
gee.dich.m.3.s.polinv = data.frame(
  sim(x = setx(gee.dich.m.3.s, cond = TRUE,
               large:munopp,
               polinv = min(m.data$polinv):max(m.data$polinv)), 
      num=300)$getqi(qi="ev", xvalue="range"))
colnames(gee.dich.m.3.s.polinv) <- seq(1:ncol(as.data.frame(t(min(m.data$polinv):max(m.data$polinv)))))  


### df's
### low
df.low = data.frame(
  mean = c(mean(gee.dich.m.3.s.low$`1`),mean(gee.dich.m.3.s.low$`2`),mean(gee.dich.m.3.s.low$`3`),mean(gee.dich.m.3.s.low$`4`),mean(gee.dich.m.3.s.low$`5`),mean(gee.dich.m.3.s.low$`6`),mean(gee.dich.m.3.s.low$`7`),mean(gee.dich.m.3.s.low$`8`)),
  Type = rep("Low Density", ncol(gee.dich.m.3.s.low)),
  Opposition = min(m.data$polinv):max(m.data$polinv),
  Upper = c(as.numeric(CI(gee.dich.m.3.s.low$`1`)[1]), as.numeric(CI(gee.dich.m.3.s.low$`2`)[1]), as.numeric(CI(gee.dich.m.3.s.low$`3`)[1]), as.numeric(CI(gee.dich.m.3.s.low$`4`)[1]), as.numeric(CI(gee.dich.m.3.s.low$`5`)[1]), as.numeric(CI(gee.dich.m.3.s.low$`6`)[1]), as.numeric(CI(gee.dich.m.3.s.low$`7`)[1]), as.numeric(CI(gee.dich.m.3.s.low$`8`)[1])),
  Lower = c(
    as.numeric(CI(gee.dich.m.3.s.low$`1`)[3]), as.numeric(CI(gee.dich.m.3.s.low$`2`)[3]), as.numeric(CI(gee.dich.m.3.s.low$`3`)[3]), as.numeric(CI(gee.dich.m.3.s.low$`4`)[3]), as.numeric(CI(gee.dich.m.3.s.low$`5`)[3]), as.numeric(CI(gee.dich.m.3.s.low$`6`)[3]), as.numeric(CI(gee.dich.m.3.s.low$`7`)[3]), as.numeric(CI(gee.dich.m.3.s.low$`8`)[3]))
)

### high
df.high = data.frame(
  mean = c(mean(gee.dich.m.3.s.high$`1`),mean(gee.dich.m.3.s.high$`2`),mean(gee.dich.m.3.s.high$`3`),mean(gee.dich.m.3.s.high$`4`),mean(gee.dich.m.3.s.high$`5`),mean(gee.dich.m.3.s.high$`6`),mean(gee.dich.m.3.s.high$`7`),mean(gee.dich.m.3.s.high$`8`)),
  Type = rep("High Density", ncol(gee.dich.m.3.s.high)),
  Opposition = min(m.data$polinv):max(m.data$polinv),
  Upper = c(as.numeric(CI(gee.dich.m.3.s.high$`1`)[1]), as.numeric(CI(gee.dich.m.3.s.high$`2`)[1]), as.numeric(CI(gee.dich.m.3.s.high$`3`)[1]), as.numeric(CI(gee.dich.m.3.s.high$`4`)[1]), as.numeric(CI(gee.dich.m.3.s.high$`5`)[1]), as.numeric(CI(gee.dich.m.3.s.high$`6`)[1]), as.numeric(CI(gee.dich.m.3.s.high$`7`)[1]), as.numeric(CI(gee.dich.m.3.s.high$`8`)[1])),
  Lower = c(
    as.numeric(CI(gee.dich.m.3.s.high$`1`)[3]), as.numeric(CI(gee.dich.m.3.s.high$`2`)[3]), as.numeric(CI(gee.dich.m.3.s.high$`3`)[3]), as.numeric(CI(gee.dich.m.3.s.high$`4`)[3]), as.numeric(CI(gee.dich.m.3.s.high$`5`)[3]), as.numeric(CI(gee.dich.m.3.s.high$`6`)[3]), as.numeric(CI(gee.dich.m.3.s.high$`7`)[3]), as.numeric(CI(gee.dich.m.3.s.high$`8`)[3]))
)

### polinv
df.polinv.alone = data.frame(
  mean = c(mean(gee.dich.m.3.s.polinv$`1`),mean(gee.dich.m.3.s.polinv$`2`),mean(gee.dich.m.3.s.polinv$`3`),mean(gee.dich.m.3.s.polinv$`4`),mean(gee.dich.m.3.s.polinv$`5`),mean(gee.dich.m.3.s.polinv$`6`),mean(gee.dich.m.3.s.polinv$`7`),mean(gee.dich.m.3.s.polinv$`8`)),
  Type = rep("Political Involvement", ncol(gee.dich.m.3.s.polinv)),
  Opposition = min(m.data$polinv):max(m.data$polinv),
  Upper = c(as.numeric(CI(gee.dich.m.3.s.polinv$`1`)[1]), as.numeric(CI(gee.dich.m.3.s.polinv$`2`)[1]), as.numeric(CI(gee.dich.m.3.s.polinv$`3`)[1]), as.numeric(CI(gee.dich.m.3.s.polinv$`4`)[1]), as.numeric(CI(gee.dich.m.3.s.polinv$`5`)[1]), as.numeric(CI(gee.dich.m.3.s.polinv$`6`)[1]), as.numeric(CI(gee.dich.m.3.s.polinv$`7`)[1]), as.numeric(CI(gee.dich.m.3.s.polinv$`8`)[1])),
  Lower =c(as.numeric(CI(gee.dich.m.3.s.polinv$`1`)[3]), as.numeric(CI(gee.dich.m.3.s.polinv$`2`)[3]), as.numeric(CI(gee.dich.m.3.s.polinv$`3`)[3]), as.numeric(CI(gee.dich.m.3.s.polinv$`4`)[3]), as.numeric(CI(gee.dich.m.3.s.polinv$`5`)[3]), as.numeric(CI(gee.dich.m.3.s.polinv$`6`)[3]), as.numeric(CI(gee.dich.m.3.s.polinv$`7`)[3]), as.numeric(CI(gee.dich.m.3.s.polinv$`8`)[3]))
)

### combined two df's
polinv.d= rbind(df.high, df.low,df.polinv.alone)


### plot
library(ggplot2)
p3=ggplot(polinv.d, aes(x=Opposition, y=mean, colour=Type)) + 
  stat_smooth() + 
  geom_ribbon(aes(ymin=Lower, ymax=Upper, linetype=NA), alpha=0.2) +
  stat_smooth(aes(x=Opposition,y=mean)) +
  xlab("Political Involvement") + ylab("Expected Value of Clientelism") + 
  theme_bw() + 
  theme(legend.position="top", legend.title=element_blank(), legend.key = element_rect())


######################################################
#  Combined Plots
library(cowplot) # install.packages("cowplot")
plot_grid(p1,p2,p3,  ncol = 1)






######################################################
#  D  E S C R I P T I V E          P   L   O   T   S #
######################################################

load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/m.data.RData")
library(car)



########################################################
# Descriptive Stats Matched Set

# matched sample
m.data.clien1dummy <- m.data$clien1dummy 
m.data.clien1dummy <- as.numeric(m.data.clien1dummy)
m.data.clien1dummy <- recode(m.data.clien1dummy, "1 = 0 ; 2 = 1")



m.data.large <- m.data$large 
m.data.wealth <- m.data$wealth 
m.data.munopp <- m.data$munopp
m.data.polinv <- m.data$polinv 

m.data.exc7 <- m.data$exc7 
m.data.ing4 <- m.data$ing4
m.data.vb3 <- m.data$vb3 

m.data.urban <- m.data$urban
m.data.urban <- as.numeric(m.data.urban)
m.data.urban <- recode(m.data.urban, "1 = 0 ; 2 = 1")

m.data.ed <- m.data$ed
m.data.ed <- as.numeric(m.data.ed)
m.data.ed <- recode(m.data.ed, "
                    1 = 0 ; 
                    2 = 1 ; 
                    3 = 2 ; 
                    4 = 3 ;
                    5 = 4 ;
                    6 = 5 ;
                    7 = 6 ;
                    8 = 7 ;
                    9 = 8 ;
                    10 = 9 ;
                    11 = 10 ;
                    12 = 11 ;
                    13 = 12 ;
                    14 = 13 ;
                    15 = 14 ;
                    16 = 15 ;
                    17 = 16")

dat.m <- data.frame(m.data.clien1dummy, m.data.large, m.data.wealth, m.data.munopp, m.data.polinv, m.data.exc7, m.data.ing4, m.data.vb3, m.data.urban, m.data.ed)


library(stargazer, quietly = T)
stargazer(dat.m, 
          summary=T, 
          title = "Summary Statistics: Matched Sample",
          label = "sumtab:1",
          type = "text",
          font.size = "scriptsize",
          style= "apsr",
          covariate.labels=c("Clientelism", "High Density", "Wealth Index", "Municipal Opposition", "Political Involvement", "Perception of Corruption", "Support for Democracy", "Party Id.", "Urban", "Years of Education"),
          table.placement = "h",
          notes.align = "c"
)


########################################################
# Descriptive Stats Raw Set

# whole sample
dat.clien1dummy <- dat$clien1dummy 
dat.clien1dummy <- as.numeric(dat.clien1dummy)
dat.clien1dummy <- recode(dat.clien1dummy, "1 = 0 ; 2 = 1")



dat.large <- dat$large 
dat.wealth <- dat$wealth 
dat.munopp <- dat$munopp
dat.polinv <- dat$polinv 

dat.exc7 <- dat$exc7 
dat.ing4 <- dat$ing4
dat.vb3 <- dat$vb3 

dat.urban <- dat$urban
dat.urban <- as.numeric(dat.urban)
dat.urban <- recode(dat.urban, "1 = 0 ; 2 = 1")

dat.ed <- dat$ed
dat.ed <- as.numeric(dat.ed)
dat.ed <- recode(dat.ed, "
                 1 = 0 ; 
                 2 = 1 ; 
                 3 = 2 ; 
                 4 = 3 ;
                 5 = 4 ;
                 6 = 5 ;
                 7 = 6 ;
                 8 = 7 ;
                 9 = 8 ;
                 10 = 9 ;
                 11 = 10 ;
                 12 = 11 ;
                 13 = 12 ;
                 14 = 13 ;
                 15 = 14 ;
                 16 = 15 ;
                 17 = 16")

dat.r <- data.frame(dat.clien1dummy, dat.large, dat.wealth, dat.munopp, dat.polinv, dat.exc7, dat.ing4, dat.vb3, dat.urban, dat.ed)


library(stargazer, quietly = T)
stargazer(dat.r, 
          summary=T, 
          title = "Summary Statistics: Raw Sample",
          label = "sumtab:2",
          type = "text",
          font.size = "scriptsize",
          style= "apsr",
          covariate.labels=c("Clientelism", "High Density", "Wealth Index", "Municipal Opposition", "Political Involvement", "Perception of Corruption", "Support for Democracy", "Party Id.", "Urban", "Years of Education"),
          table.placement = "h",
          notes.align = "c"
)


############################################################
# Distribution of Individuals by Municipality

## df of matched set
municipality.m = data.frame(
  Municipality = as.factor(m.data$municipality),
  Sample = c(rep("Matched", length(m.data$municipality))
             )
)

## df of raw set
municipality.r = data.frame(
  Municipality = as.factor(dat$municipality),
  Sample = c(rep("Raw", length(dat$municipality))
  )
)

## rbinding the two of them
municipality.d = data.frame(rbind(municipality.m, municipality.r))


## plot
library(ggplot2)
ggplot(municipality.d, aes(factor(Municipality), fill = Sample)) + geom_bar() + coord_flip() +
  xlab("Municipality") + 
  ylab("Frequency") + 
  theme_bw()

############################################################
# Distribution of Individuals by High/Low COnditions and municipality

## HIGH df
high.d = data.frame(
  Municipality = as.factor(m.data$municipality[m.data$large == 1]),
  Density = c(rep("High", length(m.data$municipality[as.numeric(m.data$large == 1)]))),
  Wealth = as.numeric(m.data$wealth)[m.data$large == 1]
)

## LOW df
low.d = data.frame(
  Municipality = as.factor(m.data$municipality[m.data$large == 0]),
  Density = c(rep("Low", length(m.data$municipality[as.numeric(m.data$large == 0)]))),
  Wealth = as.numeric(m.data$wealth)[m.data$large == 0]
)

## rbinding the two of them
density.d = data.frame(rbind(high.d, low.d))


## plot
library(ggplot2)
ggplot(density.d, aes(factor(Municipality), fill = Density)) + 
  geom_bar() + 
  geom_point(data=density.d, 
             position = position_jitter(width = 0.5), 
             size = I(1.5),
             aes(
    x=as.factor(Municipality), 
    y= abs(min(m.data$wealth))+Wealth, 
    alpha=Wealth)) + 
  coord_flip() +
  xlab("Municipality") + 
  ylab("Frequency") + 
  theme_bw() +
  theme(legend.key = element_rect(colour = NA, fill = NA, size = 0.5))


############################################################
# Distribution of Individuals by High/Low COnditions and Wealth

## plot
library(ggplot2)

### Matched Set
density.wealth.m= ggplot() + geom_point(
  aes(
    y=as.factor(m.data$large), 
    x=m.data$wealth, 
    colour=m.data$large), 
  position = position_jitter(width = 5)) +
  xlab("Wealth Index: Matched Set") + 
  ylab("Density of the Poor") + 
  xlim(min(dat$wealth), max(dat$wealth)) +
  theme_bw() +
  theme(legend.position="none") +
  scale_y_discrete(breaks=c(0, 1), labels=c("Low", "High"))

### Raw Set
density.wealth.r= ggplot() + geom_point(
  aes(
    y=as.factor(dat$large), 
    x=dat$wealth, 
    colour=dat$large), 
  position = position_jitter(width = 5)) +
  xlab("Wealth Index: Raw Set") + 
  ylab("Density of the Poor") + 
  xlim(min(dat$wealth), max(dat$wealth)) +
  theme_bw() +
  theme(legend.position="none") +
  scale_y_discrete(breaks=c(0, 1), labels=c("Low", "High"))

library(cowplot) # install.packages("cowplot")
plot_grid(density.wealth.m,density.wealth.r,  nrow = 2)


  

############################################################
# Distribution Outcome Variable Binary Outcome
m.data$clien1dummy <- factor(m.data$clien1dummy, labels = c("No", "Yes"))

library(ggplot2)
ggplot(data=m.data, aes(x=clien1dummy)) + 
  geom_bar(width=.5, stat="count",size=1, alpha=.7) + 
  xlab("Clientelism") + 
  ylab("Frequency") + 
  theme_bw()


## Distribution Outcome Variable 3 outcomes
library(ggplot2)
ggplot(data=m.data, aes(x=clientelism)) + 
  geom_bar(width=.5, stat="count",size=1, alpha=.7) + 
  xlab("Clientelism") + 
  ylab("Frequency") + 
  theme_bw()


############################################################
## Distribution treatment var
# Labels
ggplot.labels1 <- data.frame(
  time = c(15, 60), 
  value = c(75, 75), 
  label = c("Low (C)", "High (T)"), 
  type = c("NA*", "MVH")
)

## Plot BARS
library(ggplot2)
ggplot(m.data, aes(x=wagehalf)) + 
  geom_histogram(binwidth=2.5, alpha=.7) + 
  #geom_density(alpha=.1) +
  theme_bw() +
  geom_segment(data= m.data, aes(x = (wagehalf=median(m.data$wagehalf)), y = 0, xend = (wagehalf=median(m.data$wagehalf)), yend = 100), linetype="dashed", size=2, colour = "forestgreen") + 
  xlab("Density of the Poor") + ylab("Frequency") +
  geom_text(data = ggplot.labels1, aes(x = time, y = value, label = label), colour = "forestgreen")


## Plot Density
library(ggplot2)
ggplot() + 
  geom_density(aes(x=m.data$wagehalf), fill = "forestgreen", alpha = .2) + 
  geom_segment(data= 
                 m.data, aes(
                   x = (wagehalf=median(m.data$wagehalf)), 
                   y = 0, 
                   xend = (wagehalf=median(m.data$wagehalf)), 
                   yend = .0305), 
               linetype="dashed", 
               size=1.5, 
               colour = "forestgreen") + 
               xlab("Percentage of People Living with \n Less than Half of the Minimum Wage") + 
               ylab("Density") + 
               theme_bw()


######################################################
#  B   A   L   A   N   C   E       P   L   O   T   S #
######################################################

# Density Plot: Propensity Scores, by Treatment Condition and By DIscarded 
Distance = m.out$distance
Discarded = m.out$discarded
Treated = dat$clien1dummy

library(ggplot2)
ggplot() + geom_density(aes(x=Distance, colour=Discarded, linetype=Treated), alpha=.1) +
  ylab("Estimated Density") + 
  xlab("Distance") + 
  theme_bw()
  


# DISTRIBUTION PLOTS PRE AND POST MATCHING
## Matched
library(ggplot2)
balance.1.m=ggplot() + 
  geom_density(aes(x=as.numeric(m.data$wealth[m.data$large==1])), fill = "blue", alpha = .1) + 
  geom_density(aes(x=as.numeric(m.data$wealth[m.data$large==0])), fill = "red", alpha = .1) + 
  ylab("Matched Set") + 
  xlab("Wealth Index") + 
  theme_bw() +
  scale_x_continuous(limits = c(min(dat$wealth), max(dat$wealth))) +
  ylim(limits = c(0, 0.085)) +
  theme(axis.title=element_text(size=10))

balance.2.m=ggplot() + 
  geom_density(aes(x=as.numeric(m.data$urban[m.data$large==1])), fill = "blue", alpha = .1) + 
  geom_density(aes(x=as.numeric(m.data$urban[m.data$large==0])), fill = "red", alpha = .1) + 
  ylab("") + 
  xlab("Urban") + 
  theme_bw() + 
  scale_x_discrete(expand=c(0.1, 0.5),limits=c("Rural","Urban")) +
  ylim(limits = c(0, 15)) +
  theme(axis.title=element_text(size=10))


balance.3.m=ggplot() + 
  geom_density(aes(x=as.numeric(m.data$munopp[m.data$large==1])), fill = "blue", alpha = .1) + 
  geom_density(aes(x=as.numeric(m.data$munopp[m.data$large==0])), fill = "red", alpha = .1) + 
  ylab("") + 
  xlab("Municipal Opposition") + 
  theme_bw() +
  scale_x_continuous(limits = c(min(dat$munopp), max(dat$munopp))) +
  ylim(limits = c(0, .12)) +
  theme(axis.title=element_text(size=10))


balance.4.m=ggplot() + 
  geom_density(aes(x=as.numeric(m.data$polinv[m.data$large==1])), fill = "blue", alpha = .1) + 
  geom_density(aes(x=as.numeric(m.data$polinv[m.data$large==0])), fill = "red", alpha = .1) + 
  ylab("") + 
  xlab("Political Involvement") + 
  theme_bw() + 
  scale_x_continuous(limits = c(min(dat$polinv), max(dat$polinv))) +
  ylim(limits = c(0, .5)) +
  theme(axis.title=element_text(size=10))



## RAW
balance.1.r=ggplot() + 
  geom_density(aes(x=as.numeric(dat$wealth[dat$large==1])), fill = "blue", alpha = .1) + 
  geom_density(aes(x=as.numeric(dat$wealth[dat$large==0])), fill = "red", alpha = .1) + 
  ylab("Raw Set") + 
  xlab("Wealth Index") + 
  theme_bw() +
  scale_x_continuous(limits = c(min(dat$wealth), max(dat$wealth))) +
  ylim(limits = c(0, 0.085)) +
  theme(axis.title=element_text(size=10))

balance.2.r=ggplot() + 
  geom_density(aes(x=as.numeric(dat$urban[dat$large==1])), fill = "blue", alpha = .1) + 
  geom_density(aes(x=as.numeric(dat$urban[dat$large==0])), fill = "red", alpha = .1) + 
  ylab("") + 
  xlab("Urban") + 
  theme_bw() + 
  scale_x_discrete(expand=c(0.1, 0.5),limits=c("Rural","Urban")) +
  ylim(limits = c(0, 15)) +
  theme(axis.title=element_text(size=10))


balance.3.r=ggplot() + 
  geom_density(aes(x=as.numeric(dat$munopp[dat$large==1])), fill = "blue", alpha = .1) + 
  geom_density(aes(x=as.numeric(dat$munopp[dat$large==0])), fill = "red", alpha = .1) + 
  ylab("") + 
  xlab("Municipal Opposition") + 
  theme_bw() +
  scale_x_continuous(limits = c(min(dat$munopp), max(dat$munopp))) +
  ylim(limits = c(0, .12)) +
  theme(axis.title=element_text(size=10))


balance.4.r=ggplot() + 
  geom_density(aes(x=as.numeric(dat$polinv[dat$large==1])), fill = "blue", alpha = .1) + 
  geom_density(aes(x=as.numeric(dat$polinv[dat$large==0])), fill = "red", alpha = .1) + 
  ylab("") + 
  xlab("Political Involvement") + 
  theme_bw() +
  scale_x_continuous(limits = c(min(dat$polinv), max(dat$polinv))) +
  ylim(limits = c(0, .5)) +
  theme(axis.title=element_text(size=10))


library(cowplot) # install.packages("cowplot")
balance.matched= plot_grid(balance.1.m,balance.2.m,balance.3.m,balance.4.m, nrow = 1, align = "v", scale = 1)
balance.raw= plot_grid(balance.1.r,balance.2.r,balance.3.r,balance.4.r, nrow = 1, align = "v", scale = 1)
plot_grid(balance.matched,balance.raw,  nrow = 2)




##########################
#   Sensivity Analysis
##########################


library(cem)

dat$urban = as.numeric(dat$urban)
imb.m <- imbalance(group = m.data$large, data = m.data[c("wealth", "exc7", "polinv", "urban", "ed", "munopp", "ing4")], weights = m.data$wt)
imb.r <- imbalance(group = dat$large, data = dat[c("wealth", "exc7", "polinv", "urban", "ed", "munopp", "ing4")], weights = dat$wt)

imb.m.d=data.frame(
  L = round(as.numeric(imb.m$tab[,3]),3),
  Diff = round(as.numeric(imb.m$tab[,1]),3),
  Sample = rep("Matched",7),
  Variable = c(c("Wealth Index", 
                 "Perception Of Corruption", 
                 "Political Involvement", 
                 "Urban", 
                 "Education Years", 
                 "Municipal Opposition", 
                 "Democratic Support"))
)
  

imb.r.d=data.frame(
  L = round(as.numeric(imb.r$tab[,3]),3),
  Diff = round(as.numeric(imb.r$tab[,1]),3),
  Sample = rep("Raw",7),
  Variable = c(c("Wealth Index", 
                 "Perception Of Corruption", 
                 "Political Involvement", 
                 "Urban", 
                 "Education Years", 
                 "Municipal Opposition", 
                 "Democratic Support"))
  
)

imb.d = rbind(imb.m.d, imb.r.d)











#######################################
# O L D   D O N T   U S E   B E L O W 
#######################################









######################################################
#            C o r r e l o g r a m s                 #
######################################################
cat("\014")
rm(list=ls())

# load dataset
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/dat.RData")

# Correlation of INCOME and WAGEHALF (NON-MATCHED dataset).
cor(dat$wagehalf, dat$income)

# generating/centering data set
income = dat$income
wagehalf = dat$wagehalf
plot.std = data.frame(income, wagehalf)
plot.std = apply(plot.std, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
municipality = as.character(dat$municipality)
plot.std = data.frame(plot.std, municipality)


plot.std = reshape(plot.std, 
                   varying = c("income", "wagehalf"), 
                   v.names = "score",
                   timevar = "subj", 
                   times = c("income", "wagehalf"), 
                   direction = "long")



## plot
library(ggplot2)
ggplot(plot.std, aes(x = score, 
                     y = id, 
                     colour = subj)) + 
  geom_jitter(width = 0.3, 
              size=0.8, 
              alpha=I(.3)) + 
  coord_flip() + 
  labs(colour = "") + 
  ylab("Observations (unmatched set)") + 
  xlab("Standarized Score") + 
  scale_color_manual(labels = c("Income", "Density of the Poor"), values = c("blue", "red")) +
  theme_bw() + 
  scale_shape(solid = FALSE)


#
munopp <- m.data$munopp
large  <- m.data$large
wagehalf <- m.data$wagehalf
income <- m.data$income
ed <- m.data$ed

m.dataCorr1 <- data.frame(munopp, large, wagehalf, income, ed)

pdf("/Users/hectorbahamonde/RU/research/Clientelism_paper/Paper_Presentation/correlogram.pdf", width = 5, height = 5)
corrgram.plot <- corrgram(m.dataCorr1, order=TRUE, lower.panel=panel.shade,
                          upper.panel=panel.pie, text.panel=panel.txt,
                          main="Correlogram for Matched Data Set")
dev.off()

# For the correlation
m.dataCorr2 <- data.frame(large, income)
corr(m.dataCorr2) # -0.2601744


##########################
#   OLD Plotting Interactions 2 [int:2]
##########################

# Recode Before modeling
m.data$logpop = log(m.data$pop)


# model
library(Zelig, quietly = T)
logit.4.z <- zelig(clien1dummy ~ large + income + exc7 + polinv + urban + logpop + ed + munopp + vb3, 
                   model = "logit.gee", 
                   id = "municipality",
                   cite = FALSE,
                   data = m.data)

# Simulation A x0.2
polinv.range <- min(m.data$polinv):max(m.data$polinv)
logpop.range <- min(m.data$logpop):max(m.data$logpop)

x0.2.A <- setx(logit.4.z, large = 0, polinv = polinv.range)
set.seed(602)
logit.4.z.s2.A <- sim(logit.4.z, x=x0.2.A, num=1000)
logit.4.z.e2.A = data.frame(logit.4.z.s2.A$getqi(qi="ev", xvalue="range")); colnames(logit.4.z.e2.A)<-seq(1:ncol(as.data.frame(t(polinv.range)))) # low

X.scale.2 <- c(mean(logit.4.z.e2.A[,1]), 
               mean(logit.4.z.e2.A[,2]), 
               mean(logit.4.z.e2.A[,3]), 
               mean(logit.4.z.e2.A[,4]), 
               mean(logit.4.z.e2.A[,5]), 
               mean(logit.4.z.e2.A[,6])
)

# Simulation B x1.2
x1.2.B <- setx(logit.4.z, large = 1, polinv = polinv.range)
set.seed(602)
logit.4.z.s2.B <- sim(logit.4.z, x=x1.2.B, num=1000)
logit.4.z.e2.B = data.frame(logit.4.z.s2.B$getqi(qi="ev", xvalue="range")); colnames(logit.4.z.e2.B)<-seq(1:ncol(as.data.frame(t(polinv.range)))) # high

# Plot
library(ggplot2)

wealth.rage = min(dat$wealth):max(dat$wealth)

ggplot() + 
  geom_point(aes(x=polinv.range[1], y=logit.4.z.e2.A[1]), position = position_jitter(width = 5), size = I(5), color = "red", alpha = I(1/100)) +
  geom_point(aes(x=polinv.range[1], y=logit.4.z.e2.B[1]), position = position_jitter(width = 5), size = I(5), color = "blue", alpha = I(1/100)) +
  geom_point(aes(x=polinv.range[2], y=logit.4.z.e2.A[2]), position = position_jitter(width = 5), size = I(5), color = "red", alpha = I(1/100)) +
  geom_point(aes(x=polinv.range[2], y=logit.4.z.e2.B[2]), position = position_jitter(width = 5), size = I(5), color = "blue", alpha = I(1/100)) +
  geom_point(aes(x=polinv.range[3], y=logit.4.z.e2.A[3]), position = position_jitter(width = 5), size = I(5), color = "red", alpha = I(1/100)) +
  geom_point(aes(x=polinv.range[3], y=logit.4.z.e2.B[3]), position = position_jitter(width = 5), size = I(5), color = "blue", alpha = I(1/100)) +
  geom_point(aes(x=polinv.range[4], y=logit.4.z.e2.A[4]), position = position_jitter(width = 5), size = I(5), color = "red", alpha = I(1/100)) +
  geom_point(aes(x=polinv.range[4], y=logit.4.z.e2.B[4]), position = position_jitter(width = 5), size = I(5), color = "blue", alpha = I(1/100)) +
  geom_point(aes(x=polinv.range[5], y=logit.4.z.e2.A[5]), position = position_jitter(width = 5), size = I(5), color = "red", alpha = I(1/100)) +
  geom_point(aes(x=polinv.range[5], y=logit.4.z.e2.B[5]), position = position_jitter(width = 5), size = I(5), color = "blue", alpha = I(1/100)) +
  geom_point(aes(x=polinv.range[6], y=logit.4.z.e2.A[6]), position = position_jitter(width = 5), size = I(5), color = "red", alpha = I(1/100)) +
  geom_point(aes(x=polinv.range[6], y=logit.4.z.e2.B[6]), position = position_jitter(width = 5), size = I(5), color = "blue", alpha = I(1/100)) +
  
  
  
  
  
  
  scale_x_discrete(limits = c(0,1,2,3,4,5), labels=c("0", "1", "2", "3", "4", "5")) +
  xlab("Political Involvement") + ylab("Expected Value of Clientelism") + theme_bw()



##########################
#  OLD Plotting Interactions 1 [int:1]
##########################

# Transformation
m.data$logpop = log(m.data$pop)
polinv.range <- min(m.data$polinv):max(m.data$polinv)
logpop.range <- min(m.data$logpop):max(m.data$logpop)

# Interacted Model
gee.logit.z.5 <- zelig(clien1dummy ~ large + income + exc7 + polinv + urban + logpop + ed + munopp + vb3 + logpop:polinv,
                       model = "logit.gee", 
                       id = "municipality",
                       cite = FALSE,
                       data = m.data)

# Simulations
x0.1.A <- setx(gee.logit.z.5, polinv = quantile(m.data$polinv, .25), logpop = seq(min(m.data$logpop):max(m.data$logpop)))
x0.1.B <- setx(gee.logit.z.5, polinv = quantile(m.data$polinv, .75), logpop = seq(min(m.data$logpop):max(m.data$logpop)))
set.seed(602)
gee.logit.z.5.s1.A <- sim(gee.logit.z.5, x=x0.1.A, num=600)
gee.logit.z.5.s1.B <- sim(gee.logit.z.5, x=x0.1.B, num=600)

gee.logit.z.5.e1.A = data.frame(gee.logit.z.5.s1.A$getqi(qi="ev", xvalue="range")); colnames(gee.logit.z.5.e1.A)<-seq(1:ncol(as.data.frame(t(seq(min(m.data$logpop):max(m.data$logpop)))))) # low
gee.logit.z.5.e1.B = data.frame(gee.logit.z.5.s1.B$getqi(qi="ev", xvalue="range")); colnames(gee.logit.z.5.e1.B)<-seq(1:ncol(as.data.frame(t(seq(min(m.data$logpop):max(m.data$logpop)))))) # high

# Plot
library(ggplot2)
ggplot() + 
  geom_point(aes(x=polinv.range[1], y=gee.logit.z.5.e1.A[1]), position = position_jitter(width = 5), size = I(5), color = "red", alpha = I(1/90)) +
  geom_point(aes(x=polinv.range[1], y=gee.logit.z.5.e1.B[1]), position = position_jitter(width = 5), size = I(5), color = "blue", alpha = I(1/90)) +
  geom_point(aes(x=polinv.range[2], y=gee.logit.z.5.e1.A[2]), position = position_jitter(width = 5), size = I(5), color = "red", alpha = I(1/90)) +
  geom_point(aes(x=polinv.range[2], y=gee.logit.z.5.e1.B[2]), position = position_jitter(width = 5), size = I(5), color = "blue", alpha = I(1/90)) +
  geom_point(aes(x=polinv.range[3], y=gee.logit.z.5.e1.A[3]), position = position_jitter(width = 5), size = I(5), color = "red", alpha = I(1/90)) +
  geom_point(aes(x=polinv.range[3], y=gee.logit.z.5.e1.B[3]), position = position_jitter(width = 5), size = I(5), color = "blue", alpha = I(1/90)) +
  geom_point(aes(x=polinv.range[4], y=gee.logit.z.5.e1.A[4]), position = position_jitter(width = 5), size = I(5), color = "red", alpha = I(1/90)) +
  geom_point(aes(x=polinv.range[4], y=gee.logit.z.5.e1.B[4]), position = position_jitter(width = 5), size = I(5), color = "blue", alpha = I(1/90)) +
  geom_point(aes(x=polinv.range[5], y=gee.logit.z.5.e1.A[5]), position = position_jitter(width = 5), size = I(5), color = "red", alpha = I(1/90)) +
  geom_point(aes(x=polinv.range[5], y=gee.logit.z.5.e1.B[5]), position = position_jitter(width = 5), size = I(5), color = "blue", alpha = I(1/90)) +
  geom_point(aes(x=polinv.range[6], y=gee.logit.z.5.e1.A[6]), position = position_jitter(width = 5), size = I(5), color = "red", alpha = I(1/90)) +
  geom_point(aes(x=polinv.range[6], y=gee.logit.z.5.e1.B[6]), position = position_jitter(width = 5), size = I(5), color = "blue", alpha = I(1/90)) +
  scale_x_discrete(limits = c(0,1,2,3,4,5), labels=c("0", "1", "2", "3", "4", "5")) +
  xlab("Political Involvement") + ylab("Expected Value of Clientelism") + theme_bw()

#########
# OLD TECHNICAL INTERACTION TERMS PLOTS
#########

#1 
library(DAMisc) # install.packages("DAMisc")
logit.int <- glm(clien1dummy ~ large + income + exc7 + polinv + urban + log(pop) + ed + munopp + vb3 + log(pop):polinv,  
                 family=binomial(link="logit"), 
                 data = m.data)

out <- intEff(obj=logit.int, vars=c("log(pop)", "polinv"), data=m.data)

logit.int.p <- plot(jitter(out$phat, amount=0.009), 
                    jitter(out$int_eff, amount=0.009), 
                    xlab="Predicted Probability of Clientelism", 
                    ylab = "Log Pop * Political Involvement",
                    ylim=c(-0.015, 0.030)
)
abline(h=0, col = "red", lty=2, lwd=2.5)
ag <- aggregate(out$linear, list(out$phat), mean)
lines(ag[,1], ag[,2], lty=1, col="red", lwd=3)
legend("bottomright", 
       c("Correct Marginal Effect", "Linear Marginal Effect"), 
       pch=c(1, NA), 
       lty=c(NA, 1), 
       col=c("black", "red"), 
       lwd=c(NA, 2), 
       inset=.01
)

# 2
logit.int <- glm(clien1dummy ~ large + income + exc7 + polinv + urban + log(pop) + ed + munopp + vb3 + large:polinv,  
                 family=binomial(link="logit"), 
                 data = m.data)

out <- intEff(obj=logit.int, vars=c("large", "polinv"), data=m.data)

logit.int.p <- plot(jitter(out$phat, amount=0.009), 
                    jitter(out$int_eff, amount=0.009), 
                    xlab="Predicted Probability of Clientelism", 
                    ylab = "High Density * Political Involvement",
                    ylim=c(-0.015, 0.064)
)
abline(h=0, col = "red", lty=2, lwd=2.5)
ag <- aggregate(out$linear, list(out$phat), mean)
lines(ag[,1], ag[,2], lty=1, col="red", lwd=3)
legend("bottomright", 
       c("Correct Marginal Effect", "Linear Marginal Effect"), 
       pch=c(1, NA), 
       lty=c(NA, 1), 
       col=c("black", "red"), 
       lwd=c(NA, 2), 
       inset=.01
)

# 3

logit.int <- glm(clien1dummy ~ large + income + exc7 + polinv + urban + log(pop) + ed + munopp + vb3 + munopp:income,  
                 family=binomial(link="logit"), 
                 data = m.data)

out <- intEff(obj=logit.int, vars=c("munopp", "income"), data=m.data)

set.seed(604)
logit.int.p <- plot(jitter(out$phat, amount=0.009), 
                    jitter(out$int_eff, amount=0.009), 
                    xlab="Predicted Probability of Clientelism", 
                    ylab = "Municipal Opposition * Income",
                    ylim=c(-0.015, 0.02)
)
abline(h=0, col = "red", lty=2, lwd=2.5)
ag <- aggregate(out$linear, list(out$phat), mean)
lines(ag[,1], ag[,2], lty=1, col="red", lwd=3)
legend("bottomright", 
       c("Correct Marginal Effect", "Linear Marginal Effect"), 
       pch=c(1, NA), 
       lty=c(NA, 1), 
       col=c("black", "red"), 
       lwd=c(NA, 2), 
       inset=.01
)






######################################################
#     OLD P  a r a m e t r i c   O  L D   D O N T   U S E 
######################################################

cat("\014")
rm(list=ls())


# load dataset
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")

# Recode client1dummy after matching
#m.data <- match.data(m.out)
#m.data$clien1dummy <- as.numeric(m.data$clien1dummy)
#m.data$clien1dummy <- recode(m.data$clien1dummy, "1 = 0 ; 2 = 1")

# transform this: simulation methods still don't support log(var).
m.data$logpop = log(m.data$pop)



# Models

# METHOD 1
#source("http://bioconductor.org/biocLite.R")
#biocLite("graph")
#biocLite("Rgraphviz")
#install.packages("Zelig")
#install.packages("ZeligChoice")

# METHOD 2
# library(devtools) # install.packages("devtools")
# install_github('IQSS/Zelig')
#library(Zelig) # install.packages("Zelig", dependencies=TRUE) # Models

#relogit.1 <- zelig(clien1dummy ~ large,
#                   model = "relogit", 
#                   #robust = TRUE,
#                   cite = FALSE,
#                   data = m.data)

#relogit.2 <- zelig(clien1dummy ~ large + exc7 + polinv + urban + logpop + ed,
#                   model = "relogit", 
#                   #robust = TRUE,
#                   cite = FALSE,
#                   data = m.data)

#relogit.3 <- zelig(clien1dummy ~ income + exc7 + polinv + urban + logpop + ed,
#                  model = "relogit", 
#                  #robust = TRUE,
#                  cite = FALSE,
#                  data = m.data)

#relogit.4 <- zelig(clien1dummy ~ large + income + exc7 + polinv + urban + logpop + ed + munopp + vb3, 
#                  model = "relogit", 
#                  #robust = TRUE,
#                  cite = FALSE,
#                  data = m.data)

#relogit.5 <- zelig(clien1dummy ~ large + income + exc7 + polinv + urban + logpop + ed + munopp + vb3 + logpop:polinv,
#                  model = "relogit", 
#                  #robust = TRUE,
#                  cite = FALSE,
#                  data = m.data)

#relogit.6 <- zelig(clien1dummy ~ large + income + exc7 + polinv + urban + logpop + ed + munopp + vb3 + munopp:income, 
#                  model = "relogit", 
#                  #robust = TRUE,
#                  cite = FALSE,
#                  data = m.data)



# Models

## sort data first
m.data$municipality = sort(m.data$municipality)


library(geepack) # install.packages("geepack")

logit.gee.1 <- geeglm(clien1dummy ~ large,
                      family = binomial(link = "probit"), 
                      id = municipality, 
                      corstr = "independence",
                      data = m.data)

logit.gee.2 <- geeglm(clien1dummy ~ large + exc7 + polinv + urban + logpop + ed,
                      family = binomial(link = "probit"), 
                      id = municipality, 
                      corstr = "independence",
                      data = m.data)

logit.gee.3 <- geeglm(clien1dummy ~ wealth + exc7 + polinv + urban + logpop + ed ,
                      family = binomial(link = "probit"), 
                      id = municipality, 
                      corstr = "independence",
                      data = m.data)

logit.gee.4 <- geeglm(clien1dummy ~ large + wealth + exc7 + polinv + urban + logpop + ed + munopp + vb3 , 
                      family = binomial(link = "probit"), 
                      id = municipality, 
                      corstr = "independence",
                      data = m.data)

logit.gee.5 <- geeglm(clien1dummy ~ large + wealth + exc7 + polinv + urban + logpop + ed + munopp + vb3 + logpop:polinv ,
                      family = binomial(link = "probit"), 
                      id = municipality, 
                      corstr = "independence",
                      data = m.data)

logit.gee.6 <- geeglm(clien1dummy ~ large + wealth + exc7 + polinv + urban + logpop + ed + munopp + vb3 + munopp:income , 
                      family = binomial(link = "probit"), 
                      id = municipality, 
                      corstr = "independence",
                      data = m.data)

## ---- texreg-extractor-geeglm ----
extract.geepack <- function(model) {
  s <- summary(model)
  names <- rownames(s$coef)
  co <- s$coef[, 1]
  se <- s$coef[, 2]
  pval <- s$coef[, 4]
  
  n <- nrow(model.frame(model))
  nclust <- length(s$geese$clusz)
  
  gof = c(n, nclust)
  gof.names = c("Num. obs.", "Num. clust.")
  
  tr <- createTexreg(
    coef.names = names,
    coef = co,
    se = se,
    pvalues = pval,
    gof.names = gof.names,
    gof = gof,
    gof.decimal = rep(FALSE, length(gof))
  )
  return(tr)
}


library(texreg)
logit.gee.1.d = extract.geepack(logit.gee.1)
logit.gee.2.d = extract.geepack(logit.gee.2)
logit.gee.3.d = extract.geepack(logit.gee.3)
logit.gee.4.d = extract.geepack(logit.gee.4)
logit.gee.5.d = extract.geepack(logit.gee.5)
logit.gee.6.d = extract.geepack(logit.gee.6)




library(texreg)
screenreg(
  list(logit.gee.1.d, logit.gee.2.d, logit.gee.3.d, logit.gee.4.d, logit.gee.5.d, logit.gee.6.d),
  #custom.coef.names = c(# this gotta be before OMIT.COEFF
  #       "(Intercept)",
  #      "High Density",
  #     "Perception of Corruption",
  #    "Political Involvement",
  #   "Urban",
  #  "Population (ln)",
  # "Education",
  #                #"Individual Income",
  #               "Municipal Opposition",
  #              "Political Id",
  #             "Political Involvement TIMES Population (ln)",
  #            "Individual Income TIMES Municipal Opposition"),
  caption = "Likelihood of Clientelism: Generalized Estimating Equations",
  label = "tab:1",
  #override.se = logit.robust.se,
  #override.pvalues = logit.pval,
  #override.ci.low = logit.robust.se.upper,
  #override.ci.up = logit.robust.se.lower,
  stars = c(0.01, 0.05, 0.1),
  digits = 3,
  custom.note = "%stars. \n Clustered Std. Errors at the Municipality Level. \n Matched sample. \n 95% Standard errors in parentheses",
  fontsize = "scriptsize",
  float.pos = "h"
)



##########################
# Simulation and Plot 1 OLD DONT USE
##########################

# Match Data
m.data <- match.data(m.out)


# Recode client1dummy after matching
library(car) # install.packages("car") 
m.data$clien1dummy <- as.numeric(m.data$clien1dummy)
m.data$clien1dummy <- recode(m.data$clien1dummy, "1 = 0 ; 2 = 1")

# model
## sort data first
m.data$municipality = sort(m.data$municipality)

library(Zelig, quietly = T)
logit.gee.4.z <- zelig(clien1dummy ~ large + income + exc7 + polinv + urban + logpop + ed + munopp + vb3, 
                       model = "logit.gee", 
                       id = "municipality",
                       cite = FALSE,
                       data = m.data)

x <- setx(logit.gee.4.z, large = 0)  
x1 <- setx(logit.gee.4.z, large = 1) 

sim.large2 <- sim(logit.gee.4.z, x = x, x1 = x1, num=2500)

print(summary(sim.large2))

par(mar = rep(2, 4))
plot(sim.large2)


#######################################################################
# Simulation and Plot 2 O L D   D O N T   U S E 
#######################################################################

# Match Data
m.data <- match.data(m.out)


# Recode client1dummy after matching
library(car) # install.packages("car") 
m.data$clien1dummy <- as.numeric(m.data$clien1dummy)
m.data$clien1dummy <- recode(m.data$clien1dummy, "1 = 0 ; 2 = 1")

# model
## sort data first
m.data$municipality = sort(m.data$municipality)

library(Zelig, quietly = T)
logit.gee.4.z <- zelig(clien1dummy ~ large + income + exc7 + polinv + urban + logpop + ed + munopp + vb3, 
                       model = "logit.gee", 
                       id = "municipality",
                       cite = FALSE,
                       data = m.data)


# Simulation
large.0 <- setx(logit.gee.4.z, large =0)
large.1 <- setx(logit.gee.4.z, large =1)
set.seed(602)
Model.large.s <- sim(logit.gee.4.z, x = large.0, x1 = large.1, num=250000)

large.0.s = data.frame(Model.large.s$getqi(qi="ev", xvalue="x")); colnames(large.0.s)[1] <- "X1"
large.1.s = data.frame(Model.large.s$getqi(qi="ev", xvalue="x1")); colnames(large.1.s)[1] <- "X2"
Model.large.e = cbind(large.0.s,large.1.s)


X1 <- Model.large.e$X1
X2 <- Model.large.e$X2

# Plot
ggplot.labels2 <- data.frame(
  time = c(mean(Model.large.e$X1), mean(Model.large.e$X2), mean(Model.large.e$X1), mean(Model.large.e$X2)), 
  value = c(14, 17, 11, 15), 
  label = c("Low", "High", round(mean(Model.large.e$X1), 3),round(mean(Model.large.e$X2), 3)), 
  type = c("NA*", "MVH")
)

library(ggplot2) # install.packages("ggplot2")
ggplot() + 
  geom_density(aes(x=X1), data= Model.large.e,  fill="grey", alpha = .9, linetype="dotted") + 
  geom_density(aes(x=X2), data=Model.large.e, fill="dark grey", alpha = .8) +
  xlab("") + ylab("") +
  theme_bw() +
  geom_segment(data= Model.large.e, aes(x = mean(X1), y = 0, xend = mean(X1), yend = 10), linetype="dashed", size=0.9, colour = "forestgreen") + 
  geom_segment(data= Model.large.e, aes(x = mean(X2), y = 0, xend = mean(X2), yend = 10), linetype="dashed", size=0.9, colour = "forestgreen") + 
  theme(legend.title=element_blank()) +
  geom_text(data = ggplot.labels2, aes(x = time, y = value, label = label),colour = "black", size=4)

#######################################################################
#   R O B U S T N E S S   C H E C K S   D O N T   U S E    #
#######################################################################

# Recode client1dummy after matching
m.data <- match.data(m.out)
m.data$clien1dummy <- as.numeric(m.data$clien1dummy)
m.data$clien1dummy <- recode(m.data$clien1dummy, "1 = 0 ; 2 = 1")


m.4.logit <- zelig(clien1dummy ~ large + income + exc7 + polinv + urban + log(pop) + ed + munopp + vb3, 
                   model = "logit", 
                   robust = TRUE,
                   cite = FALSE,
                   data = m.data)

m.4.probit <- zelig(clien1dummy ~ large + income + exc7 + polinv + urban + log(pop) + ed + munopp + vb3, 
                    model = "probit", 
                    robust = TRUE,
                    cite = FALSE,
                    data = m.data)


m.4.zip <- zeroinfl(clien1dummy ~ large + exc7 + polinv + urban + log(pop) + ed + munopp + vb3 | income + large, data = m.data,  link = "logit")

stargazer(m.4.logit, m.4.probit,m.4.zip, 
          type = "text", # change to "latex"/"text" when nedded
          covariate.labels=c(
            "High Density", 
            "Income",
            "Perception of Corruption",
            "Political Involvement Index",
            "Urban",
            "Population (ln)",
            "Years of Schooling",
            "Municipal Opposition",
            "Political Id"),
          dep.var.labels=c("Clientelism"),
          label = "tab:2",
          zero.component=F,
          notes = "Robust Std. Errors in Parentheses (logit and probit)",
          title = "Robustness Checks [main model]",
          font.size = "scriptsize",
          table.placement = "h",
          notes.align = "c",
          style = "apsr")


##############################################################################
#   O L D   G P S     W e i g h t i n g   M O D E L S   D O N T   U S E 
##############################################################################
cat("\014")
rm(list=ls())


load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/dat.RData")

# Generating the Propensity Score 
library(CBPS, quietly = T) # install.packages("CBPS")

fit <- CBPS(wagehalf ~ wealth + ed + polinv, data = dat, iterations = 25000)
# Attaching weights to DF
dat$weights = round(fit$weights, digits=5)



# Recode Before modeling
dat$clien1dummy <- as.numeric(dat$clien1dummy)
library(car)
dat$clien1dummy <- recode(dat$clien1dummy, "1 = 0 ; 2 = 1")
dat$logpop = log(dat$pop)


# Models
# gps.1 <- glm(clien1dummy ~ wagehalf + polinv + income + munopp:income + logpop:polinv + weights,
#              family =binomial(link = "logit"),
#             data = dat)

# gps.2 <- glm(clien1dummy ~ wagehalf + exc7 + polinv  + ed + weights,
#              family =binomial(link = "logit"),
#             data = dat)

# gps.3 <- glm(clien1dummy ~ wealth + exc7 + polinv  + ed + weights,
#              family =binomial(link = "logit"),
#              data = dat)

# gps.4 <- glm(clien1dummy ~ wagehalf + wealth + exc7 + polinv + ed + munopp + vb3 + weights, 
#              family =binomial(link = "logit"),
#             data = dat)

# gps.5 <- glm(clien1dummy ~ wagehalf + wealth + exc7 + polinv + ed + munopp + vb3 + logpop:polinv + weights,
#             family =binomial(link = "logit"),
#             data = dat)

# gps.6 <- glm(clien1dummy ~ wagehalf + wealth + exc7 + polinv + ed + munopp + vb3 + munopp:income + weights, 
#             family =binomial(link = "logit"),
#             data = dat)

# function that does clustered SEs
vcovCluster <- function(
  model,
  cluster
)
{
  require(sandwich)
  require(lmtest)
  if(nrow(model.matrix(model))!=length(cluster)){
    stop("check your data: cluster variable has different N than model")
  }
  M <- length(unique(cluster))
  N <- length(cluster)           
  K <- model$rank   
  if(M<50){
    warning("Fewer than 50 clusters, variances may be unreliable (could try block bootstrap instead).")
  }
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj  <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
  rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
  return(rcse.cov)
}


library(lmtest)
library(sandwich)
library(msm) # install.packages("msm")



gps.1.robust = coeftest(gps.1, vcov = vcovCluster(gps.1, cluster = as.numeric(dat$municipality)))
gps.2.robust = coeftest(gps.2, vcov = vcovCluster(gps.2, cluster = as.numeric(dat$municipality)))
gps.3.robust = coeftest(gps.3, vcov = vcovCluster(gps.3, cluster = as.numeric(dat$municipality)))
gps.4.robust = coeftest(gps.4, vcov = vcovCluster(gps.4, cluster = as.numeric(dat$municipality)))
gps.5.robust = coeftest(gps.5, vcov = vcovCluster(gps.5, cluster = as.numeric(dat$municipality)))
#gps.6.robust = coeftest(gps.6, vcov = vcovCluster(gps.6, cluster = as.numeric(dat$municipality)))


gps.robust.se = list(c(gps.1.robust[4],gps.1.robust[5], gps.1.robust[6]),
                     c(gps.2.robust[9],gps.2.robust[10], gps.2.robust[11], gps.2.robust[12], gps.2.robust[13], gps.2.robust[14],gps.2.robust[15], gps.2.robust[16]),
                     c(gps.3.robust[9], gps.3.robust[10], gps.3.robust[11], gps.3.robust[12], gps.3.robust[13], gps.3.robust[14], gps.3.robust[15], gps.3.robust[16]),
                     c(gps.4.robust[12], gps.4.robust[13], gps.4.robust[14], gps.4.robust[15], gps.4.robust[16], gps.4.robust[17], gps.4.robust[18], gps.4.robust[19],  gps.4.robust[20],  gps.4.robust[21], gps.4.robust[22]),
                     c(gps.5.robust[13],gps.5.robust[14],gps.5.robust[15],gps.5.robust[16],gps.5.robust[17],gps.5.robust[18],gps.5.robust[19],gps.5.robust[20], gps.5.robust[21], gps.5.robust[22], gps.5.robust[23], gps.5.robust[24]),
                     c(gps.6.robust[13], gps.6.robust[14], gps.6.robust[15], gps.6.robust[16], gps.6.robust[17], gps.6.robust[18], gps.6.robust[19], gps.6.robust[20], gps.6.robust[21], gps.6.robust[22], gps.6.robust[23], gps.6.robust[24])
)

gps.coeffs = list(c(gps.1.robust[1],gps.1.robust[2], gps.1.robust[3]),
                  c(gps.2.robust[1],gps.2.robust[2], gps.2.robust[3], gps.2.robust[4], gps.2.robust[5], gps.2.robust[6],gps.2.robust[7], gps.2.robust[8]),
                  c(gps.3.robust[1], gps.3.robust[2], gps.3.robust[3], gps.3.robust[4], gps.3.robust[5], gps.3.robust[6], gps.3.robust[7], gps.3.robust[8]),
                  c(gps.4.robust[1], gps.4.robust[2], gps.4.robust[3], gps.4.robust[4], gps.4.robust[5], gps.4.robust[6], gps.4.robust[7], gps.4.robust[8],  gps.4.robust[9],  gps.4.robust[10], gps.4.robust[11]),
                  c(gps.5.robust[1],gps.5.robust[2],gps.5.robust[3],gps.5.robust[4],gps.5.robust[5],gps.5.robust[6],gps.5.robust[7],gps.5.robust[8], gps.5.robust[9], gps.5.robust[10], gps.5.robust[11], gps.5.robust[12]),
                  c(gps.6.robust[1], gps.6.robust[2], gps.6.robust[3], gps.6.robust[4], gps.6.robust[5], gps.6.robust[6], gps.6.robust[7], gps.6.robust[8], gps.6.robust[9], gps.6.robust[10], gps.6.robust[11], gps.6.robust[12])
)

gps.robust.se.upper = list(
  c(gps.coeffs[[1]] + 1.96*gps.robust.se[[1]]),
  c(gps.coeffs[[2]] + 1.96*gps.robust.se[[2]]),
  c(gps.coeffs[[3]] + 1.96*gps.robust.se[[3]]),
  c(gps.coeffs[[4]] + 1.96*gps.robust.se[[4]]),
  c(gps.coeffs[[5]] + 1.96*gps.robust.se[[5]]),
  c(gps.coeffs[[6]] + 1.96*gps.robust.se[[6]])
)



gps.robust.se.lower = list(
  c(gps.coeffs[[1]] - 1.96*gps.robust.se[[1]]),
  c(gps.coeffs[[2]] - 1.96*gps.robust.se[[2]]),
  c(gps.coeffs[[3]] - 1.96*gps.robust.se[[3]]),
  c(gps.coeffs[[4]] - 1.96*gps.robust.se[[4]]),
  c(gps.coeffs[[5]] - 1.96*gps.robust.se[[5]]),
  c(gps.coeffs[[6]] - 1.96*gps.robust.se[[6]])
)

gps.pval = list(
  c(gps.1.robust[10], gps.1.robust[11], gps.1.robust[12]),
  c(gps.2.robust[25], gps.2.robust[26],gps.2.robust[27],gps.2.robust[28],gps.2.robust[29],gps.2.robust[30],gps.2.robust[31],gps.2.robust[32]),
  c(gps.3.robust[25], gps.3.robust[26],gps.3.robust[27],gps.3.robust[28],gps.3.robust[29],gps.3.robust[30],gps.3.robust[31],gps.3.robust[32]),
  c(gps.4.robust[34],gps.4.robust[35],gps.4.robust[36],gps.4.robust[37],gps.4.robust[38],gps.4.robust[39],gps.4.robust[40],gps.4.robust[41],gps.4.robust[42],gps.4.robust[43],gps.4.robust[44]),
  c(gps.5.robust[37],gps.5.robust[38],gps.5.robust[39],gps.5.robust[40],gps.5.robust[41],gps.5.robust[42],gps.5.robust[43],gps.5.robust[44],gps.5.robust[45],gps.5.robust[46],gps.5.robust[47],gps.5.robust[48]),
  c(gps.6.robust[37],gps.6.robust[38],gps.6.robust[39],gps.6.robust[40],gps.6.robust[41],gps.6.robust[42],gps.6.robust[43],gps.6.robust[44],gps.6.robust[45],gps.6.robust[46],gps.6.robust[47],gps.6.robust[48])
)



library(texreg)
screenreg(
  list(gps.1, gps.2, gps.3, gps.4, gps.5, gps.6),
  #        custom.coef.names = c(# this gotta be before OMIT.COEFF
  #                "(Intercept)",
  #                "High Density",
  #                "weights",
  #                "Perception of Corruption",
  #                "Political Involvement",
  #                "Urban",
  #                "Population (ln)",
  #                "Education",
  #                "Individual Income",
  #                "Municipal Opposition",
  #                "Political Id",
  #                "Political Involvement TIMES Population (ln)",
  #                "Individual Income TIMES Municipal Opposition"),
  omit.coef = "weights", # this gotta be AFTER custo.coef.names
  caption = "Models using the Generalized Propensity Score as a weighting device - Unmatched Sample ",
  label = "results.gps:1",
  override.se = gps.robust.se,
  override.pvalues = gps.pval,
  #override.ci.low = gps.robust.se.upper,
  #override.ci.up = gps.robust.se.lower,
  stars = c(0.01, 0.05, 0.1),
  digits = 3,
  custom.note = "%stars. \n Robust Standard Errors in All Models. \n Raw sample. \n 95% Confidence Intervals in brackets.",
  fontsize = "scriptsize",
  float.pos = "h"
)


###

## ---- texreg-extractor-Relogit ZELIG ----
library(texreg)
extract.Zeligrelogit <- function(model, include.aic = TRUE, include.bic = TRUE, 
                                 include.loglik = TRUE, include.deviance = TRUE, include.nobs = TRUE, ...) {
  g <- model$zelig.out$z.out[[1]]
  class(g) <- "glm"
  e <- extract(g, include.aic = include.aic, include.bic = include.bic, 
               include.loglik = include.loglik, include.deviance = include.deviance, 
               include.nobs = include.nobs, ...)
  return(e)
}

setMethod("extract", signature = className("Zelig-relogit", "Zelig"), 
          definition = extract.Zeligrelogit)


## **OLD DONT USE***
##########################
#   GPS  Weighting PLOT: GEE LOGIT [gps:wagehalf_polinv1] WAGE HALF and POLINV
##########################

# METHOD 2
# library(devtools) # install.packages("devtools")
# install_github('IQSS/Zelig')
#library(Zelig) # install.packages("Zelig", dependencies=TRUE) # Models

# Simulation
library(Zelig)
low.wagehalf.gee.polinv <- setx(gee.logit.gps.zelig, wagehalf = mean(dat$wagehalf)-sd(dat$wagehalf), polinv = min(dat$polinv):max(dat$polinv))
high.wagehalf.gee.polinv <- setx(gee.logit.gps.zelig, wagehalf = mean(dat$wagehalf)+sd(dat$wagehalf), polinv = min(dat$polinv):max(dat$polinv))




set.seed(602); options(scipen=999)
gps.logit.gee.wagehalf.polinv.s <- sim(gps.logit.gee, x = low.wagehalf.gee.polinv, x1 = high.wagehalf.gee.polinv, num=500)


gps.logit.gee.wagehalf.polinv.s.d.low = data.frame(gps.logit.gee.wagehalf.polinv.s$getqi(qi="ev", xvalue="range")) ; colnames(gps.logit.gee.wagehalf.polinv.s.d.low) <- seq(1:ncol(as.data.frame(t(min(dat$polinv):max(dat$polinv)))))  # low
gps.logit.gee.wagehalf.polinv.s.d.high = data.frame(gps.logit.gee.wagehalf.polinv.s$getqi(qi="ev", xvalue="range1")) ; colnames(gps.logit.gee.wagehalf.polinv.s.d.high) <- seq(1:ncol(as.data.frame(t(min(dat$polinv):max(dat$polinv)))))  # high



polinv.range = as.numeric(min(dat$polinv):max(dat$polinv))

library(ggplot2) ; library(gridExtra)
set.seed(602)
grid.arrange(ggplot() + 
               geom_point(aes(x=polinv.range[1], y=gps.logit.gee.wagehalf.polinv.s.d.low[1], color = "Low"), position = position_jitter(width = 3), size = I(5), alpha = 1/100) +
               geom_point(aes(x=polinv.range[2], y=gps.logit.gee.wagehalf.polinv.s.d.low[2], color = "Low"), position = position_jitter(width = 3), size = I(5), alpha = 1/100) +
               geom_point(aes(x=polinv.range[3], y=gps.logit.gee.wagehalf.polinv.s.d.low[3], color = "Low"), position = position_jitter(width = 3), size = I(5), alpha = 1/100) + 
               geom_point(aes(x=polinv.range[4], y=gps.logit.gee.wagehalf.polinv.s.d.low[4], color = "Low"), position = position_jitter(width = 3), size = I(5), alpha = 1/100) +
               geom_point(aes(x=polinv.range[5], y=gps.logit.gee.wagehalf.polinv.s.d.low[5], color = "Low"), position = position_jitter(width = 3), size = I(5), alpha = 1/100) +
               geom_point(aes(x=polinv.range[6], y=gps.logit.gee.wagehalf.polinv.s.d.low[6], color = "Low"), position = position_jitter(width = 3), size = I(5), alpha = 1/100) +
               geom_point(aes(x=polinv.range[7], y=gps.logit.gee.wagehalf.polinv.s.d.low[7], color = "Low"), position = position_jitter(width = 3), size = I(5), alpha = 1/100) +
               geom_point(aes(x=polinv.range[8], y=gps.logit.gee.wagehalf.polinv.s.d.low[8], color = "Low"), position = position_jitter(width = 3), size = I(5), alpha = 1/100) +
               geom_point(aes(x=polinv.range[9], y=gps.logit.gee.wagehalf.polinv.s.d.low[9], color = "Low"), position = position_jitter(width = 3), size = I(5), alpha = 1/100) +
               geom_point(aes(x=polinv.range[10], y=gps.logit.gee.wagehalf.polinv.s.d.low[10], color = "Low"), position = position_jitter(width = 3), size = I(5), alpha = 1/100) +
               geom_point(aes(x=polinv.range[1], y=gps.logit.gee.wagehalf.polinv.s.d.high[1], color = "High"), position = position_jitter(width = 3), size = I(5), alpha = 1/100) +
               geom_point(aes(x=polinv.range[2], y=gps.logit.gee.wagehalf.polinv.s.d.high[2], color = "High"), position = position_jitter(width = 3), size = I(5), alpha = 1/100) +
               geom_point(aes(x=polinv.range[3], y=gps.logit.gee.wagehalf.polinv.s.d.high[3], color = "High"), position = position_jitter(width = 3), size = I(5), alpha = 1/100) + 
               geom_point(aes(x=polinv.range[4], y=gps.logit.gee.wagehalf.polinv.s.d.high[4], color = "High"), position = position_jitter(width = 3), size = I(5), alpha = 1/100) +
               geom_point(aes(x=polinv.range[5], y=gps.logit.gee.wagehalf.polinv.s.d.high[5], color = "High"), position = position_jitter(width = 3), size = I(5), alpha = 1/100) +
               geom_point(aes(x=polinv.range[6], y=gps.logit.gee.wagehalf.polinv.s.d.high[6], color = "High"), position = position_jitter(width = 3), size = I(5), alpha = 1/100) +
               geom_point(aes(x=polinv.range[7], y=gps.logit.gee.wagehalf.polinv.s.d.high[7], color = "High"), position = position_jitter(width = 3), size = I(5), alpha = 1/100) +
               geom_point(aes(x=polinv.range[8], y=gps.logit.gee.wagehalf.polinv.s.d.high[8], color = "High"), position = position_jitter(width = 3), size = I(5), alpha = 1/100) +
               geom_point(aes(x=polinv.range[9], y=gps.logit.gee.wagehalf.polinv.s.d.high[9], color = "High"), position = position_jitter(width = 3), size = I(5), alpha = 1/100) +
               geom_point(aes(x=polinv.range[10], y=gps.logit.gee.wagehalf.polinv.s.d.high[10], color = "High"), position = position_jitter(width = 3), size = I(5), alpha = 1/100) +
               xlab("Political Involvement Index") + ylab("Expected Value of Clientelism") + 
               theme_bw() + 
               labs(colour = "Density of the Poor")  +
               theme(legend.key = element_rect(colour = NA, fill = NA, size = 0.5)),
             bottom = "Notes: Logit GEE model: Std. clustered errors at the municipal level. \n No cutoff was used. Sample matched on the generalized propensity score. \n Low and High represent one SD below/above the mean.")




## **OLD DONT USE***
##########################
#   GPS  Weighting PLOT: RE-LOGIT [gps:wagehalf_polinv2] WAGE HALF and POLINV
##########################

# METHOD 2
# library(devtools) # install.packages("devtools")
# install_github('IQSS/Zelig')
#library(Zelig) # install.packages("Zelig", dependencies=TRUE) # Models


# Simulation
library(Zelig)
low.wagehalf.relogit.polinv <- setx(relogit.gps.1, wagehalf = mean(dat$wagehalf)-sd(dat$wagehalf), polinv = min(dat$polinv):max(dat$polinv))
high.wagehalf.relogit.polinv <- setx(relogit.gps.1, wagehalf = mean(dat$wagehalf)+sd(dat$wagehalf), polinv = min(dat$polinv):max(dat$polinv))




set.seed(602); options(scipen=999)
gps.relogit.wagehalf.polinv.s <- sim(relogit.gps.1, x = low.wagehalf.relogit.polinv, x1 = high.wagehalf.relogit.polinv, num=500)


gps.relogit.wagehalf.polinv.s.d.low = data.frame(gps.relogit.wagehalf.polinv.s$getqi(qi="ev", xvalue="range")) ; colnames(gps.relogit.wagehalf.polinv.s.d.low) <- seq(1:ncol(as.data.frame(t(min(dat$polinv):max(dat$polinv)))))  # low
gps.relogit.wagehalf.polinv.s.d.high = data.frame(gps.relogit.wagehalf.polinv.s$getqi(qi="ev", xvalue="range1")) ; colnames(gps.relogit.wagehalf.polinv.s.d.high) <- seq(1:ncol(as.data.frame(t(min(dat$polinv):max(dat$polinv)))))  # high



polinv.range = as.numeric(min(dat$polinv):max(dat$polinv))



library(ggplot2) ; library(gridExtra)
set.seed(602)
grid.arrange(ggplot() + 
               geom_point(aes(x=polinv.range[1], y=gps.relogit.wagehalf.polinv.s.d.low[1], color = "Low"), position = position_jitter(width = 3), size = I(4), alpha = 1/100) +
               geom_point(aes(x=polinv.range[2], y=gps.relogit.wagehalf.polinv.s.d.low[2], color = "Low"), position = position_jitter(width = 3), size = I(4), alpha = 1/100) +
               geom_point(aes(x=polinv.range[3], y=gps.relogit.wagehalf.polinv.s.d.low[3], color = "Low"), position = position_jitter(width = 3), size = I(4), alpha = 1/100) + 
               geom_point(aes(x=polinv.range[4], y=gps.relogit.wagehalf.polinv.s.d.low[4], color = "Low"), position = position_jitter(width = 3), size = I(4), alpha = 1/100) +
               geom_point(aes(x=polinv.range[5], y=gps.relogit.wagehalf.polinv.s.d.low[5], color = "Low"), position = position_jitter(width = 3), size = I(4), alpha = 1/100) +
               geom_point(aes(x=polinv.range[6], y=gps.relogit.wagehalf.polinv.s.d.low[6], color = "Low"), position = position_jitter(width = 3), size = I(4), alpha = 1/100) +
               geom_point(aes(x=polinv.range[7], y=gps.relogit.wagehalf.polinv.s.d.low[7], color = "Low"), position = position_jitter(width = 3), size = I(4), alpha = 1/100) +
               geom_point(aes(x=polinv.range[8], y=gps.relogit.wagehalf.polinv.s.d.low[8], color = "Low"), position = position_jitter(width = 3), size = I(4), alpha = 1/100) +
               geom_point(aes(x=polinv.range[9], y=gps.relogit.wagehalf.polinv.s.d.low[9], color = "Low"), position = position_jitter(width = 3), size = I(4), alpha = 1/100) +
               geom_point(aes(x=polinv.range[10], y=gps.relogit.wagehalf.polinv.s.d.low[10], color = "Low"), position = position_jitter(width = 3), size = I(4), alpha = 1/100) +
               geom_point(aes(x=polinv.range[1], y=gps.relogit.wagehalf.polinv.s.d.high[1], color = "High"), position = position_jitter(width = 3), size = I(4), alpha = 1/100) +
               geom_point(aes(x=polinv.range[2], y=gps.relogit.wagehalf.polinv.s.d.high[2], color = "High"), position = position_jitter(width = 3), size = I(4), alpha = 1/100) +
               geom_point(aes(x=polinv.range[3], y=gps.relogit.wagehalf.polinv.s.d.high[3], color = "High"), position = position_jitter(width = 3), size = I(4), alpha = 1/100) + 
               geom_point(aes(x=polinv.range[4], y=gps.relogit.wagehalf.polinv.s.d.high[4], color = "High"), position = position_jitter(width = 3), size = I(4), alpha = 1/100) +
               geom_point(aes(x=polinv.range[5], y=gps.relogit.wagehalf.polinv.s.d.high[5], color = "High"), position = position_jitter(width = 3), size = I(4), alpha = 1/100) +
               geom_point(aes(x=polinv.range[6], y=gps.relogit.wagehalf.polinv.s.d.high[6], color = "High"), position = position_jitter(width = 3), size = I(4), alpha = 1/100) +
               geom_point(aes(x=polinv.range[7], y=gps.relogit.wagehalf.polinv.s.d.high[7], color = "High"), position = position_jitter(width = 3), size = I(4), alpha = 1/100) +
               geom_point(aes(x=polinv.range[8], y=gps.relogit.wagehalf.polinv.s.d.high[8], color = "High"), position = position_jitter(width = 3), size = I(4), alpha = 1/100) +
               geom_point(aes(x=polinv.range[9], y=gps.relogit.wagehalf.polinv.s.d.high[9], color = "High"), position = position_jitter(width = 3), size = I(4), alpha = 1/100) +
               geom_point(aes(x=polinv.range[10], y=gps.relogit.wagehalf.polinv.s.d.high[10], color = "High"), position = position_jitter(width = 3), size = I(4), alpha = 1/100) +
               xlab("Political Involvement Index") + ylab("Expected Value of Clientelism") + 
               theme_bw() + 
               labs(colour = "Density of the Poor")  +
               theme(legend.key = element_rect(colour = NA, fill = NA, size = 0.5)),
             bottom = "Notes: Rare Event Logistic model. \n No cutoff was used. Sample matched on the generalized propensity score. \n Low and High represent one SD below/above the mean.")

