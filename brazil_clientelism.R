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

# save matched dataset
save(m.data, file = "/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")


#####################################################################
### PARAMETRIC models
#####################################################################



# 1
################################################
# gee.dich.m.X: DICH // MATCHED // GEE
################################################

cat("\014")
rm(list=ls())


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


# models
library(geepack) # install.packages("geepack")
gee.dich.m.1.t = extract.geepack(gee.dich.m.1 <- geeglm(clien1dummy ~ large + wealth + large:wealth,
                                              family = binomial(link = "logit"), 
                                              id = municipality, 
                                              std.err = "san.se",
                                              corstr = "independence",
                                              data = m.data))


gee.dich.m.2.t = extract.geepack(gee.dich.m.2 <- geeglm(clien1dummy ~ large + wealth + urban + munopp, 
                                                        family = binomial(link = "logit"), 
                                                        id = municipality, 
                                                        std.err = "san.se",
                                                        corstr = "independence",
                                                        data = m.data))

gee.dich.m.3.t = extract.geepack(gee.dich.m.3 <- geeglm(clien1dummy ~ large + wealth + polinv + munopp + ing4, 
                                                        family = binomial(link = "logit"), 
                                                        id = municipality, 
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


## Generating the Propensity Score 
library(CBPS, quietly = T) # install.packages("CBPS")
#fit <- CBPS(wagehalf ~ wealth + munopp + polinv,  # wealth + munopp + polinv
# fit <- CBPS(wagehalf ~ wealth + munopp + polinv + urban,  # wealth + munopp + polinv
fit <- CBPS(wagehalf ~ wealth + munopp + polinv + ing4,  # wealth + munopp + polinv
            data = dat, 
            iterations = 25000, 
            twostep = T, # F
            method = "exact", # EXACT
            ATT = 2, # 2
            standardize = F) # F

## transform the weight var. // Attaching weights to DF // sorting for GEE models
dat$weights = fit$weights




## Recode Before modeling
dat$clien1dummy <- as.numeric(dat$clien1dummy)
library(car)
dat$clien1dummy <- recode(dat$clien1dummy, "1 = 0 ; 2 = 1")
dat$logpop = log(dat$pop)


# models
set.seed(602); options(scipen=999)


gee.cont.rgps.1.t = extract.geepack(gee.cont.rgps.1 <- geeglm(clien1dummy ~ wagehalf + wealth + wagehalf:wealth + weights,
                                                        family = binomial(link = "probit"), 
                                                        id = municipality, 
                                                        #weights = weights,
                                                        std.err = "san.se",
                                                        corstr = "independence",
                                                        data = dat))


gee.cont.rgps.2.t = extract.geepack(gee.cont.rgps.2 <- geeglm(clien1dummy ~ wagehalf + wealth + urban + munopp + weights, 
                                                        family = binomial(link = "probit"), 
                                                        id = municipality, 
                                                        #weights = weights,
                                                        std.err = "san.se",
                                                        corstr = "independence",
                                                        data = dat))


gee.cont.rgps.3.t = extract.geepack(gee.cont.rgps.3 <- geeglm(clien1dummy ~ wagehalf + wealth + polinv + munopp + ing4 + weights, 
                                                        family = binomial(link = "probit"), 
                                                        id = municipality, 
                                                        #weights = weights,
                                                        std.err = "san.se",
                                                        corstr = "independence",
                                                        data = dat))




# back up
# gee.cont.m.3.t = extract.geepack(gee.cont.rgps.3 <- geeglm(clien1dummy ~ wagehalf + wealth + polinv + munopp + ing4, 
#                                                            family = binomial(link = "logit"), 
#                                                            id = municipality, 
#                                                            weights = weights,
#                                                            std.err = "san.se",
#                                                            corstr = "exchangeable",
#                                                            data = dat))


###### COEFFICIENT PLOT

cem.plot = data.frame(
  Coefficients = as.numeric(c(gee.dich.m.1$"coefficients", gee.dich.m.2$"coefficients", gee.dich.m.3$"coefficients")),
  Covariate = as.character(c(
    "Intercept", "Poverty Density", "Wealth Index", "Poverty Density*Wealth Index", 
    "Intercept", "Poverty Density", "Wealth Index", "Urban", "Municipal Opposition", 
    "Intercept", "Poverty Density", "Wealth Index", "Political Involvement Index", "Municipal Opposition", "Domocratic Support")),
  Model = as.character(c(rep("Economic", 4), rep("Contextual", 5), rep("Political", 6))),
  se = c(as.numeric(c(sqrt(diag(gee.dich.m.1$geese$vbeta)))), as.numeric(sqrt(diag(gee.dich.m.2$geese$vbeta))), as.numeric(sqrt(diag(gee.dich.m.3$geese$vbeta)))),
  upper = c(as.numeric(gee.dich.m.1$"coefficients")  + 1.28*sqrt(diag(gee.dich.m.1$geese$vbeta)),
            as.numeric(gee.dich.m.2$"coefficients") + 1.28*sqrt(diag(gee.dich.m.2$geese$vbeta)),
            as.numeric(gee.dich.m.3$"coefficients") + 1.28*sqrt(diag(gee.dich.m.3$geese$vbeta))),
  lower = c(as.numeric(gee.dich.m.1$"coefficients")  - 1.28*sqrt(diag(gee.dich.m.1$geese$vbeta)),
            as.numeric(gee.dich.m.2$"coefficients") - 1.28*sqrt(diag(gee.dich.m.2$geese$vbeta)),
            as.numeric(gee.dich.m.3$"coefficients") - 1.28*sqrt(diag(gee.dich.m.3$geese$vbeta))),
  Matching = rep(as.character("CEM"), 15)
)


gps.plot = data.frame(
  Coefficients = as.numeric(c(gee.cont.rgps.1$"coefficients", gee.cont.rgps.2$"coefficients", gee.cont.rgps.3$"coefficients")),
  Covariate = as.character(c(
    "Intercept", "Poverty Density", "Wealth Index", "Poverty Density*Wealth Index", "Weights", 
    "Intercept", "Poverty Density", "Wealth Index", "Urban", "Municipal Opposition", "Weights", 
    "Intercept", "Poverty Density", "Wealth Index", "Political Involvement Index", "Municipal Opposition", "Domocratic Support", "Weights")),
  Model = as.character(c(rep("Economic", 5), rep("Contextual", 6), rep("Political", 7))),
  se = c(as.numeric(c(sqrt(diag(gee.cont.rgps.1$geese$vbeta)))), as.numeric(sqrt(diag(gee.cont.rgps.2$geese$vbeta))), as.numeric(sqrt(diag(gee.cont.rgps.3$geese$vbeta)))),
  upper = c(as.numeric(gee.cont.rgps.1$"coefficients")  + 1.28*sqrt(diag(gee.cont.rgps.1$geese$vbeta)),
            as.numeric(gee.cont.rgps.2$"coefficients") + 1.28*sqrt(diag(gee.cont.rgps.2$geese$vbeta)),
            as.numeric(gee.cont.rgps.3$"coefficients") + 1.28*sqrt(diag(gee.cont.rgps.3$geese$vbeta))),
  lower = c(as.numeric(gee.cont.rgps.1$"coefficients")  - 1.28*sqrt(diag(gee.cont.rgps.1$geese$vbeta)),
            as.numeric(gee.cont.rgps.2$"coefficients") - 1.28*sqrt(diag(gee.cont.rgps.2$geese$vbeta)),
            as.numeric(gee.cont.rgps.3$"coefficients") - 1.28*sqrt(diag(gee.cont.rgps.3$geese$vbeta))),
  Matching = rep(as.character("GPS"), 18)
)

# cbind these two datasets
gee.plot = rbind(cem.plot, gps.plot)

# delete "non" important estimations
gee.plot<-gee.plot[!(gee.plot$Covariate=="Intercept" | gee.plot$Covariate=="Weights"),]



# plot

# Plot
library(ggplot2)
ggplot(gee.plot, aes(
  x = Covariate, 
  y = Coefficients, 
  ymin = upper, 
  ymax = lower,
  colour = Model,
  shape = Matching,
  position="dodge"
)) +
  geom_pointrange(position=position_dodge(width=0.65), fill = NA) + 
  geom_hline(yintercept = 0, alpha = 1/3, colour = gray(1/2), lty = 2) +
  coord_flip() + 
  xlab("") + 
  ylab("Coefficients (logit scale)") +
  ggtitle("Likelihood of Clientelism") +
  #guides(colour=FALSE) +
  #theme(legend.position="none") + 
  theme_bw() +
  #labs(colour = "Sample") +
  theme(legend.key = element_rect(colour = NA, fill = NA, size = 0.5)) 






# SIMULATIONS // gee.dich.m.X.s
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")

library(Zelig)
gee.dich.m.1.s = zelig(clien1dummy ~ large + wealth + large:wealth, 
              model = "logit.gee",
              id = "municipality", 
              std.err = "san.se",
              corstr = "independence",
              data = m.data)

gee.dich.m.2.s = zelig(clien1dummy ~ large + wealth + urban + munopp, 
              model = "logit.gee",
              id = "municipality", 
              std.err = "san.se",
              corstr = "independence",
              data = m.data)

gee.dich.m.3.s = zelig(clien1dummy ~ large + wealth + polinv + munopp + exc7 + ing4, 
              model = "logit.gee",
              id = "municipality", 
              std.err = "san.se",
              corstr = "independence",
              data = m.data)








### TEST AREA ### TEST AREA
### TEST AREA ### TEST AREA
### TEST AREA ### TEST AREA
### TEST AREA ### TEST AREA
### TEST AREA ### TEST AREA

cat("\014")
rm(list=ls())

load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")

# extract function for texreg
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




# DONT TOUCH BELOW 1
library(geepack) # install.packages("geepack")
library(texreg)
gee.1.m.t = extract.geepack(gee.1.m <- geeglm(clien1dummy ~ large,
                                              family = binomial(link = "logit"), 
                                              id = municipality, 
                                              std.err = "san.se",
                                              corstr = "independence",
                                              data = m.data))


# DONT TOUCH BELOW 2
library(geepack) # install.packages("geepack")
library(texreg)
gee.2.m.t = extract.geepack(gee.2.m <- geeglm(clien1dummy ~ large*polinv,
                                              family = binomial(link = "logit"), 
                                              id = municipality, 
                                              std.err = "san.se",
                                              corstr = "independence",
                                              data = m.data))



# DONT TOUCH BELOW 3
library(geepack) # install.packages("geepack")
library(texreg)
gee.3.m.t = extract.geepack(gee.3.m <- geeglm(clien1dummy ~ wealth,
                                              family = binomial(link = "logit"), 
                                              id = municipality, 
                                              std.err = "san.se",
                                              corstr = "independence",
                                              data = m.data))


# DONT TOUCH BELOW 4
library(geepack) # install.packages("geepack")
library(texreg)
gee.4.m.t = extract.geepack(gee.4.m <- geeglm(clien1dummy ~ wealth*large, 
                                              family = binomial(link = "logit"), 
                                              id = municipality, 
                                              std.err = "san.se",
                                              corstr = "independence",
                                              data = m.data))

# DONT TOUCH BELOW 5
library(geepack) # install.packages("geepack")
library(texreg)
gee.5.m.t = extract.geepack(gee.5.m <- geeglm(clien1dummy ~ polinv,
                                              family = binomial(link = "logit"), 
                                              id = municipality, 
                                              std.err = "san.se",
                                              corstr = "independence",
                                              data = m.data))


# DONT TOUCH BELOW 6
library(geepack) # install.packages("geepack")
library(texreg)
gee.6.m.t = extract.geepack(gee.6.m <- geeglm(clien1dummy ~ urban,
                                              family = binomial(link = "logit"), 
                                              id = municipality, 
                                              std.err = "san.se",
                                              corstr = "independence",
                                              data = m.data))


# DONT TOUCH BELOW 7
library(geepack) # install.packages("geepack")
library(texreg)
gee.7.m.t = extract.geepack(gee.7.m <- geeglm(clien1dummy ~ pop,
                                              family = binomial(link = "logit"), 
                                              id = municipality, 
                                              std.err = "san.se",
                                              corstr = "independence",
                                              data = m.data))





#####################################################################
### PARAMETRIC RAW S A M P L E // GPS // clustered std errors // GEE models
#####################################################################

load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/dat.RData")

## transform the wagehalf variable
dat$wagehalf = round(dat$wagehalf, digits=0)


# Generating the Propensity Score 
library(CBPS, quietly = T) # install.packages("CBPS")
#fit <- CBPS(wagehalf ~ wealth + munopp + polinv,  # wealth + munopp + polinv
fit <- CBPS(wagehalf ~ wealth + munopp + polinv + urban,  # wealth + munopp + polinv
            data = dat, 
            iterations = 25000, 
            twostep = F, # F
            method = "exact", # EXACT
            ATT = 2, # 2
            standardize = F) # F

# transform the weight var. // Attaching weights to DF // sorting for GEE models
dat$weights = fit$weights




# Recode Before modeling
dat$clien1dummy <- as.numeric(dat$clien1dummy)
library(car)
dat$clien1dummy <- recode(dat$clien1dummy, "1 = 0 ; 2 = 1")
dat$logpop = log(dat$pop)



# Tables
# ---- texreg-extrac.rtor-geeglm ----
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



# Models
# DONT TOUCH BELOW 1
library(geepack) # install.packages("geepack")
library(texreg)
gee.1.r.t = extract.geepack(gee.1.r <- geeglm(clien1dummy ~ large + weights,
                                               family = binomial(link = "logit"), 
                                               id = municipality, 
                                               std.err = "san.se",
                                               corstr = "independence",
                                               data = dat))


# DONT TOUCH BELOW 2
library(geepack) # install.packages("geepack")
library(texreg)
gee.2.r.t = extract.geepack(gee.2.r <- geeglm(clien1dummy ~ large*polinv+ weights,
                                               family = binomial(link = "logit"), 
                                               id = municipality, 
                                               std.err = "san.se",
                                               corstr = "independence",
                                               data = dat))




# DONT TOUCH BELOW 3
library(geepack) # install.packages("geepack")
library(texreg)
gee.3.r.t = extract.geepack(gee.3.r <- geeglm(clien1dummy ~ wealth+ weights,
                                               family = binomial(link = "logit"), 
                                               id = municipality, 
                                               std.err = "san.se",
                                               corstr = "independence",
                                               data = dat))


# DONT TOUCH BELOW 4
library(geepack) # install.packages("geepack")
library(texreg)
gee.4.r.t = extract.geepack(gee.4.r <- geeglm(clien1dummy ~ wealth*large+ weights, 
                                               family = binomial(link = "logit"), 
                                               id = municipality, 
                                               std.err = "san.se",
                                               corstr = "independence",
                                               data = dat))



# DONT TOUCH BELOW 5
library(geepack) # install.packages("geepack")
library(texreg)
gee.5.r.t = extract.geepack(gee.5.r <- geeglm(clien1dummy ~ polinv+ weights,
                                               family = binomial(link = "logit"), 
                                               id = municipality, 
                                               std.err = "san.se",
                                               corstr = "independence",
                                               data = dat))


# DONT TOUCH BELOW 6
library(geepack) # install.packages("geepack")
library(texreg)
gee.6.r.t = extract.geepack(gee.6.r <- geeglm(clien1dummy ~ urban+ weights,
                                               family = binomial(link = "logit"), 
                                               id = municipality, 
                                               std.err = "san.se",
                                               corstr = "independence",
                                               data = dat))


# DONT TOUCH BELOW 7
library(geepack) # install.packages("geepack")
library(texreg)
gee.7.r.t = extract.geepack(gee.7.r <- geeglm(clien1dummy ~ pop+ weights,
                                               family = binomial(link = "logit"), 
                                               id = municipality, 
                                               std.err = "san.se",
                                               corstr = "independence",
                                               data = dat))



########################################################################################
### PARAMETRIC RAW S A M P L E // GPS // clustered std errors // GEE models // CONTINUOUS TREAT
########################################################################################

load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/dat.RData")

## transform the wagehalf variable
dat$wagehalf = round(dat$wagehalf, digits=0)


# Generating the Propensity Score 
library(CBPS, quietly = T) # install.packages("CBPS")
fit <- CBPS(wagehalf ~ wealth + munopp + polinv,  # wealth + munopp + polinv
            data = dat, 
            iterations = 25000, 
            twostep = F, # F
            method = "exact", # EXACT
            ATT = 0, # 2
            standardize = F) # F

# transform the weight var. // Attaching weights to DF // sorting for GEE models
dat$weights = fit$weights




# Recode Before modeling
dat$clien1dummy <- as.numeric(dat$clien1dummy)
library(car)
dat$clien1dummy <- recode(dat$clien1dummy, "1 = 0 ; 2 = 1")
dat$logpop = log(dat$pop)



# Tables
# extract function for texreg
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




# Models
# DONT TOUCH BELOW 1
library(geepack) # install.packages("geepack")
library(texreg)
gee.1.c.r.t = extract.geepack(gee.1.c.r <- geeglm(clien1dummy ~ wagehalf + weights,
                                              family = binomial(link = "logit"), 
                                              id = municipality, 
                                              std.err = "san.se",
                                              corstr = "independence",
                                              data = dat))


# DONT TOUCH BELOW 2
library(geepack) # install.packages("geepack")
library(texreg)
gee.2.c.r.t = extract.geepack(gee.2.c.r <- geeglm(clien1dummy ~ wagehalf*polinv+ weights,
                                              family = binomial(link = "logit"), 
                                              id = municipality, 
                                              std.err = "san.se",
                                              corstr = "independence",
                                              data = dat))




# DONT TOUCH BELOW 3
library(geepack) # install.packages("geepack")
library(texreg)
gee.3.c.r.t = extract.geepack(gee.3.c.r <- geeglm(clien1dummy ~ wealth+ weights,
                                              family = binomial(link = "logit"), 
                                              id = municipality, 
                                              std.err = "san.se",
                                              corstr = "independence",
                                              data = dat))


# DONT TOUCH BELOW 4
library(geepack) # install.packages("geepack")
library(texreg)
gee.4.c.r.t = extract.geepack(gee.4.c.r <- geeglm(clien1dummy ~ wealth*wagehalf+ weights, 
                                              family = binomial(link = "logit"), 
                                              id = municipality, 
                                              std.err = "san.se",
                                              corstr = "independence",
                                              data = dat))



# DONT TOUCH BELOW 5
library(geepack) # install.packages("geepack")
library(texreg)
gee.5.c.r.t = extract.geepack(gee.5.c.r <- geeglm(clien1dummy ~ polinv+ weights,
                                              family = binomial(link = "logit"), 
                                              id = municipality, 
                                              std.err = "san.se",
                                              corstr = "independence",
                                              data = dat))


# DONT TOUCH BELOW 6
library(geepack) # install.packages("geepack")
library(texreg)
gee.6.c.r.t = extract.geepack(gee.6.c.r <- geeglm(clien1dummy ~ urban+ weights,
                                              family = binomial(link = "logit"), 
                                              id = municipality, 
                                              std.err = "san.se",
                                              corstr = "independence",
                                              data = dat))


# DONT TOUCH BELOW 7
library(geepack) # install.packages("geepack")
library(texreg)
gee.7.c.r.t = extract.geepack(gee.7.c.r <- geeglm(clien1dummy ~ pop+ weights,
                                              family = binomial(link = "logit"), 
                                              id = municipality, 
                                              std.err = "san.se",
                                              corstr = "independence",
                                              data = dat))



#####################################################################
### T A B L E S :   R A W   A N D   M A T C H E D   S A M P L E S 
#####################################################################


# TABLE
library(texreg)
screenreg(list(gee.1.r.t,gee.2.r.t,gee.2.r.t,gee.3.r.t,gee.4.r.t,gee.5.r.t,gee.6.r.t,gee.7.r.t), # screenreg / texreg
          #custom.coef.names = c(# this gotta be before OMIT.COEFF
          #                   "(Intercept)",
          #       "High Poor Density",
          #       "weights",
          #       "High Poor Density * Political Involvement",
          #       "Wealth Index",
          #       "High Poor Density * Wealth Index",
          #       "Political Involvement",
          #       "Urban",
          #       "Municipal Population"),
          caption = "Likelihood of Clientelism: Logit GEE Models using a Generalized Propensity Score",
          omit.coef = "weights",
         # ci.force = T,
          #override.pvalues = 0,
          #override.ci.low = as.list(gee.gps.dich.1$upper),
          #override.ci.up = as.list(gee.gps.dich.1$upper),
          label = "gee:gps:dich:1",
          stars = c(0.01, 0.05, 0.1),
          digits = 3,
          custom.note = "%stars. \n Logit GEE uses clustered std. errors at the municipality Level. \n Raw sample marched on the generalized propensity score. GPS vector omited. \n Binary treatment variable.\n 95% Standard errors in parentheses",
          fontsize = "scriptsize",
          float.pos = "h"
)





# TABLE
library(texreg)
screenreg(list(gee.1.m.t,gee.2.m.t,gee.2.m.t,gee.3.m.t,gee.4.m.t,gee.5.m.t,gee.6.m.t,gee.7.m.t), # screenreg / texreg
          #custom.coef.names = c(# this gotta be before OMIT.COEFF
          #         "(Intercept)",
          #        "Size of the Poor",
          #        "Wealth Index",
          #        "Size of the Poor TIMES Wealth Index",
          #        "Size of the Poor TIMES Political Involvement",
          #        "logpop",
          #        "weights"),
          caption = "Clientelism: Logit GEE Models",
          label = "gee:cem:dich:1",
          stars = c(0.01, 0.05, 0.1),
          digits = 3,
          #custom.model.names = c("Logit GEE", "Rare Event Logit"),
          custom.note = "%stars. \n Logit GEE uses clustered std. errors at the municipality Level. \n Matched sample. \n Binary treatment variable.\n 95% Standard errors in parentheses",
          fontsize = "scriptsize",
          float.pos = "h"
)

# TABLE
library(texreg)
screenreg(list(gee.1.c.r.t,gee.2.c.r.t,gee.3.c.r.t,gee.4.c.r.t,gee.5.c.r.t,gee.6.c.r.t,gee.7.c.r.t), # screenreg / texreg
          #custom.coef.names = c(# this gotta be before OMIT.COEFF
          #         "(Intercept)",
          #        "Size of the Poor",
          #        "Wealth Index",
          #        "Size of the Poor TIMES Wealth Index",
          #        "Size of the Poor TIMES Political Involvement",
          #        "logpop",
          #        "weights"),
          caption = "Clientelism: Logit GEE Models",
          label = "gee:gps:cont:1",
          stars = c(0.01, 0.05, 0.1),
          digits = 3,
          #custom.model.names = c("Logit GEE", "Rare Event Logit"),
          custom.note = "%stars. \n Logit GEE uses clustered std. errors at the municipality Level. \n Raw sample marched on the generalized propensity score. \n No cutoff was used. \n 95% Standard errors in parentheses",
          fontsize = "scriptsize",
          float.pos = "h"
)



######################################################
#         C O E F F I C I E N T   P L O T S 
#              N  O T   S I M U L A T E D
######################################################


# construct the dataset for raw sample
gee.r.plot <- data.frame(
  Sample = as.character("GPS"),
  Treatment = as.character("Binary"),
  variable = seq(1:7),
  coefficients = as.numeric(c(
    as.numeric(gee.1.r$"coefficients"["large"]),
    as.numeric(gee.2.r$"coefficients"["large:polinv"]),
    as.numeric(gee.3.r$"coefficients"["wealth"]),
    as.numeric(gee.4.r$"coefficients"["wealth:large"]),
    as.numeric(gee.5.r$"coefficients"["polinv"]),
    as.numeric(gee.6.r$"coefficients"["urbanUr"]),
    as.numeric(gee.7.r$"coefficients"["pop"]))),
  se = as.numeric(c(
    as.numeric(sqrt(diag(gee.1.r$geese$vbeta))[2]),
    as.numeric(sqrt(diag(gee.2.r$geese$vbeta))[2]),
    as.numeric(sqrt(diag(gee.3.r$geese$vbeta))[2]),
    as.numeric(sqrt(diag(gee.4.r$geese$vbeta))[2]),
    as.numeric(sqrt(diag(gee.5.r$geese$vbeta))[2]),
    as.numeric(sqrt(diag(gee.6.r$geese$vbeta))[2]),
    as.numeric(sqrt(diag(gee.7.r$geese$vbeta))[2]))
    )
  )

gee.r.plot$upper <- gee.r.plot$coefficients + 1.96*gee.r.plot$se
gee.r.plot$lower <- gee.r.plot$coefficients - 1.96*gee.r.plot$se


gee.r.plot$variable <- factor(gee.r.plot$variable,
                              levels = c(1,2,3,4,5,6,7), ordered=TRUE,
                              labels =   c("High Poor Density", 
                                           "High Poor Density * Political Involvement Index", 
                                           "Wealth Index",
                                           "High Poor Density * Wealth Index", 
                                           "Political Involvement Index", 
                                           "Urban",
                                           "Municipal Population")
                              )


# construct the dataset for matched sample
gee.m.plot <- data.frame(
  Sample = as.character("CEM"),
  Treatment = as.character("Binary"),
  variable = seq(1:7),
  coefficients = as.numeric(c(
    as.numeric(gee.1.m$"coefficients"["large"]),
    as.numeric(gee.2.m$"coefficients"["large:polinv"]),
    as.numeric(gee.3.m$"coefficients"["wealth"]),
    as.numeric(gee.4.m$"coefficients"["wealth:large"]),
    as.numeric(gee.5.m$"coefficients"["polinv"]),
    as.numeric(gee.6.m$"coefficients"["urbanUr"]),
    as.numeric(gee.7.m$"coefficients"["pop"]))),
  se = as.numeric(c(
    as.numeric(sqrt(diag(gee.1.m$geese$vbeta))[2]),
    as.numeric(sqrt(diag(gee.2.m$geese$vbeta))[2]),
    as.numeric(sqrt(diag(gee.3.m$geese$vbeta))[2]),
    as.numeric(sqrt(diag(gee.4.m$geese$vbeta))[2]),
    as.numeric(sqrt(diag(gee.5.m$geese$vbeta))[2]),
    as.numeric(sqrt(diag(gee.6.m$geese$vbeta))[2]),
    as.numeric(sqrt(diag(gee.7.m$geese$vbeta))[2])
    )
  )
)

gee.m.plot$upper <- gee.m.plot$coefficients + 1.96*gee.m.plot$se
gee.m.plot$lower <- gee.m.plot$coefficients - 1.96*gee.m.plot$se


gee.m.plot$variable <- factor(gee.m.plot$variable,
                              levels = c(1,2,3,4,5,6,7), ordered=TRUE,
                              labels =   c("High Poor Density", 
                                           "High Poor Density * Political Involvement Index", 
                                           "Wealth Index",
                                           "High Poor Density * Wealth Index", 
                                           "Political Involvement Index", 
                                           "Urban",
                                           "Municipal Population")
                              )



# Raw sample matched on the generalized propensity score // continuous treatment
gee.c.r.plot <- data.frame(
  Sample = as.character("GPS"),
  Treatment = as.character("Continuous"),
  variable = seq(1:7),
  coefficients = as.numeric(c(
    as.numeric(gee.1.c.r$"coefficients"["wagehalf"]),
    as.numeric(gee.2.c.r$"coefficients"["wagehalf:polinv"]),
    as.numeric(gee.3.c.r$"coefficients"["wealth"]),
    as.numeric(gee.4.c.r$"coefficients"["wealth:wagehalf"]),
    as.numeric(gee.5.c.r$"coefficients"["polinv"]),
    as.numeric(gee.6.c.r$"coefficients"["urbanUr"]),
    as.numeric(gee.7.c.r$"coefficients"["pop"]))),
  se = as.numeric(c(
    as.numeric(sqrt(diag(gee.1.c.r$geese$vbeta))[2]),
    as.numeric(sqrt(diag(gee.2.c.r$geese$vbeta))[2]),
    as.numeric(sqrt(diag(gee.3.c.r$geese$vbeta))[2]),
    as.numeric(sqrt(diag(gee.4.c.r$geese$vbeta))[2]),
    as.numeric(sqrt(diag(gee.5.c.r$geese$vbeta))[2]),
    as.numeric(sqrt(diag(gee.6.c.r$geese$vbeta))[2]),
    as.numeric(sqrt(diag(gee.7.c.r$geese$vbeta))[2])
  )
  )
)

gee.c.r.plot$upper <- gee.c.r.plot$coefficients + 1.96*gee.c.r.plot$se
gee.c.r.plot$lower <- gee.c.r.plot$coefficients - 1.96*gee.c.r.plot$se


gee.c.r.plot$variable <- factor(gee.c.r.plot$variable,
                                levels = c(1,2,3,4,5,6,7), ordered=TRUE,
                                labels =   c("Size of the Poor", 
                                             "Size of the Poor * Political Involvement Index", 
                                             "Wealth Index",
                                             "Size of the Poor * Wealth Index", 
                                             "Political Involvement Index", 
                                             "Urban",
                                             "Municipal Population")
                                )




# cbind these two datasets
gee.plot = rbind(gee.m.plot, gee.r.plot,gee.c.r.plot)

# plot

# Plot
library(ggplot2)
ggplot(gee.plot, aes(
  x = variable, 
  y = coefficients, 
  ymin = upper, 
  ymax = lower,
  colour = Sample,
  shape = Treatment,
  position="dodge"
  )) +
  geom_pointrange(position=position_dodge(width=0.45)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
  coord_flip() + 
  xlab("") + 
  ylab("Coefficients (logit scale)") +
  ggtitle("Likelihood of Clientelism") +
  #guides(colour=FALSE) +
  #theme(legend.position="none") + 
  theme_bw() +
  #labs(colour = "Sample") +
  theme(legend.key = element_rect(colour = NA, fill = NA, size = 0.5)) 



##########################
#   MATCHED SAMPLE // GEE LOGIT // [gps:plot:0] // LARGE [distribution plots]
##########################

cat("\014")
rm(list=ls())

load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")



# METHOD 2
# library(devtools) # install.packages("devtools")
# install_github('IQSS/Zelig')
#library(Zelig) # install.packages("Zelig", dependencies=TRUE) # Models


# model
set.seed(602); options(scipen=999)
library(Zelig) # this is for simulation only, not for the table
#gee.1.m.zelig = zelig(clien1dummy ~ large,
gee.1.m.zelig = zelig(clien1dummy ~ large + wealth + polinv + large*wealth + large*polinv,
                      model = "logit.gee",
                      family = binomial(link = "logit"), 
                      id = "municipality", 
                      std.err = "san.se",
                      corstr = "independence",
                      data = m.data)


# simulation
## low 
gee.1.m.zelig.low = data.frame(
  sim(x = setx(gee.1.m.zelig, 
               large = min(m.data$large)), num=700)$getqi(qi="ev"))
colnames(gee.1.m.zelig.low) <- c("Low")  # low

## high
gee.1.m.zelig.high = data.frame(
  sim(x = setx(gee.1.m.zelig, 
               large = max(m.data$large)), num=700)$getqi(qi="ev"))
colnames(gee.1.m.zelig.high) <- c("High")  # low



# plot
library(ggplot2) ; library(gridExtra)

grid.arrange(ggplot() + 
               geom_density(aes(x=Low, fill="Low"), data= gee.1.m.zelig.low, alpha = .2) + 
               geom_density(aes(x=High, fill="High"), data= gee.1.m.zelig.high, alpha = .2) + 
               xlab("Expected Value") + ylab("Density") + 
               theme_bw() + 
               theme(legend.key = element_rect(colour = NA, fill = NA, size = 0.5)) + 
               scale_fill_discrete(guide = guide_legend(title = "Density of the Poor")),
             bottom = "Note: Logit GEE model. Std. clustered errors at the municipal level. Matched Dataset.")


##########################
#   MATCHED SAMPLE // GEE LOGIT // [plot:1] // LARGE * POLINV
##########################

cat("\014")
rm(list=ls())

load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")



# METHOD 2
# library(devtools) # install.packages("devtools")
# install_github('IQSS/Zelig')
#library(Zelig) # install.packages("Zelig", dependencies=TRUE) # Models


# model
library(Zelig) # this is for simulation only, not for the table
#gee.2.m.zelig = zelig(clien1dummy ~ large*polinv,
gee.2.m.zelig = zelig(clien1dummy ~ large + wealth + polinv + large*wealth + large*polinv,
                      model = "logit.gee",
                      family = binomial(link = "logit"), 
                      id = "municipality", 
                      std.err = "san.se",
                      corstr = "independence",
                      data = m.data)

# simulation
library(Zelig)
set.seed(602); options(scipen=999)



# low 
gee.2.m.zelig.low = data.frame(
  sim(
    x = setx(gee.2.m.zelig, 
             large = min(m.data$large), 
             polinv = min(m.data$polinv):max(m.data$polinv)), 
    num=700)$getqi(qi="ev", xvalue="range"))

colnames(gee.2.m.zelig.low) <- seq(1:ncol(as.data.frame(t(min(m.data$polinv):max(m.data$polinv)))))  # low

# high
gee.2.m.zelig.high = data.frame(
  sim(
    x = setx(gee.2.m.zelig, 
               large = max(m.data$large), 
               polinv = min(m.data$polinv):max(m.data$polinv)), 
      num=700)$getqi(qi="ev", xvalue="range")) ; 
colnames(gee.2.m.zelig.high) <- seq(1:ncol(as.data.frame(t(min(m.data$polinv):max(m.data$polinv)))))  # high



# plot ## geom_point also works
library(ggplot2) ; library(gridExtra)
polinv.range = as.numeric(min(m.data$polinv):max(m.data$polinv))
set.seed(602)
grid.arrange(ggplot() + 
               geom_line(aes(x=polinv.range[1], y=gee.2.m.zelig.low[1], colour = "Low"), position = position_jitter(width = 3), alpha = 1/20) +
               geom_line(aes(x=polinv.range[2], y=gee.2.m.zelig.low[2], colour = "Low"), position = position_jitter(width = 3), alpha = 1/20) +
               geom_line(aes(x=polinv.range[3], y=gee.2.m.zelig.low[3], colour = "Low"), position = position_jitter(width = 3), alpha = 1/20) + 
               geom_line(aes(x=polinv.range[4], y=gee.2.m.zelig.low[4], colour = "Low"), position = position_jitter(width = 3), alpha = 1/20) +
               geom_line(aes(x=polinv.range[5], y=gee.2.m.zelig.low[5], colour = "Low"), position = position_jitter(width = 3), alpha = 1/20) +
               geom_line(aes(x=polinv.range[6], y=gee.2.m.zelig.low[6], colour = "Low"), position = position_jitter(width = 3), alpha = 1/20) +
               geom_line(aes(x=polinv.range[7], y=gee.2.m.zelig.low[7], colour = "Low"), position = position_jitter(width = 3), alpha = 1/20) +
               geom_line(aes(x=polinv.range[1], y=gee.2.m.zelig.high[1], colour = "High"), position = position_jitter(width = 3), alpha = 1/20) +
               geom_line(aes(x=polinv.range[2], y=gee.2.m.zelig.high[2], colour = "High"), position = position_jitter(width = 3), alpha = 1/20) +
               geom_line(aes(x=polinv.range[3], y=gee.2.m.zelig.high[3], colour = "High"), position = position_jitter(width = 3), alpha = 1/20) + 
               geom_line(aes(x=polinv.range[4], y=gee.2.m.zelig.high[4], colour = "High"), position = position_jitter(width = 3), alpha = 1/20) +
               geom_line(aes(x=polinv.range[5], y=gee.2.m.zelig.high[5], colour = "High"), position = position_jitter(width = 3), alpha = 1/20) +
               geom_line(aes(x=polinv.range[6], y=gee.2.m.zelig.high[6], colour = "High"), position = position_jitter(width = 3), alpha = 1/20) +
               geom_line(aes(x=polinv.range[7], y=gee.2.m.zelig.high[7], colour = "High"), position = position_jitter(width = 3), alpha = 1/20) +
               xlab("Political Involvement Index") + ylab("Expected Value of Clientelism") + 
               theme_bw() + 
               labs(colour = "Density of the Poor") +
               theme(legend.key = element_rect(colour = NA, fill = NA, size = 0.5)), 
             bottom = "Note: Logit GEE model. Std. clustered errors at the municipal level. Matched Dataset.")


##########################
#   MATCHED SAMPLE // GEE LOGIT // [plot:2] // LARGE * WEALTH
##########################

cat("\014")
rm(list=ls())

load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")



# METHOD 2
# library(devtools) # install.packages("devtools")
# install_github('IQSS/Zelig')
#library(Zelig) # install.packages("Zelig", dependencies=TRUE) # Models


# model
library(Zelig) # this is for simulation only, not for the table
gee.4.m.zelig = zelig(clien1dummy ~ wealth*large,
#gee.4.m.zelig = zelig(clien1dummy ~ large + wealth + polinv + large*wealth + large*polinv,
                      model = "logit.gee",
                      family = binomial(link = "logit"), 
                      id = "municipality", 
                      std.err = "san.se",
                      corstr = "independence",
                      data = m.data)

# simulation
library(Zelig)
set.seed(602); options(scipen=999)


# low 
gee.4.m.zelig.low = data.frame(
  sim(
    x = setx(gee.4.m.zelig, 
             large = min(m.data$large), 
             wealth = min(m.data$wealth):max(m.data$wealth)), 
    num=300)$getqi(qi="ev", xvalue="range"))

colnames(gee.4.m.zelig.low) <- seq(1:ncol(as.data.frame(t(min(m.data$wealth):max(m.data$wealth)))))  # low

# high
gee.4.m.zelig.high = data.frame(
  sim(x = setx(gee.4.m.zelig, 
               large = max(m.data$large), 
               wealth = min(m.data$wealth):max(m.data$wealth)), 
      num=300)$getqi(qi="ev", xvalue="range")) ; 
colnames(gee.4.m.zelig.high) <- seq(1:ncol(as.data.frame(t(min(m.data$wealth):max(m.data$wealth)))))  # high





# plot ##  geom_point also works
# plot ##  geom_line also works
library(ggplot2) ; library(gridExtra)
wealth.range = as.numeric(min(m.data$wealth):max(m.data$wealth))
set.seed(602)
grid.arrange(ggplot() + 
               geom_line(aes(x=wealth.range[1], y=gee.4.m.zelig.low[1], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) + 
               geom_line(aes(x=wealth.range[2], y=gee.4.m.zelig.low[2], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[3], y=gee.4.m.zelig.low[3], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[4], y=gee.4.m.zelig.low[4], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[5], y=gee.4.m.zelig.low[5], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[6], y=gee.4.m.zelig.low[6], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[7], y=gee.4.m.zelig.low[7], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[8], y=gee.4.m.zelig.low[8], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[9], y=gee.4.m.zelig.low[9], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[10], y=gee.4.m.zelig.low[10], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[11], y=gee.4.m.zelig.low[11], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[12], y=gee.4.m.zelig.low[12], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[13], y=gee.4.m.zelig.low[13], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[14], y=gee.4.m.zelig.low[14], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[15], y=gee.4.m.zelig.low[15], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[16], y=gee.4.m.zelig.low[16], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[17], y=gee.4.m.zelig.low[17], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[18], y=gee.4.m.zelig.low[18], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[19], y=gee.4.m.zelig.low[19], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[20], y=gee.4.m.zelig.low[20], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[21], y=gee.4.m.zelig.low[21], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[22], y=gee.4.m.zelig.low[22], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[23], y=gee.4.m.zelig.low[23], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[24], y=gee.4.m.zelig.low[24], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[25], y=gee.4.m.zelig.low[25], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[26], y=gee.4.m.zelig.low[26], colour = "Low"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[1], y=gee.4.m.zelig.high[1], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) + 
               geom_line(aes(x=wealth.range[2], y=gee.4.m.zelig.high[2], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[3], y=gee.4.m.zelig.high[3], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[4], y=gee.4.m.zelig.high[4], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[5], y=gee.4.m.zelig.high[5], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[6], y=gee.4.m.zelig.high[6], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[7], y=gee.4.m.zelig.high[7], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[8], y=gee.4.m.zelig.high[8], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[9], y=gee.4.m.zelig.high[9], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[10], y=gee.4.m.zelig.high[10], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[11], y=gee.4.m.zelig.high[11], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[12], y=gee.4.m.zelig.high[12], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[13], y=gee.4.m.zelig.high[13], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[14], y=gee.4.m.zelig.high[14], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[15], y=gee.4.m.zelig.high[15], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[16], y=gee.4.m.zelig.high[16], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[17], y=gee.4.m.zelig.high[17], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[18], y=gee.4.m.zelig.high[18], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[19], y=gee.4.m.zelig.high[19], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[20], y=gee.4.m.zelig.high[20], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[21], y=gee.4.m.zelig.high[21], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[22], y=gee.4.m.zelig.high[22], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[23], y=gee.4.m.zelig.high[23], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[24], y=gee.4.m.zelig.high[24], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[25], y=gee.4.m.zelig.high[25], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               geom_line(aes(x=wealth.range[26], y=gee.4.m.zelig.high[26], colour = "High"), position = position_jitter(width = 5), alpha = 1/20) +
               xlab("Wealth Index") + ylab("Expected Value of Clientelism") + 
               theme_bw() + 
               labs(colour = "Density of the Poor") +
               theme(legend.key = element_rect(colour = NA, fill = NA, size = 0.5)), 
             bottom = "Note: Logit GEE model. Std. clustered errors at the municipal level. \n Matched Dataset.")


######################################################
#  D  E S C R I P T I V E          P   L   O   T   S #
######################################################

cat("\014")
rm(list=ls())

load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")



# Subset Data for Descriptive Stats Table

# matched sample
m.dat.clien1dummy <- m.data$clien1dummy 
m.dat.clien1dummy <- m.data$clien1dummy 
m.dat.large <- m.data$large 
m.dat.polinv <- m.data$polinv 
m.dat.exc7 <- m.data$exc7 
m.dat.vb3 <- m.data$vb3 
m.dat.ed <- m.data$ed 
m.dat.income <- m.data$income 
m.dat.munopp <- m.data$munopp
m.dat.pop = m.data$pop
m.dat.logpop = log(m.dat.pop)
m.dat.urban <- m.data$urban
m.dat.urban <- as.numeric(m.dat.urban)
m.dat.urban <- recode(m.dat.urban, "1 = 0 ; 2 = 1")
m.dat.polinv <-  m.data$polinv
m.dat.ed <- m.data$ed
m.dat.ed <- as.numeric(m.dat.ed)
m.dat.ed <- recode(m.dat.ed, "
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

m.data.s <- data.frame(m.dat.clien1dummy, m.dat.large, m.dat.income, m.dat.exc7, m.dat.polinv, m.dat.urban, m.dat.logpop, m.dat.ed, m.dat.munopp, m.dat.vb3)

# Descriptive Stats Matched Set
library(stargazer, quietly = T)
stargazer(m.data.s, 
          summary=T, 
          title = "Summary Statistics: Matched Sample",
          label = "sumtab:1",
          type = "text",
          font.size = "scriptsize",
          style= "apsr",
          covariate.labels=c(
            "Clientelism",
            "High Density",
            "Income",
            "Perception of Corruption",
            "Political Involvement Index",
            "Urban",
            "Population (ln)",
            "Years of Schooling",
            "Municipal Opposition",
            "Political Id"),
          table.placement = "h",
          notes.align = "c"
)

# Descriptive Stats Raw Set



# Subset Data for Descriptive Stats Table
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/dat.RData")

# whole sample
dat.clien1dummy <- dat$clien1dummy 
dat.large <- dat$large 
dat.polinv <- dat$polinv 
dat.exc7 <- dat$exc7 
dat.vb3 <- dat$vb3 
dat.ed <- dat$ed 
dat.income <- dat$income 
dat.munopp <- dat$munopp
dat.pop = dat$pop
dat.logpop = log(dat.pop)
dat.urban <- dat$urban
dat.urban <- as.numeric(dat.urban)
dat.urban <- recode(dat.urban, "1 = 0 ; 2 = 1")
dat.polinv <-  dat$polinv
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

dat.s <- data.frame(dat.clien1dummy, dat.large, dat.income, dat.exc7, dat.polinv, dat.urban, dat.logpop, dat.ed, dat.munopp, dat.vb3)


library(stargazer, quietly = T)
stargazer(dat.s, 
          summary=T, 
          title = "Summary Statistics: Raw Sample",
          label = "sumtab:2",
          type = "text",
          font.size = "scriptsize",
          style= "apsr",
          covariate.labels=c(
            "Clientelism",
            "High Density",
            "Income",
            "Perception of Corruption",
            "Political Involvement Index",
            "Urban",
            "Population (ln)",
            "Years of Schooling",
            "Municipal Opposition",
            "Political Id"),
          table.placement = "h",
          notes.align = "c"
)




## Distribution Outcome Variable Binary Outcome
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

library(ggplot2) ; library(gridExtra)
grid.arrange(ggplot() + 
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
               xlab("Percentage of People Living with Less than Half of the Minimum Wage") + 
               ylab("Density Estimate") + 
               theme_bw(),
             bottom = "Note: Matched Sample.")


######################################################
#  B   A   L   A   N   C   E       P   L   O   T   S #
######################################################

# 5 x 5 size.

# Jitter
plot(m.out, type = "jitter")
dev.off()

# Histogram de PSs
plot(m.out, type = "hist")
dev.off()


boxplot(distance~clien1dummy,data=m.data, 
        xlab="Clientelism", 
        ylab="Propensity Score")


# QQ of de PSs by covariate
set.seed(602)
plot(m.out, col = "forestgreen")
dev.off()


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
#   Sensivity Analysis
##########################

large <- as.numeric(m.data$large)
income <- as.numeric(m.data$income)
exc7 <- as.numeric(m.data$exc7)
polinv <- as.numeric(m.data$polinv)
urban <- as.numeric(m.data$urban)
logpop = log(m.data$pop)
logpop <- as.numeric(logpop)
ed <- as.numeric(m.data$ed)
munopp <- as.numeric(m.data$munopp)
vb3 <- as.numeric(m.data$vb3)


m.data.imb <- data.frame(large, income, exc7, polinv, urban, logpop, ed, munopp, vb3)
vars <- c("income", "exc7", "polinv", "urban", "logpop", "ed", "munopp", "vb3")
library(cem)
imb <- imbalance(group = m.data$large, data = m.data.imb[vars])
imb

# l
l = imb$tab[3] # Extract L Statistic
st = imb$tab[1] # Extract Diff in Means

# Calling values
l.income = round(l$L1[1], 3)
l.exc7 = round(l$L1[2], 3)
l.polinv = round(l$L1[3], 3)
l.urban = round(l$L1[4], 3)
l.logpop = round(l$L1[5], 3)
l.ed = round(l$L1[6], 3)
l.munopp = round(l$L1[7], 3)
l.vb3 = round(l$L1[8], 3)

# Calling values
st.income = round(st$statistic[1], 3)
st.exc7 = round(st$statistic[2], 3)
st.polinv = round(st$statistic[3], 3)
st.urban = round(st$statistic[4], 3)
st.logpop = round(st$statistic[5], 3)
st.ed = round(st$statistic[6], 3)
st.munopp = round(st$statistic[7], 3)
st.vb3 = round(st$statistic[8], 3)










#######################################
# O L D   D O N T   U S E   B E L O W 
#######################################





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

