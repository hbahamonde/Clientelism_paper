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


## transform the wagehalf variable
dat$wagehalf = round(dat$wagehalf, digits=0)


# Transform the continuous "treatment" variable in four segments
wagehalf.4 = cut(dat$wagehalf, breaks = c(0,
                                          quantile(dat$wagehalf, .25), 
                                          quantile(dat$wagehalf, .50), 
                                          quantile(dat$wagehalf, .75), 
                                          quantile(dat$wagehalf, 1)), include.lowest=T, labels=c("0-25","25-50","50-75","75-100")
)

dat$wagehalf.4 = as.numeric(wagehalf.4)

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
dat$pop.10 = as.numeric(pop.10.r)


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

cat("\014")
rm(list=ls())

############ formulas
model.m = formula(clien1dummy ~ wealth*munopp*large + pop.10 + polinv*wealth + ing4 + vb3 + exc7 + ed)
model.gps = formula(clien1dummy ~ wealth*munopp*wagehalf.4 + pop.10 + polinv*wealth + ing4 + vb3 + exc7 + ed + weights)



############ GENERATE THE GPS vector

# load data
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/dat.RData")


## Generating the Propensity Score 
library(CBPS, quietly = T) # install.packages("CBPS")
fit <- CBPS(as.factor(wagehalf.4) ~  wealth + urban + munopp + polinv,
              #wealth,# + polinv,# + munopp + polinv + ing4,  # wealth + munopp + polinv
            data = dat, 
            iterations = 25000, 
            twostep = TRUE, # F
            method = "over", # EXACT
            ATT = 0, # 2
            standardize = F) # F


## transform the weight var. // Attaching weights to DF // sorting for GEE models
dat$weights = fit$weights

################################################
options(scipen=999)

## Recode Before modeling
dat$clien1dummy <- as.numeric(dat$clien1dummy)
library(car)
dat$clien1dummy <- recode(dat$clien1dummy, "1 = 0 ; 2 = 1")


### GEE: In gee there is no quasipossion, because gee is in a way already quasi.
### With GEE we do not fit a poisson glm, but use in the construction of the sandwich covariance 
### matrix the variance function of the poisson family. In Gee always an 'overdispersion' is estimated.



# models
library(geepack) # install.packages("geepack")

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


model.m.t = extract.geepack(model.m.model <- geeglm(model.m,
                               family = binomial(link = "logit"), 
                               id = municipality, 
                               weights = wt,
                               std.err = "san.se",
                               corstr = "exchangeable",
                               data = m.data))

library(geepack) # install.packages("geepack")
model.gps.t = extract.geepack(model.gps.model <- geeglm(model.gps,
                                 family = binomial(link = "logit"), 
                                 id = municipality, 
                                 #weights = wt,
                                 std.err = "san.se",
                                 corstr = "exchangeable",
                                 data = dat))

#####################################################################
### T A B L E S :   G P S   A N D   M A T C H E D   S A M P L E S 
#####################################################################


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


screenreg(
  c(model.m.t,model.gps.t), 
  caption = "Generalized Estimating Logistic Equations: Clientelism",
custom.coef.names = c(
  "(intercept)",
  "Wealth Index", 
  "Municipal Opposition", 
  "Density of the Poor", 
  "Municipal Population", 
  "Political Involvement", 
  "Democratic Support", 
  "Political Id.", 
  "Perception of Corruption", 
  "Schooling", 
  "Wealth Index * Municipal Opposition",
  "Wealth Index * Density of the Poor", 
  "Municipal Opposition * Density of the Poor", 
  "Wealth Index * Political Involvement", 
  "Wealth Index * Municipal Opposition * Density of the Poor", 
  "Density of the Poor",
  "Weights", 
  "Wealth Index * Density of the Poor", 
  "Municipal Opposition * Density of the Poor", 
  "Wealth Index * Municipal Opposition * Density of the Poor"),
custom.model.names = c(
  "Matched Dataset",
  "GPS Dataset"),
label = "tab:1",
custom.note = "%stars. Clustered Standard Errors in parentheses.",
fontsize = "scriptsize",
center = TRUE,
no.margin = TRUE, 
float.pos = "h"
)



#####################################################################
### S I M U L A T I O N S:                            M  O D E L S
#####################################################################


library(Zelig)
model.m.s = zelig(model.m, 
                       model = "logit.gee",
                       id = "municipality", 
                       weights = "wt",
                       std.err = "san.se",
                       corstr = "exchangeable",
                       data = m.data,
                       cite = F)


model.gps.s = zelig(model.gps, 
                       model = "logit.gee",
                       id = "municipality", 
                       weights = "wt",
                       std.err = "san.se",
                       corstr = "exchangeable",
                       data = dat,
                       cite = F)



#####################################################################
### S I M U L A T I O N S:      D  I S T R I B U T I O N   P L O T S 
#####################################################################
set.seed(602); options(scipen=999)


## matched
gee.sim.low.matched = data.frame(Low = sim(x = setx(model.m.s, cond = T, large = min(m.data$large)), num=10000)$getqi(qi="ev"))
gee.sim.high.matched = data.frame(High = sim(x = setx(model.m.s, cond = T, large = max(m.data$large)), num=10000)$getqi(qi="ev"))
## gps
gee.sim.low.gps = data.frame(Low = sim(x = setx(model.gps.s, cond = T, wagehalf.4 = min(dat$wagehalf.4)), num=10000)$getqi(qi="ev"))
gee.sim.high.gps = data.frame(High = sim(x = setx(model.gps.s, cond = T, wagehalf.4 = max(dat$wagehalf.4)), num=10000)$getqi(qi="ev"))



# plot
library(ggplot2);library(grid)

large.m1 = 
  ggplot() + 
  geom_density(aes(x=Low, fill="Low"),  data= gee.sim.low.matched, alpha = .2) + 
  geom_density(aes(x=High, fill="High"), data= gee.sim.high.matched, alpha = .2) + 
  xlab("Expected Value \n of Clientelism") + ylab("Estimated Density") + 
  theme_bw() + xlab("Expected Value \n of Clientelism") +
  ggtitle("Matched Dataset") +
  theme(
    legend.key = 
      element_rect(colour = NA, fill = NA, size = 0.5), 
    panel.margin = unit(0, "lines")) + 
  scale_fill_discrete(guide = guide_legend(title = "Density of the Poor")) +
  guides(fill=FALSE)

large.m2 = 
  ggplot() + 
  geom_density(aes(x=Low, fill="Low"), data= gee.sim.low.gps, alpha = .2) + 
  geom_density(aes(x=High, fill="High"), data= gee.sim.high.gps, alpha = .2) + 
  ylab("") + xlab("Expected Value \n of Clientelism") +
  theme_bw() + 
  ggtitle("Complete Dataset (GPS)") +
  theme(
    legend.key = element_rect(colour = NA, fill = NA, size = 0.5),
    panel.margin = unit(0, "lines"),
    axis.title.x = element_text(colour = "black")) + 
  scale_fill_discrete(guide = guide_legend(title = "Density of the Poor")) + 
  guides(fill=FALSE)



library(cowplot) # install.packages("cowplot")
plot_grid(large.m1,large.m2, nrow = 1, align = "v", scale = 1)


##########################################################################
### S I M U L A T I O N S:      D  I S T R I B U T I O N   P L O T S  I I 
##########################################################################




##########################
##### BY Competition and Income
##########################

set.seed(602); options(scipen=999)

# simulation DISTRIBUTION PLOTS
library(Zelig)
high.poor.lowcomp = data.frame(competition = rep("Low Competition", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE,large = max(m.data$large), wealth= quantile(m.data$wealth, .25), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.poor.highcomp = data.frame(competition = rep("High Competition", 1000000),income = rep("Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE,large = max(m.data$large), wealth= quantile(m.data$wealth, .25), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.lowcomp = data.frame(competition = rep("Low Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = max(m.data$large), wealth= quantile(m.data$wealth, .75), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.highcomp = data.frame(competition = rep("High Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = max(m.data$large), wealth= quantile(m.data$wealth, .75), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.lowcomp = data.frame(competition = rep("Low Competition", 1000000),income = rep("Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = min(m.data$large), wealth= quantile(m.data$wealth, .25), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.highcomp = data.frame(competition = rep("High Competition", 1000000),income = rep("Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = min(m.data$large), wealth= quantile(m.data$wealth, .25), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.lowcomp = data.frame(competition = rep("Low Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = min(m.data$large), wealth= quantile(m.data$wealth, .75), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.highcomp = data.frame(competition = rep("High Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = min(m.data$large), wealth= quantile(m.data$wealth, .75), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))


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


### quadrants 1 and 4
t.test(low.poor.lowcomp$x, high.rich.highcomp$x,conf.level = 0.99) # significative pvalue = significantly different

### quadrants 3-4
t.test(high.poor.highcomp$x, high.rich.highcomp$x,conf.level = 0.99, paired = T) # significative pvalue = significantly different



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
high.poor.lowcomp.lowpop = data.frame(competition = rep("Low Competition", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE,large = max(m.data$large), pop.10.m = min(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .25), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.poor.highcomp.lowpop = data.frame(competition = rep("High Competition", 1000000),income = rep("Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE,large = max(m.data$large), pop.10.m = min(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .25), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.lowcomp.lowpop = data.frame(competition = rep("Low Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = max(m.data$large), pop.10.m = min(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .75), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.highcomp.lowpop = data.frame(competition = rep("High Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = max(m.data$large), pop.10.m = min(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .75), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.lowcomp.lowpop = data.frame(competition = rep("Low Competition", 1000000),income = rep("Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = min(m.data$large), pop.10.m = min(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .25), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.highcomp.lowpop = data.frame(competition = rep("High Competition", 1000000),income = rep("Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = min(m.data$large), pop.10.m = min(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .25), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.lowcomp.lowpop = data.frame(competition = rep("Low Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = min(m.data$large), pop.10.m = min(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .75), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.highcomp.lowpop = data.frame(competition = rep("High Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = min(m.data$large), pop.10.m = min(m.data$pop.10.m) ,wealth= quantile(m.data$wealth, .75), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))



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
### PUT IN APPENDIX: SAY THAT I DIDNT FIND SUPPORT //

set.seed(602); options(scipen=999)
# simulation DISTRIBUTION PLOTS //
high.poor.highcomp.dem =  data.frame(dem = rep("Democratic", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE, large = max(m.data$large), wealth= quantile(m.data$wealth, .25), ing4 = max(m.data$ing4),munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.highcomp.dem =  data.frame(dem = rep("Democratic", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE, large = min(m.data$large), wealth= quantile(m.data$wealth, .25), ing4 = max(m.data$ing4),munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.highcomp.dem =  data.frame(dem = rep("Democratic", 1000000), income = rep("Non-Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE, large = max(m.data$large), wealth= quantile(m.data$wealth, .75), ing4 = max(m.data$ing4),munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.highcomp.dem =  data.frame(dem = rep("Democratic", 1000000), income = rep("Non-Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE, large = min(m.data$large), wealth= quantile(m.data$wealth, .75), ing4 = max(m.data$ing4),munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.poor.highcomp.nondem =  data.frame(dem = rep("Non-Democratic", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE, large = max(m.data$large), wealth= quantile(m.data$wealth, .25), ing4 = min(m.data$ing4),munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.highcomp.nondem =  data.frame(dem = rep("Non-Democratic", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE, large = min(m.data$large), wealth= quantile(m.data$wealth, .25), ing4 = min(m.data$ing4),munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.highcomp.nondem =  data.frame(dem = rep("Non-Democratic", 1000000), income = rep("Non-Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE, large = max(m.data$large), wealth= quantile(m.data$wealth, .75), ing4 = min(m.data$ing4),munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.highcomp.nondem =  data.frame(dem = rep("Non-Democratic", 1000000), income = rep("Non-Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE, large = min(m.data$large), wealth= quantile(m.data$wealth, .75), ing4 = min(m.data$ing4),munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))


high.poor.lowcomp.dem =  data.frame(dem = rep("Democratic", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE, large = max(m.data$large), wealth= quantile(m.data$wealth, .25), ing4 = max(m.data$ing4),munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.lowcomp.dem =  data.frame(dem = rep("Democratic", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE, large = min(m.data$large), wealth= quantile(m.data$wealth, .25), ing4 = max(m.data$ing4),munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.lowcomp.dem =  data.frame(dem = rep("Democratic", 1000000), income = rep("Non-Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE, large = max(m.data$large), wealth= quantile(m.data$wealth, .75), ing4 = max(m.data$ing4),munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.lowcomp.dem =  data.frame(dem = rep("Democratic", 1000000), income = rep("Non-Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE, large = min(m.data$large), wealth= quantile(m.data$wealth, .75), ing4 = max(m.data$ing4),munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.poor.lowcomp.nondem =  data.frame(dem = rep("Non-Democratic", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE, large = max(m.data$large), wealth= quantile(m.data$wealth, .25), ing4 = min(m.data$ing4),munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.lowcomp.nondem =  data.frame(dem = rep("Non-Democratic", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE, large = min(m.data$large), wealth= quantile(m.data$wealth, .25), ing4 = min(m.data$ing4),munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.lowcomp.nondem =  data.frame(dem = rep("Non-Democratic", 1000000), income = rep("Non-Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE, large = max(m.data$large), wealth= quantile(m.data$wealth, .75), ing4 = min(m.data$ing4),munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.lowcomp.nondem =  data.frame(dem = rep("Non-Democratic", 1000000), income = rep("Non-Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE, large = min(m.data$large), wealth= quantile(m.data$wealth, .75), ing4 = min(m.data$ing4),munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))



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
#  LARGE * POP
##########################

## low 
model.m.s.low = data.frame(
  sim(x = setx(model.m.s, cond = TRUE,
               large = min(m.data$large), 
               pop.10 = min(m.data$pop.10):max(m.data$pop.10)), 
      num=300)$getqi(qi="ev", xvalue="range"))
colnames(model.m.s.low) <- seq(1:ncol(as.data.frame(t(min(m.data$pop.10):max(m.data$pop.10)))))  # low


## high
gee.dich.m.2.high = data.frame(
  sim(x = setx(model.m.s, cond = TRUE,
               large = max(m.data$large), 
               pop.10 = min(m.data$pop.10):max(m.data$pop.10)),num=300)$getqi(qi="ev", xvalue="range"))
colnames(gee.dich.m.2.high) <- seq(1:ncol(as.data.frame(t(min(m.data$pop.10):max(m.data$pop.10)))))  # high


## pop.10
gee.dich.m.2.pop.10 = data.frame(
  sim(x = setx(model.m.s, cond = TRUE,
               pop.10 = min(m.data$pop.10):max(m.data$pop.10)),num=300)$getqi(qi="ev", xvalue="range"))
colnames(gee.dich.m.2.pop.10) <- seq(1:ncol(as.data.frame(t(min(m.data$pop.10):max(m.data$pop.10)))))  # high



library(Rmisc) # install.packages("Rmisc")

### low
df.low = data.frame(
  mean = c(mean(model.m.s.low$`1`),mean(model.m.s.low$`2`),mean(model.m.s.low$`3`),mean(model.m.s.low$`4`),mean(model.m.s.low$`5`),mean(model.m.s.low$`6`),mean(model.m.s.low$`7`),mean(model.m.s.low$`8`), mean(model.m.s.low$`9`),mean(model.m.s.low$`10`)),
  Population = min(m.data$pop.10):max(m.data$pop.10),
  Poverty = rep("Low Density", ncol(model.m.s.low)),
  Upper = c(as.numeric(CI(model.m.s.low$`1`)[1]), as.numeric(CI(model.m.s.low$`2`)[1]), as.numeric(CI(model.m.s.low$`3`)[1]), as.numeric(CI(model.m.s.low$`4`)[1]), as.numeric(CI(model.m.s.low$`5`)[1]), as.numeric(CI(model.m.s.low$`6`)[1]), as.numeric(CI(model.m.s.low$`7`)[1]), as.numeric(CI(model.m.s.low$`8`)[1]), as.numeric(CI(model.m.s.low$`9`)[1]), as.numeric(CI(model.m.s.low$`10`)[1])),
  Lower =c(as.numeric(CI(model.m.s.low$`1`)[3]), as.numeric(CI(model.m.s.low$`2`)[3]), as.numeric(CI(model.m.s.low$`3`)[3]), as.numeric(CI(model.m.s.low$`4`)[3]), as.numeric(CI(model.m.s.low$`5`)[3]), as.numeric(CI(model.m.s.low$`6`)[3]), as.numeric(CI(model.m.s.low$`7`)[3]), as.numeric(CI(model.m.s.low$`8`)[3]), as.numeric(CI(model.m.s.low$`9`)[3]), as.numeric(CI(model.m.s.low$`10`)[3]))
)



### high
df.high = data.frame(
  mean = c(mean(gee.dich.m.2.high$`1`),mean(gee.dich.m.2.high$`2`),mean(gee.dich.m.2.high$`3`),mean(gee.dich.m.2.high$`4`),mean(gee.dich.m.2.high$`5`),mean(gee.dich.m.2.high$`6`),mean(gee.dich.m.2.high$`7`),mean(gee.dich.m.2.high$`8`), mean(gee.dich.m.2.high$`9`),mean(gee.dich.m.2.high$`10`)),
  Population = min(m.data$pop.10):max(m.data$pop.10),
  Poverty = rep("High Density", ncol(gee.dich.m.2.high)),
  Upper = c(as.numeric(CI(gee.dich.m.2.high$`1`)[1]), as.numeric(CI(gee.dich.m.2.high$`2`)[1]), as.numeric(CI(gee.dich.m.2.high$`3`)[1]), as.numeric(CI(gee.dich.m.2.high$`4`)[1]), as.numeric(CI(gee.dich.m.2.high$`5`)[1]), as.numeric(CI(gee.dich.m.2.high$`6`)[1]), as.numeric(CI(gee.dich.m.2.high$`7`)[1]), as.numeric(CI(gee.dich.m.2.high$`8`)[1]), as.numeric(CI(gee.dich.m.2.high$`9`)[1]), as.numeric(CI(gee.dich.m.2.high$`10`)[1])),
  Lower =c(as.numeric(CI(gee.dich.m.2.high$`1`)[3]), as.numeric(CI(gee.dich.m.2.high$`2`)[3]), as.numeric(CI(gee.dich.m.2.high$`3`)[3]), as.numeric(CI(gee.dich.m.2.high$`4`)[3]), as.numeric(CI(gee.dich.m.2.high$`5`)[3]), as.numeric(CI(gee.dich.m.2.high$`6`)[3]), as.numeric(CI(gee.dich.m.2.high$`7`)[3]), as.numeric(CI(gee.dich.m.2.high$`8`)[3]), as.numeric(CI(gee.dich.m.2.high$`9`)[3]), as.numeric(CI(gee.dich.m.2.high$`10`)[3]))
)

### pop
df.pop.alone = data.frame(
  mean = c(mean(gee.dich.m.2.pop.10$`1`),mean(gee.dich.m.2.pop.10$`2`),mean(gee.dich.m.2.pop.10$`3`),mean(gee.dich.m.2.pop.10$`4`),mean(gee.dich.m.2.pop.10$`5`),mean(gee.dich.m.2.pop.10$`6`),mean(gee.dich.m.2.pop.10$`7`),mean(gee.dich.m.2.pop.10$`8`), mean(gee.dich.m.2.pop.10$`9`),mean(gee.dich.m.2.pop.10$`10`)),
  Population = min(m.data$pop.10):max(m.data$pop.10),
  Poverty = rep("Population Size", ncol(gee.dich.m.2.pop.10)),
  Upper = c(as.numeric(CI(gee.dich.m.2.pop.10$`1`)[1]), as.numeric(CI(gee.dich.m.2.pop.10$`2`)[1]), as.numeric(CI(gee.dich.m.2.pop.10$`3`)[1]), as.numeric(CI(gee.dich.m.2.pop.10$`4`)[1]), as.numeric(CI(gee.dich.m.2.pop.10$`5`)[1]), as.numeric(CI(gee.dich.m.2.pop.10$`6`)[1]), as.numeric(CI(gee.dich.m.2.pop.10$`7`)[1]), as.numeric(CI(gee.dich.m.2.pop.10$`8`)[1]), as.numeric(CI(gee.dich.m.2.pop.10$`9`)[1]), as.numeric(CI(gee.dich.m.2.pop.10$`10`)[1])),
  Lower =c(as.numeric(CI(gee.dich.m.2.pop.10$`1`)[3]), as.numeric(CI(gee.dich.m.2.pop.10$`2`)[3]), as.numeric(CI(gee.dich.m.2.pop.10$`3`)[3]), as.numeric(CI(gee.dich.m.2.pop.10$`4`)[3]), as.numeric(CI(gee.dich.m.2.pop.10$`5`)[3]), as.numeric(CI(gee.dich.m.2.pop.10$`6`)[3]), as.numeric(CI(gee.dich.m.2.pop.10$`7`)[3]), as.numeric(CI(gee.dich.m.2.pop.10$`8`)[3]), as.numeric(CI(gee.dich.m.2.pop.10$`9`)[3]), as.numeric(CI(gee.dich.m.2.pop.10$`10`)[3]))
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
#  LARGE * WEALTH:
##########################


# METHOD 2
# library(devtools) # install.packages("devtools")
# install_github('IQSS/Zelig')
#library(Zelig) # install.packages("Zelig", dependencies=TRUE) # Models

# simulation
library(Zelig)
set.seed(602); options(scipen=999)



## low 
model.m.s.low = data.frame(
  sim(x = setx(model.m.s, cond = TRUE,
               large = min(m.data$large), 
               large:wealth,
               wealth = min(m.data$wealth):max(m.data$wealth)), 
      num=300)$getqi(qi="ev", xvalue="range"))
colnames(model.m.s.low) <- seq(1:ncol(as.data.frame(t(min(m.data$wealth):max(m.data$wealth)))))  # high


## high
model.m.s.high = data.frame(
  sim(x = setx(model.m.s, cond = TRUE,
               large = max(m.data$large), 
               large:wealth,
               wealth = min(m.data$wealth):max(m.data$wealth)),num=300)$getqi(qi="ev", xvalue="range"))
colnames(model.m.s.high) <- seq(1:ncol(as.data.frame(t(min(m.data$wealth):max(m.data$wealth)))))  # high



## wealth
model.m.s.wealth = data.frame(
  sim(x = setx(model.m.s, cond = TRUE,
               large:wealth,
               wealth = min(m.data$wealth):max(m.data$wealth)), 
      num=300)$getqi(qi="ev", xvalue="range"))
colnames(model.m.s.wealth) <- seq(1:ncol(as.data.frame(t(min(m.data$wealth):max(m.data$wealth)))))  # high


# to compute confidence intervals
library(Rmisc) # install.packages("Rmisc")

### low
df.low = data.frame(
  mean = c(mean(model.m.s.low$`1`),mean(model.m.s.low$`2`),mean(model.m.s.low$`3`),mean(model.m.s.low$`4`),mean(model.m.s.low$`5`),mean(model.m.s.low$`6`),mean(model.m.s.low$`7`),mean(model.m.s.low$`8`), mean(model.m.s.low$`9`),mean(model.m.s.low$`10`), mean(model.m.s.low$`11`), mean(model.m.s.low$`12`),mean(model.m.s.low$`13`),mean(model.m.s.low$`14`),mean(model.m.s.low$`15`),mean(model.m.s.low$`16`),mean(model.m.s.low$`17`),mean(model.m.s.low$`18`),mean(model.m.s.low$`19`),mean(model.m.s.low$`20`),mean(model.m.s.low$`21`),mean(model.m.s.low$`22`),mean(model.m.s.low$`23`),mean(model.m.s.low$`24`),mean(model.m.s.low$`25`)),Wealth = min(m.data$wealth):max(m.data$wealth),
  Poverty = rep("Low Density", ncol(model.m.s.low)),
  Upper = c(as.numeric(CI(model.m.s.low$`1`)[1]), as.numeric(CI(model.m.s.low$`2`)[1]), as.numeric(CI(model.m.s.low$`3`)[1]), as.numeric(CI(model.m.s.low$`4`)[1]), as.numeric(CI(model.m.s.low$`5`)[1]), as.numeric(CI(model.m.s.low$`6`)[1]), as.numeric(CI(model.m.s.low$`7`)[1]), as.numeric(CI(model.m.s.low$`8`)[1]), as.numeric(CI(model.m.s.low$`9`)[1]), as.numeric(CI(model.m.s.low$`10`)[1]), as.numeric(CI(model.m.s.low$`11`)[1]), as.numeric(CI(model.m.s.low$`12`)[1]), as.numeric(CI(model.m.s.low$`13`)[1]), as.numeric(CI(model.m.s.low$`14`)[1]), as.numeric(CI(model.m.s.low$`15`)[1]), as.numeric(CI(model.m.s.low$`16`)[1]), as.numeric(CI(model.m.s.low$`17`)[1]), as.numeric(CI(model.m.s.low$`18`)[1]), as.numeric(CI(model.m.s.low$`19`)[1]), as.numeric(CI(model.m.s.low$`20`)[1]), as.numeric(CI(model.m.s.low$`21`)[1]), as.numeric(CI(model.m.s.low$`22`)[1]), as.numeric(CI(model.m.s.low$`23`)[1]), as.numeric(CI(model.m.s.low$`24`)[1]), as.numeric(CI(model.m.s.low$`25`)[1])),
  Lower =c(as.numeric(CI(model.m.s.low$`1`)[3]), as.numeric(CI(model.m.s.low$`2`)[3]), as.numeric(CI(model.m.s.low$`3`)[3]), as.numeric(CI(model.m.s.low$`4`)[3]), as.numeric(CI(model.m.s.low$`5`)[3]), as.numeric(CI(model.m.s.low$`6`)[3]), as.numeric(CI(model.m.s.low$`7`)[3]), as.numeric(CI(model.m.s.low$`8`)[3]), as.numeric(CI(model.m.s.low$`9`)[3]), as.numeric(CI(model.m.s.low$`10`)[3]), as.numeric(CI(model.m.s.low$`11`)[3]), as.numeric(CI(model.m.s.low$`12`)[3]), as.numeric(CI(model.m.s.low$`13`)[3]), as.numeric(CI(model.m.s.low$`14`)[3]), as.numeric(CI(model.m.s.low$`15`)[3]), as.numeric(CI(model.m.s.low$`16`)[3]), as.numeric(CI(model.m.s.low$`17`)[3]), as.numeric(CI(model.m.s.low$`18`)[3]), as.numeric(CI(model.m.s.low$`19`)[3]), as.numeric(CI(model.m.s.low$`20`)[3]), as.numeric(CI(model.m.s.low$`21`)[3]), as.numeric(CI(model.m.s.low$`22`)[3]), as.numeric(CI(model.m.s.low$`23`)[3]), as.numeric(CI(model.m.s.low$`24`)[3]), as.numeric(CI(model.m.s.low$`25`)[3]))
)



### high
df.high = data.frame(
  mean = c(mean(model.m.s.high$`1`),mean(model.m.s.high$`2`),mean(model.m.s.high$`3`),mean(model.m.s.high$`4`),mean(model.m.s.high$`5`),mean(model.m.s.high$`6`),mean(model.m.s.high$`7`),mean(model.m.s.high$`8`),mean(model.m.s.high$`9`),mean(model.m.s.high$`10`),mean(model.m.s.high$`11`),mean(model.m.s.high$`12`),mean(model.m.s.high$`13`),mean(model.m.s.high$`14`),mean(model.m.s.high$`15`),mean(model.m.s.high$`16`),mean(model.m.s.high$`17`),mean(model.m.s.high$`18`),mean(model.m.s.high$`19`),mean(model.m.s.high$`20`),mean(model.m.s.high$`21`),mean(model.m.s.high$`22`),mean(model.m.s.high$`23`),mean(model.m.s.high$`24`),mean(model.m.s.high$`25`)),
  Wealth = min(m.data$wealth):max(m.data$wealth),
  Poverty = rep("High Density", ncol(model.m.s.high)),
  Upper = c(as.numeric(CI(model.m.s.high$`1`)[1]), as.numeric(CI(model.m.s.high$`2`)[1]), as.numeric(CI(model.m.s.high$`3`)[1]), as.numeric(CI(model.m.s.high$`4`)[1]), as.numeric(CI(model.m.s.high$`5`)[1]), as.numeric(CI(model.m.s.high$`6`)[1]), as.numeric(CI(model.m.s.high$`7`)[1]), as.numeric(CI(model.m.s.high$`8`)[1]), as.numeric(CI(model.m.s.high$`9`)[1]), as.numeric(CI(model.m.s.high$`10`)[1]), as.numeric(CI(model.m.s.high$`11`)[1]), as.numeric(CI(model.m.s.high$`12`)[1]), as.numeric(CI(model.m.s.high$`13`)[1]), as.numeric(CI(model.m.s.high$`14`)[1]), as.numeric(CI(model.m.s.high$`15`)[1]), as.numeric(CI(model.m.s.high$`16`)[1]), as.numeric(CI(model.m.s.high$`17`)[1]), as.numeric(CI(model.m.s.high$`18`)[1]), as.numeric(CI(model.m.s.high$`19`)[1]), as.numeric(CI(model.m.s.high$`20`)[1]), as.numeric(CI(model.m.s.high$`21`)[1]), as.numeric(CI(model.m.s.high$`22`)[1]), as.numeric(CI(model.m.s.high$`23`)[1]), as.numeric(CI(model.m.s.high$`24`)[1]), as.numeric(CI(model.m.s.high$`25`)[1])),
  Lower =c(as.numeric(CI(model.m.s.high$`1`)[3]), as.numeric(CI(model.m.s.high$`2`)[3]), as.numeric(CI(model.m.s.high$`3`)[3]), as.numeric(CI(model.m.s.high$`4`)[3]), as.numeric(CI(model.m.s.high$`5`)[3]), as.numeric(CI(model.m.s.high$`6`)[3]), as.numeric(CI(model.m.s.high$`7`)[3]), as.numeric(CI(model.m.s.high$`8`)[3]), as.numeric(CI(model.m.s.high$`9`)[3]), as.numeric(CI(model.m.s.high$`10`)[3]), as.numeric(CI(model.m.s.high$`11`)[3]), as.numeric(CI(model.m.s.high$`12`)[3]), as.numeric(CI(model.m.s.high$`13`)[3]), as.numeric(CI(model.m.s.high$`14`)[3]), as.numeric(CI(model.m.s.high$`15`)[3]), as.numeric(CI(model.m.s.high$`16`)[3]), as.numeric(CI(model.m.s.high$`17`)[3]), as.numeric(CI(model.m.s.high$`18`)[3]), as.numeric(CI(model.m.s.high$`19`)[3]), as.numeric(CI(model.m.s.high$`20`)[3]), as.numeric(CI(model.m.s.high$`21`)[3]), as.numeric(CI(model.m.s.high$`22`)[3]), as.numeric(CI(model.m.s.high$`23`)[3]), as.numeric(CI(model.m.s.high$`24`)[3]), as.numeric(CI(model.m.s.high$`25`)[3]))
)

### wealth
df.wealth.alone = data.frame(
  mean = c(mean(model.m.s.wealth$`1`),mean(model.m.s.wealth$`2`),mean(model.m.s.wealth$`3`),mean(model.m.s.wealth$`4`),mean(model.m.s.wealth$`5`),mean(model.m.s.wealth$`6`),mean(model.m.s.wealth$`7`),mean(model.m.s.wealth$`8`),mean(model.m.s.wealth$`9`),mean(model.m.s.wealth$`10`),mean(model.m.s.wealth$`11`),mean(model.m.s.wealth$`12`),mean(model.m.s.wealth$`13`),mean(model.m.s.wealth$`14`),mean(model.m.s.wealth$`15`),mean(model.m.s.wealth$`16`),mean(model.m.s.wealth$`17`),mean(model.m.s.wealth$`18`),mean(model.m.s.wealth$`19`),mean(model.m.s.wealth$`20`),mean(model.m.s.wealth$`21`),mean(model.m.s.wealth$`22`),mean(model.m.s.wealth$`23`),mean(model.m.s.wealth$`24`),mean(model.m.s.wealth$`25`)),
  Wealth = min(m.data$wealth):max(m.data$wealth),
  Poverty = rep("Wealth Index", ncol(model.m.s.wealth)),
  Upper = c(as.numeric(CI(model.m.s.wealth$`1`)[1]), as.numeric(CI(model.m.s.wealth$`2`)[1]), as.numeric(CI(model.m.s.wealth$`3`)[1]), as.numeric(CI(model.m.s.wealth$`4`)[1]), as.numeric(CI(model.m.s.wealth$`5`)[1]), as.numeric(CI(model.m.s.wealth$`6`)[1]), as.numeric(CI(model.m.s.wealth$`7`)[1]), as.numeric(CI(model.m.s.wealth$`8`)[1]), as.numeric(CI(model.m.s.wealth$`9`)[1]), as.numeric(CI(model.m.s.wealth$`10`)[1]), as.numeric(CI(model.m.s.wealth$`11`)[1]), as.numeric(CI(model.m.s.wealth$`12`)[1]), as.numeric(CI(model.m.s.wealth$`13`)[1]), as.numeric(CI(model.m.s.wealth$`14`)[1]), as.numeric(CI(model.m.s.wealth$`15`)[1]), as.numeric(CI(model.m.s.wealth$`16`)[1]), as.numeric(CI(model.m.s.wealth$`17`)[1]), as.numeric(CI(model.m.s.wealth$`18`)[1]), as.numeric(CI(model.m.s.wealth$`19`)[1]), as.numeric(CI(model.m.s.wealth$`20`)[1]), as.numeric(CI(model.m.s.wealth$`21`)[1]), as.numeric(CI(model.m.s.wealth$`22`)[1]), as.numeric(CI(model.m.s.wealth$`23`)[1]), as.numeric(CI(model.m.s.wealth$`24`)[1]), as.numeric(CI(model.m.s.wealth$`25`)[1])),
  Lower =c(as.numeric(CI(model.m.s.wealth$`1`)[3]), as.numeric(CI(model.m.s.wealth$`2`)[3]), as.numeric(CI(model.m.s.wealth$`3`)[3]), as.numeric(CI(model.m.s.wealth$`4`)[3]), as.numeric(CI(model.m.s.wealth$`5`)[3]), as.numeric(CI(model.m.s.wealth$`6`)[3]), as.numeric(CI(model.m.s.wealth$`7`)[3]), as.numeric(CI(model.m.s.wealth$`8`)[3]), as.numeric(CI(model.m.s.wealth$`9`)[3]), as.numeric(CI(model.m.s.wealth$`10`)[3]), as.numeric(CI(model.m.s.wealth$`11`)[3]), as.numeric(CI(model.m.s.wealth$`12`)[3]), as.numeric(CI(model.m.s.wealth$`13`)[3]), as.numeric(CI(model.m.s.wealth$`14`)[3]), as.numeric(CI(model.m.s.wealth$`15`)[3]), as.numeric(CI(model.m.s.wealth$`16`)[3]), as.numeric(CI(model.m.s.wealth$`17`)[3]), as.numeric(CI(model.m.s.wealth$`18`)[3]), as.numeric(CI(model.m.s.wealth$`19`)[3]), as.numeric(CI(model.m.s.wealth$`20`)[3]), as.numeric(CI(model.m.s.wealth$`21`)[3]), as.numeric(CI(model.m.s.wealth$`22`)[3]), as.numeric(CI(model.m.s.wealth$`23`)[3]), as.numeric(CI(model.m.s.wealth$`24`)[3]), as.numeric(CI(model.m.s.wealth$`25`)[3]))
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
#  LARGE * MUNOPP:
##########################


# simulation
library(Zelig)
set.seed(602); options(scipen=999)


# low 
model.m.s.low = data.frame(
  sim(
    x = setx(model.m.s, cond = TRUE,
             large = min(m.data$large), 
             large:munopp,
             munopp = min(m.data$munopp):max(m.data$munopp)), 
    num=200)$getqi(qi="ev", xvalue="range"))

colnames(model.m.s.low) <- seq(1:ncol(as.data.frame(t(min(m.data$munopp):max(m.data$munopp)))))  # low



# high 
model.m.s.high = data.frame(
  sim(
    x = setx(model.m.s, cond = TRUE,
             large = max(m.data$large), 
             large:munopp,
             munopp = min(m.data$munopp):max(m.data$munopp)), 
    num=200)$getqi(qi="ev", xvalue="range"))

colnames(model.m.s.high) <- seq(1:ncol(as.data.frame(t(min(m.data$munopp):max(m.data$munopp)))))  # low



# munopp 
model.m.s.munopp = data.frame(
  sim(x = setx(model.m.s, cond = TRUE,
               large:munopp,
               munopp = min(m.data$munopp):max(m.data$munopp)), 
      num=300)$getqi(qi="ev", xvalue="range"))
colnames(model.m.s.munopp) <- seq(1:ncol(as.data.frame(t(min(m.data$munopp):max(m.data$munopp)))))  # high


# to compute confidence intervals
library(Rmisc) # install.packages("Rmisc")


### df's
### low
df.low = data.frame(
  mean = c(mean(model.m.s.low$`1`),mean(model.m.s.low$`2`),mean(model.m.s.low$`3`),mean(model.m.s.low$`4`),mean(model.m.s.low$`5`),mean(model.m.s.low$`6`),mean(model.m.s.low$`7`),mean(model.m.s.low$`8`), mean(model.m.s.low$`9`),mean(model.m.s.low$`10`), mean(model.m.s.low$`11`), mean(model.m.s.low$`12`),mean(model.m.s.low$`13`),mean(model.m.s.low$`14`),mean(model.m.s.low$`15`),mean(model.m.s.low$`16`),mean(model.m.s.low$`17`),mean(model.m.s.low$`18`),mean(model.m.s.low$`19`),mean(model.m.s.low$`20`), mean(model.m.s.low$`21`),mean(model.m.s.low$`22`),mean(model.m.s.low$`23`),mean(model.m.s.low$`24`),mean(model.m.s.low$`25`),mean(model.m.s.low$`26`),mean(model.m.s.low$`27`),mean(model.m.s.low$`28`), mean(model.m.s.low$`29`),mean(model.m.s.low$`30`), mean(model.m.s.low$`31`), mean(model.m.s.low$`32`),mean(model.m.s.low$`33`),mean(model.m.s.low$`34`),mean(model.m.s.low$`35`),mean(model.m.s.low$`36`),mean(model.m.s.low$`37`),mean(model.m.s.low$`38`),mean(model.m.s.low$`39`),mean(model.m.s.low$`40`),mean(model.m.s.low$`41`)),
  Type = rep("Low Density", ncol(model.m.s.low)),
  Opposition = min(m.data$munopp):max(m.data$munopp),
  Upper = c(as.numeric(CI(model.m.s.low$`1`)[1]), as.numeric(CI(model.m.s.low$`2`)[1]), as.numeric(CI(model.m.s.low$`3`)[1]), as.numeric(CI(model.m.s.low$`4`)[1]), as.numeric(CI(model.m.s.low$`5`)[1]), as.numeric(CI(model.m.s.low$`6`)[1]), as.numeric(CI(model.m.s.low$`7`)[1]), as.numeric(CI(model.m.s.low$`8`)[1]), as.numeric(CI(model.m.s.low$`9`)[1]), as.numeric(CI(model.m.s.low$`10`)[1]), as.numeric(CI(model.m.s.low$`11`)[1]), as.numeric(CI(model.m.s.low$`12`)[1]), as.numeric(CI(model.m.s.low$`13`)[1]), as.numeric(CI(model.m.s.low$`14`)[1]), as.numeric(CI(model.m.s.low$`15`)[1]), as.numeric(CI(model.m.s.low$`16`)[1]), as.numeric(CI(model.m.s.low$`17`)[1]), as.numeric(CI(model.m.s.low$`18`)[1]), as.numeric(CI(model.m.s.low$`19`)[1]), as.numeric(CI(model.m.s.low$`20`)[1]), as.numeric(CI(model.m.s.low$`21`)[1]), as.numeric(CI(model.m.s.low$`22`)[1]), as.numeric(CI(model.m.s.low$`23`)[1]), as.numeric(CI(model.m.s.low$`24`)[1]), as.numeric(CI(model.m.s.low$`25`)[1]), as.numeric(CI(model.m.s.low$`26`)[1]), as.numeric(CI(model.m.s.low$`27`)[1]), as.numeric(CI(model.m.s.low$`28`)[1]), as.numeric(CI(model.m.s.low$`29`)[1]), as.numeric(CI(model.m.s.low$`30`)[1]), as.numeric(CI(model.m.s.low$`31`)[1]), as.numeric(CI(model.m.s.low$`32`)[1]), as.numeric(CI(model.m.s.low$`33`)[1]), as.numeric(CI(model.m.s.low$`34`)[1]), as.numeric(CI(model.m.s.low$`35`)[1]), as.numeric(CI(model.m.s.low$`36`)[1]), as.numeric(CI(model.m.s.low$`37`)[1]), as.numeric(CI(model.m.s.low$`38`)[1]), as.numeric(CI(model.m.s.low$`39`)[1]), as.numeric(CI(model.m.s.low$`40`)[1]),as.numeric(CI(model.m.s.low$`41`)[1])),
  Lower = c(
    as.numeric(CI(model.m.s.low$`1`)[3]), as.numeric(CI(model.m.s.low$`2`)[3]), as.numeric(CI(model.m.s.low$`3`)[3]), as.numeric(CI(model.m.s.low$`4`)[3]), as.numeric(CI(model.m.s.low$`5`)[3]), as.numeric(CI(model.m.s.low$`6`)[3]), as.numeric(CI(model.m.s.low$`7`)[3]), as.numeric(CI(model.m.s.low$`8`)[3]), as.numeric(CI(model.m.s.low$`9`)[3]), as.numeric(CI(model.m.s.low$`10`)[3]), as.numeric(CI(model.m.s.low$`11`)[3]), as.numeric(CI(model.m.s.low$`12`)[3]), as.numeric(CI(model.m.s.low$`13`)[3]), as.numeric(CI(model.m.s.low$`14`)[3]), as.numeric(CI(model.m.s.low$`15`)[3]), as.numeric(CI(model.m.s.low$`16`)[3]), as.numeric(CI(model.m.s.low$`17`)[3]), as.numeric(CI(model.m.s.low$`18`)[3]), as.numeric(CI(model.m.s.low$`19`)[3]), as.numeric(CI(model.m.s.low$`20`)[3]),as.numeric(CI(model.m.s.low$`21`)[3]), as.numeric(CI(model.m.s.low$`22`)[3]), as.numeric(CI(model.m.s.low$`23`)[3]), as.numeric(CI(model.m.s.low$`24`)[3]), as.numeric(CI(model.m.s.low$`25`)[3]), as.numeric(CI(model.m.s.low$`26`)[3]), as.numeric(CI(model.m.s.low$`27`)[3]), as.numeric(CI(model.m.s.low$`28`)[3]), as.numeric(CI(model.m.s.low$`29`)[3]), as.numeric(CI(model.m.s.low$`30`)[3]), as.numeric(CI(model.m.s.low$`31`)[3]), as.numeric(CI(model.m.s.low$`32`)[3]), as.numeric(CI(model.m.s.low$`33`)[3]), as.numeric(CI(model.m.s.low$`34`)[3]), as.numeric(CI(model.m.s.low$`35`)[3]), as.numeric(CI(model.m.s.low$`36`)[3]), as.numeric(CI(model.m.s.low$`37`)[3]), as.numeric(CI(model.m.s.low$`38`)[3]), as.numeric(CI(model.m.s.low$`39`)[3]), as.numeric(CI(model.m.s.low$`40`)[3]),as.numeric(CI(model.m.s.low$`41`)[3]))
)

### high
df.high = data.frame(
  mean = c(mean(model.m.s.high$`1`),mean(model.m.s.high$`2`),mean(model.m.s.high$`3`),mean(model.m.s.high$`4`),mean(model.m.s.high$`5`),mean(model.m.s.high$`6`),mean(model.m.s.high$`7`),mean(model.m.s.high$`8`), mean(model.m.s.high$`9`),mean(model.m.s.high$`10`), mean(model.m.s.high$`11`), mean(model.m.s.high$`12`),mean(model.m.s.high$`13`),mean(model.m.s.high$`14`),mean(model.m.s.high$`15`),mean(model.m.s.high$`16`),mean(model.m.s.high$`17`),mean(model.m.s.high$`18`),mean(model.m.s.high$`19`),mean(model.m.s.high$`20`), mean(model.m.s.high$`21`),mean(model.m.s.high$`22`),mean(model.m.s.high$`23`),mean(model.m.s.high$`24`),mean(model.m.s.high$`25`),mean(model.m.s.high$`26`),mean(model.m.s.high$`27`),mean(model.m.s.high$`28`), mean(model.m.s.high$`29`),mean(model.m.s.high$`30`), mean(model.m.s.high$`31`), mean(model.m.s.high$`32`),mean(model.m.s.high$`33`),mean(model.m.s.high$`34`),mean(model.m.s.high$`35`),mean(model.m.s.high$`36`),mean(model.m.s.high$`37`),mean(model.m.s.high$`38`),mean(model.m.s.high$`39`),mean(model.m.s.high$`40`),mean(model.m.s.high$`41`)),
  Type = rep("High Density", ncol(model.m.s.high)),
  Opposition = min(m.data$munopp):max(m.data$munopp),
  Upper = c(as.numeric(CI(model.m.s.high$`1`)[1]), as.numeric(CI(model.m.s.high$`2`)[1]), as.numeric(CI(model.m.s.high$`3`)[1]), as.numeric(CI(model.m.s.high$`4`)[1]), as.numeric(CI(model.m.s.high$`5`)[1]), as.numeric(CI(model.m.s.high$`6`)[1]), as.numeric(CI(model.m.s.high$`7`)[1]), as.numeric(CI(model.m.s.high$`8`)[1]), as.numeric(CI(model.m.s.high$`9`)[1]), as.numeric(CI(model.m.s.high$`10`)[1]), as.numeric(CI(model.m.s.high$`11`)[1]), as.numeric(CI(model.m.s.high$`12`)[1]), as.numeric(CI(model.m.s.high$`13`)[1]), as.numeric(CI(model.m.s.high$`14`)[1]), as.numeric(CI(model.m.s.high$`15`)[1]), as.numeric(CI(model.m.s.high$`16`)[1]), as.numeric(CI(model.m.s.high$`17`)[1]), as.numeric(CI(model.m.s.high$`18`)[1]), as.numeric(CI(model.m.s.high$`19`)[1]), as.numeric(CI(model.m.s.high$`20`)[1]), as.numeric(CI(model.m.s.high$`21`)[1]), as.numeric(CI(model.m.s.high$`22`)[1]), as.numeric(CI(model.m.s.high$`23`)[1]), as.numeric(CI(model.m.s.high$`24`)[1]), as.numeric(CI(model.m.s.high$`25`)[1]), as.numeric(CI(model.m.s.high$`26`)[1]), as.numeric(CI(model.m.s.high$`27`)[1]), as.numeric(CI(model.m.s.high$`28`)[1]), as.numeric(CI(model.m.s.high$`29`)[1]), as.numeric(CI(model.m.s.high$`30`)[1]), as.numeric(CI(model.m.s.high$`31`)[1]), as.numeric(CI(model.m.s.high$`32`)[1]), as.numeric(CI(model.m.s.high$`33`)[1]), as.numeric(CI(model.m.s.high$`34`)[1]), as.numeric(CI(model.m.s.high$`35`)[1]), as.numeric(CI(model.m.s.high$`36`)[1]), as.numeric(CI(model.m.s.high$`37`)[1]), as.numeric(CI(model.m.s.high$`38`)[1]), as.numeric(CI(model.m.s.high$`39`)[1]), as.numeric(CI(model.m.s.high$`40`)[1]),as.numeric(CI(model.m.s.high$`41`)[1])),
  Lower =c(as.numeric(CI(model.m.s.high$`1`)[3]), as.numeric(CI(model.m.s.high$`2`)[3]), as.numeric(CI(model.m.s.high$`3`)[3]), as.numeric(CI(model.m.s.high$`4`)[3]), as.numeric(CI(model.m.s.high$`5`)[3]), as.numeric(CI(model.m.s.high$`6`)[3]), as.numeric(CI(model.m.s.high$`7`)[3]), as.numeric(CI(model.m.s.high$`8`)[3]), as.numeric(CI(model.m.s.high$`9`)[3]), as.numeric(CI(model.m.s.high$`10`)[3]), as.numeric(CI(model.m.s.high$`11`)[3]), as.numeric(CI(model.m.s.high$`12`)[3]), as.numeric(CI(model.m.s.high$`13`)[3]), as.numeric(CI(model.m.s.high$`14`)[3]), as.numeric(CI(model.m.s.high$`15`)[3]), as.numeric(CI(model.m.s.high$`16`)[3]), as.numeric(CI(model.m.s.high$`17`)[3]), as.numeric(CI(model.m.s.high$`18`)[3]), as.numeric(CI(model.m.s.high$`19`)[3]), as.numeric(CI(model.m.s.high$`20`)[3]),as.numeric(CI(model.m.s.high$`21`)[3]), as.numeric(CI(model.m.s.high$`22`)[3]), as.numeric(CI(model.m.s.high$`23`)[3]), as.numeric(CI(model.m.s.high$`24`)[3]), as.numeric(CI(model.m.s.high$`25`)[3]), as.numeric(CI(model.m.s.high$`26`)[3]), as.numeric(CI(model.m.s.high$`27`)[3]), as.numeric(CI(model.m.s.high$`28`)[3]), as.numeric(CI(model.m.s.high$`29`)[3]), as.numeric(CI(model.m.s.high$`30`)[3]), as.numeric(CI(model.m.s.high$`31`)[3]), as.numeric(CI(model.m.s.high$`32`)[3]), as.numeric(CI(model.m.s.high$`33`)[3]), as.numeric(CI(model.m.s.high$`34`)[3]), as.numeric(CI(model.m.s.high$`35`)[3]), as.numeric(CI(model.m.s.high$`36`)[3]), as.numeric(CI(model.m.s.high$`37`)[3]), as.numeric(CI(model.m.s.high$`38`)[3]), as.numeric(CI(model.m.s.high$`39`)[3]), as.numeric(CI(model.m.s.high$`40`)[3]),as.numeric(CI(model.m.s.high$`41`)[3]))
)

### munopp
df.munopp.alone = data.frame(
  mean = c(mean(model.m.s.munopp$`1`),mean(model.m.s.munopp$`2`),mean(model.m.s.munopp$`3`),mean(model.m.s.munopp$`4`),mean(model.m.s.munopp$`5`),mean(model.m.s.munopp$`6`),mean(model.m.s.munopp$`7`),mean(model.m.s.munopp$`8`), mean(model.m.s.munopp$`9`),mean(model.m.s.munopp$`10`), mean(model.m.s.munopp$`11`), mean(model.m.s.munopp$`12`),mean(model.m.s.munopp$`13`),mean(model.m.s.munopp$`14`),mean(model.m.s.munopp$`15`),mean(model.m.s.munopp$`16`),mean(model.m.s.munopp$`17`),mean(model.m.s.munopp$`18`),mean(model.m.s.munopp$`19`),mean(model.m.s.munopp$`20`), mean(model.m.s.munopp$`21`),mean(model.m.s.munopp$`22`),mean(model.m.s.munopp$`23`),mean(model.m.s.munopp$`24`),mean(model.m.s.munopp$`25`),mean(model.m.s.munopp$`26`),mean(model.m.s.munopp$`27`),mean(model.m.s.munopp$`28`), mean(model.m.s.munopp$`29`),mean(model.m.s.munopp$`30`), mean(model.m.s.munopp$`31`), mean(model.m.s.munopp$`32`),mean(model.m.s.munopp$`33`),mean(model.m.s.munopp$`34`),mean(model.m.s.munopp$`35`),mean(model.m.s.munopp$`36`),mean(model.m.s.munopp$`37`),mean(model.m.s.munopp$`38`),mean(model.m.s.munopp$`39`),mean(model.m.s.munopp$`40`),mean(model.m.s.munopp$`41`)),
  Type = rep("Municipal Opposition", ncol(model.m.s.munopp)),
  Opposition = min(m.data$munopp):max(m.data$munopp),
  Upper = c(as.numeric(CI(model.m.s.munopp$`1`)[1]), as.numeric(CI(model.m.s.munopp$`2`)[1]), as.numeric(CI(model.m.s.munopp$`3`)[1]), as.numeric(CI(model.m.s.munopp$`4`)[1]), as.numeric(CI(model.m.s.munopp$`5`)[1]), as.numeric(CI(model.m.s.munopp$`6`)[1]), as.numeric(CI(model.m.s.munopp$`7`)[1]), as.numeric(CI(model.m.s.munopp$`8`)[1]), as.numeric(CI(model.m.s.munopp$`9`)[1]), as.numeric(CI(model.m.s.munopp$`10`)[1]), as.numeric(CI(model.m.s.munopp$`11`)[1]), as.numeric(CI(model.m.s.munopp$`12`)[1]), as.numeric(CI(model.m.s.munopp$`13`)[1]), as.numeric(CI(model.m.s.munopp$`14`)[1]), as.numeric(CI(model.m.s.munopp$`15`)[1]), as.numeric(CI(model.m.s.munopp$`16`)[1]), as.numeric(CI(model.m.s.munopp$`17`)[1]), as.numeric(CI(model.m.s.munopp$`18`)[1]), as.numeric(CI(model.m.s.munopp$`19`)[1]), as.numeric(CI(model.m.s.munopp$`20`)[1]), as.numeric(CI(model.m.s.munopp$`21`)[1]), as.numeric(CI(model.m.s.munopp$`22`)[1]), as.numeric(CI(model.m.s.munopp$`23`)[1]), as.numeric(CI(model.m.s.munopp$`24`)[1]), as.numeric(CI(model.m.s.munopp$`25`)[1]), as.numeric(CI(model.m.s.munopp$`26`)[1]), as.numeric(CI(model.m.s.munopp$`27`)[1]), as.numeric(CI(model.m.s.munopp$`28`)[1]), as.numeric(CI(model.m.s.munopp$`29`)[1]), as.numeric(CI(model.m.s.munopp$`30`)[1]), as.numeric(CI(model.m.s.munopp$`31`)[1]), as.numeric(CI(model.m.s.munopp$`32`)[1]), as.numeric(CI(model.m.s.munopp$`33`)[1]), as.numeric(CI(model.m.s.munopp$`34`)[1]), as.numeric(CI(model.m.s.munopp$`35`)[1]), as.numeric(CI(model.m.s.munopp$`36`)[1]), as.numeric(CI(model.m.s.munopp$`37`)[1]), as.numeric(CI(model.m.s.munopp$`38`)[1]), as.numeric(CI(model.m.s.munopp$`39`)[1]), as.numeric(CI(model.m.s.munopp$`40`)[1]),as.numeric(CI(model.m.s.munopp$`41`)[1])),
  Lower =c(as.numeric(CI(model.m.s.munopp$`1`)[3]), as.numeric(CI(model.m.s.munopp$`2`)[3]), as.numeric(CI(model.m.s.munopp$`3`)[3]), as.numeric(CI(model.m.s.munopp$`4`)[3]), as.numeric(CI(model.m.s.munopp$`5`)[3]), as.numeric(CI(model.m.s.munopp$`6`)[3]), as.numeric(CI(model.m.s.munopp$`7`)[3]), as.numeric(CI(model.m.s.munopp$`8`)[3]), as.numeric(CI(model.m.s.munopp$`9`)[3]), as.numeric(CI(model.m.s.munopp$`10`)[3]), as.numeric(CI(model.m.s.munopp$`11`)[3]), as.numeric(CI(model.m.s.munopp$`12`)[3]), as.numeric(CI(model.m.s.munopp$`13`)[3]), as.numeric(CI(model.m.s.munopp$`14`)[3]), as.numeric(CI(model.m.s.munopp$`15`)[3]), as.numeric(CI(model.m.s.munopp$`16`)[3]), as.numeric(CI(model.m.s.munopp$`17`)[3]), as.numeric(CI(model.m.s.munopp$`18`)[3]), as.numeric(CI(model.m.s.munopp$`19`)[3]), as.numeric(CI(model.m.s.munopp$`20`)[3]),as.numeric(CI(model.m.s.munopp$`21`)[3]), as.numeric(CI(model.m.s.munopp$`22`)[3]), as.numeric(CI(model.m.s.munopp$`23`)[3]), as.numeric(CI(model.m.s.munopp$`24`)[3]), as.numeric(CI(model.m.s.munopp$`25`)[3]), as.numeric(CI(model.m.s.munopp$`26`)[3]), as.numeric(CI(model.m.s.munopp$`27`)[3]), as.numeric(CI(model.m.s.munopp$`28`)[3]), as.numeric(CI(model.m.s.munopp$`29`)[3]), as.numeric(CI(model.m.s.munopp$`30`)[3]), as.numeric(CI(model.m.s.munopp$`31`)[3]), as.numeric(CI(model.m.s.munopp$`32`)[3]), as.numeric(CI(model.m.s.munopp$`33`)[3]), as.numeric(CI(model.m.s.munopp$`34`)[3]), as.numeric(CI(model.m.s.munopp$`35`)[3]), as.numeric(CI(model.m.s.munopp$`36`)[3]), as.numeric(CI(model.m.s.munopp$`37`)[3]), as.numeric(CI(model.m.s.munopp$`38`)[3]), as.numeric(CI(model.m.s.munopp$`39`)[3]), as.numeric(CI(model.m.s.munopp$`40`)[3]),as.numeric(CI(model.m.s.munopp$`41`)[3]))
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
#  LARGE * POLINV: 
##########################


# simulation
library(Zelig)
set.seed(602); options(scipen=999)


# low 
model.m.s.low = data.frame(
  sim(
    x = setx(model.m.s, cond = TRUE,
             large = min(m.data$large), 
             polinv:large,
             polinv = min(m.data$polinv):max(m.data$polinv)), 
    num=700)$getqi(qi="ev", xvalue="range"))

colnames(model.m.s.low) <- seq(1:ncol(as.data.frame(t(min(m.data$polinv):max(m.data$polinv)))))  # low

# high
model.m.s.high = data.frame(
  sim(
    x = setx(model.m.s, cond = TRUE,
             large = max(m.data$large), 
             polinv:large,
             polinv = min(m.data$polinv):max(m.data$polinv)), 
    num=700)$getqi(qi="ev", xvalue="range")) ; 
colnames(model.m.s.high) <- seq(1:ncol(as.data.frame(t(min(m.data$polinv):max(m.data$polinv)))))  # high


# polinv 
model.m.s.polinv = data.frame(
  sim(x = setx(model.m.s, cond = TRUE,
               large:munopp,
               polinv = min(m.data$polinv):max(m.data$polinv)), 
      num=300)$getqi(qi="ev", xvalue="range"))
colnames(model.m.s.polinv) <- seq(1:ncol(as.data.frame(t(min(m.data$polinv):max(m.data$polinv)))))  


# to compute confidence intervals
library(Rmisc) # install.packages("Rmisc")

### df's
### low
df.low = data.frame(
  mean = c(mean(model.m.s.low$`1`),mean(model.m.s.low$`2`),mean(model.m.s.low$`3`),mean(model.m.s.low$`4`),mean(model.m.s.low$`5`),mean(model.m.s.low$`6`),mean(model.m.s.low$`7`),mean(model.m.s.low$`8`)),
  Type = rep("Low Density", ncol(model.m.s.low)),
  Opposition = min(m.data$polinv):max(m.data$polinv),
  Upper = c(as.numeric(CI(model.m.s.low$`1`)[1]), as.numeric(CI(model.m.s.low$`2`)[1]), as.numeric(CI(model.m.s.low$`3`)[1]), as.numeric(CI(model.m.s.low$`4`)[1]), as.numeric(CI(model.m.s.low$`5`)[1]), as.numeric(CI(model.m.s.low$`6`)[1]), as.numeric(CI(model.m.s.low$`7`)[1]), as.numeric(CI(model.m.s.low$`8`)[1])),
  Lower = c(
    as.numeric(CI(model.m.s.low$`1`)[3]), as.numeric(CI(model.m.s.low$`2`)[3]), as.numeric(CI(model.m.s.low$`3`)[3]), as.numeric(CI(model.m.s.low$`4`)[3]), as.numeric(CI(model.m.s.low$`5`)[3]), as.numeric(CI(model.m.s.low$`6`)[3]), as.numeric(CI(model.m.s.low$`7`)[3]), as.numeric(CI(model.m.s.low$`8`)[3]))
)

### high
df.high = data.frame(
  mean = c(mean(model.m.s.high$`1`),mean(model.m.s.high$`2`),mean(model.m.s.high$`3`),mean(model.m.s.high$`4`),mean(model.m.s.high$`5`),mean(model.m.s.high$`6`),mean(model.m.s.high$`7`),mean(model.m.s.high$`8`)),
  Type = rep("High Density", ncol(model.m.s.high)),
  Opposition = min(m.data$polinv):max(m.data$polinv),
  Upper = c(as.numeric(CI(model.m.s.high$`1`)[1]), as.numeric(CI(model.m.s.high$`2`)[1]), as.numeric(CI(model.m.s.high$`3`)[1]), as.numeric(CI(model.m.s.high$`4`)[1]), as.numeric(CI(model.m.s.high$`5`)[1]), as.numeric(CI(model.m.s.high$`6`)[1]), as.numeric(CI(model.m.s.high$`7`)[1]), as.numeric(CI(model.m.s.high$`8`)[1])),
  Lower = c(
    as.numeric(CI(model.m.s.high$`1`)[3]), as.numeric(CI(model.m.s.high$`2`)[3]), as.numeric(CI(model.m.s.high$`3`)[3]), as.numeric(CI(model.m.s.high$`4`)[3]), as.numeric(CI(model.m.s.high$`5`)[3]), as.numeric(CI(model.m.s.high$`6`)[3]), as.numeric(CI(model.m.s.high$`7`)[3]), as.numeric(CI(model.m.s.high$`8`)[3]))
)

### polinv
df.polinv.alone = data.frame(
  mean = c(mean(model.m.s.polinv$`1`),mean(model.m.s.polinv$`2`),mean(model.m.s.polinv$`3`),mean(model.m.s.polinv$`4`),mean(model.m.s.polinv$`5`),mean(model.m.s.polinv$`6`),mean(model.m.s.polinv$`7`),mean(model.m.s.polinv$`8`)),
  Type = rep("Political Involvement", ncol(model.m.s.polinv)),
  Opposition = min(m.data$polinv):max(m.data$polinv),
  Upper = c(as.numeric(CI(model.m.s.polinv$`1`)[1]), as.numeric(CI(model.m.s.polinv$`2`)[1]), as.numeric(CI(model.m.s.polinv$`3`)[1]), as.numeric(CI(model.m.s.polinv$`4`)[1]), as.numeric(CI(model.m.s.polinv$`5`)[1]), as.numeric(CI(model.m.s.polinv$`6`)[1]), as.numeric(CI(model.m.s.polinv$`7`)[1]), as.numeric(CI(model.m.s.polinv$`8`)[1])),
  Lower =c(as.numeric(CI(model.m.s.polinv$`1`)[3]), as.numeric(CI(model.m.s.polinv$`2`)[3]), as.numeric(CI(model.m.s.polinv$`3`)[3]), as.numeric(CI(model.m.s.polinv$`4`)[3]), as.numeric(CI(model.m.s.polinv$`5`)[3]), as.numeric(CI(model.m.s.polinv$`6`)[3]), as.numeric(CI(model.m.s.polinv$`7`)[3]), as.numeric(CI(model.m.s.polinv$`8`)[3]))
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
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/dat.RData")
library(car)

# HERE


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
# Distribution of Individuals by Municipality [municipality:sample:plot]

load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/dat.RData")


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
municipality.d = data.frame(table(municipality.d))

## plot
#library(ggplot2)
#ggplot(municipality.d, aes(factor(Municipality), Freq, fill = Sample)) + geom_bar(stat = "identity") + coord_flip() +
#  xlab("Municipality") + 
#  ylab("Frequency") + 
#  #coord_flip() +
#  theme_bw()

library(ggplot2)
#mun.p1 = 
  ggplot(municipality.d, aes(x = Municipality, y = Freq, fill = Sample)) + geom_bar(stat = "identity") +
  xlab("") + 
  ylab("Frequency") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.key = element_rect(colour = NA, fill = NA, size = 0.5))
  

############################################################
# Distribution of Individuals by High/Low COnditions and municipality [municipality:income:large:plot]

  
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
#density.d$Wealth = as.numeric((density.d$Wealth-min(density.d$Wealth))/(max(density.d$Wealth)-min(density.d$Wealth))*60)

## plot
library(ggplot2)
ggplot(density.d, aes(factor(Municipality), fill = Density)) + 
  geom_bar() + 
  geom_point(data=density.d, 
             position = position_jitter(width = 0.6), 
             size = I(1),
             aes(
               x=as.factor(Municipality), 
               y=Wealth,
               alpha=Wealth))+
  #coord_flip() +
  xlab("") + 
  ylab("Frequency (matched set)") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.key = element_rect(colour = NA, fill = NA, size = 0.5))

######
## Combine mun.p1, mun.p2
#library(cowplot) # install.packages("cowplot")
#plot_grid(mun.p1,mun.p2,  nrow = 2)


############################################################
# Distribution of Individuals by High/Low COnditions and Wealth [municipality:wealth:large:plot]

load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/dat.RData")

library(ggplot2)

### Matched Set
density.wealth.m= ggplot() + geom_point(
  aes(
    y=as.factor(m.data$large), 
    x=m.data$wealth, 
    colour=m.data$large,
    alpha = 1/length(m.data$wealth)), 
  position = position_jitter(width = 5)) +
  xlab("Wealth Index: Matched Set") + 
  ylab("Density of \nthe Poor") + 
  xlim(min(dat$wealth), max(dat$wealth)) +
  theme_bw() +
  theme(
    legend.position="none", 
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10)) + 
  scale_y_discrete(breaks=c(0, 1), labels=c("Low", "High"))

### Raw Set
density.wealth.r= ggplot() + geom_point(
  aes(
    y=as.factor(dat$large), 
    x=dat$wealth, 
    colour=dat$large,
    alpha = 1/length(dat$large)), 
  position = position_jitter(width = 5)) +
  xlab("Wealth Index: Raw Set") + 
  ylab("Density of \nthe Poor") + 
  xlim(min(dat$wealth), max(dat$wealth)) +
  theme_bw() +
  theme(
    legend.position="none", 
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10)) + 
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
  geom_segment(data= m.data, aes(x = (wagehalf=median(m.data$wagehalf)), y = 0, xend = (wagehalf=median(m.data$wagehalf)), yend = 100), linetype="dashed", size=1.5, colour = "forestgreen") + 
  xlab("Density of the Poor") + ylab("Frequency") +
  geom_text(data = ggplot.labels1, aes(x = time, y = value, label = label), colour = "forestgreen")


## Plot Density [tgraph:plot]
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
               size=.5, 
               colour = "forestgreen") + 
               xlab("Percentage of People Living with \n Less than Half of the Minimum Wage") + 
               ylab("Density") + 
               theme_bw() +
  theme(
    legend.position="none", 
    axis.title.y = element_text(size = 10),
    axis.title.x = element_text(size = 10)) 
  


######################################################
#  B   A   L   A   N   C   E       P   L   O   T   S #
######################################################

# Density Plot: Propensity Scores, by Treatment Condition and By DIscarded 
Distance = m.out$distance
Sample = recode(as.numeric(as.vector(m.out$discarded)), "0 = 'Matched' ; 1 = 'Raw' ")
Density = recode(as.numeric(as.vector(m.out$treat)), "0 = 'Low' ; 1 = 'High' ")



library(ggplot2)
#distance.p=
ggplot() + geom_density(aes(x=Distance, colour=Sample, linetype=Density), alpha=.1) +
  ylab("Estimated Density") + 
  xlab("Distance") + 
  theme_bw() +
  theme(
    axis.title.y = element_text(size = 10), 
    axis.title.x = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10)) + 
  scale_y_discrete(breaks=c(0, 1), labels=c("Low", "High"))




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
#plot_grid(balancing1234,distance.p,  nrow = 2)



# alternative layout
#balance.matched1= plot_grid(balance.1.m,balance.2.m, nrow = 1, align = "v", scale = 1)
#balance.matched2= plot_grid(balance.1.r,balance.2.r, nrow = 1, align = "v", scale = 1)
#p12 = plot_grid(balance.matched1,balance.matched2,  ncol = 1)
#balance.matched3= plot_grid(balance.3.m,balance.4.m, nrow = 1, align = "v", scale = 1)
#balance.matched4= plot_grid(balance.3.r,balance.4.r, nrow = 1, align = "v", scale = 1)
#p34 = plot_grid(balance.matched3,balance.matched4,  ncol = 1)
#balance.p1234= plot_grid(p12,p34, nrow = 2, align = "v", scale = 1)
#plot_grid(balance.p1234,distance.p,  ncol = 2)





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

data.frame(cbind(imb.m.d, imb.r.d))





