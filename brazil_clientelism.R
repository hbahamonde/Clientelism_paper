# LOAD
#setwd("/Users/hectorbahamonde/RU/research/Clientelism_paper/Paper_Presentation")

## ---- loadings:data ----
cat("\014")
rm(list=ls())


# Load the data
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(foreign)


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
dat$munopp = round(dat$munopp, 0)


# constructing relative wealth index (Cordova 2009)
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(car)

dat$wealth1 = recode(dat$wealth1, "1 = 0 ; 2 = 1")
dat$wealth2 = recode(dat$wealth2, "1 = 0 ; 2 = 1")
dat$wealth3 = recode(dat$wealth3, "1 = 0 ; 2 = 1")
dat$wealth4 = recode(dat$wealth4, "1 = 0 ; 2 = 1")
dat$wealth5 = recode(dat$wealth5, "1 = 0 ; 2 = 1 ; 3 = 2 ; 4 = 3")
dat$wealth6 = recode(dat$wealth6, "1 = 0 ; 2 = 1")
dat$wealth7 = recode(dat$wealth7, "1 = 0 ; 2 = 1")
dat$wealth8 = recode(dat$wealth8, "1 = 0 ; 2 = 1")
dat$wealth9 = recode(dat$wealth9, "1 = 0 ; 2 = 1")
dat$wealth10 = recode(dat$wealth10, "1 = 0 ; 2 = 1")



## splitting df in two
dat.ur <- subset(dat, dat$urban == "Ur")
dat.ru <- subset(dat, dat$urban == "Ru")



## PCA for RURAL and URBAN
wealth.dat.ur = data.frame(dat.ur$wealth1,dat.ur$wealth2,dat.ur$wealth3, dat.ur$wealth4, dat.ur$wealth5, dat.ur$wealth6, dat.ur$wealth7, dat.ur$wealth8, dat.ur$wealth9, dat.ur$wealth10)
wealth.dat.ru = data.frame(dat.ru$wealth1,dat.ru$wealth2,dat.ru$wealth3, dat.ru$wealth4, dat.ru$wealth5, dat.ru$wealth6, dat.ru$wealth7, dat.ru$wealth8, dat.ru$wealth9, dat.ru$wealth10)


# get pca for rural
wealth_pca_ru <- princomp(wealth.dat.ru, scores = T)
wealth_loadings_ru <- with(wealth_pca_ru, unclass(loadings))

# attach scores to dataset
dat.ru$wealth = 
  wealth_loadings_ru[1, 1] * (dat.ru$wealth1-mean(dat.ru$wealth1) / sd(dat.ru$wealth1)) + 
  wealth_loadings_ru[2, 1] * (dat.ru$wealth2-mean(dat.ru$wealth2) / sd(dat.ru$wealth2)) + 
  wealth_loadings_ru[3, 1] * (dat.ru$wealth3-mean(dat.ru$wealth3) / sd(dat.ru$wealth3)) + 
  wealth_loadings_ru[4, 1] * (dat.ru$wealth4-mean(dat.ru$wealth4) / sd(dat.ru$wealth4)) + 
  wealth_loadings_ru[5, 1] * (dat.ru$wealth5-mean(dat.ru$wealth5) / sd(dat.ru$wealth5)) + 
  wealth_loadings_ru[6, 1] * (dat.ru$wealth6-mean(dat.ru$wealth6) / sd(dat.ru$wealth6)) + 
  wealth_loadings_ru[7, 1] * (dat.ru$wealth7-mean(dat.ru$wealth7) / sd(dat.ru$wealth7)) + 
  wealth_loadings_ru[8, 1] * (dat.ru$wealth8-mean(dat.ru$wealth8) / sd(dat.ru$wealth8)) + 
  wealth_loadings_ru[9, 1] * (dat.ru$wealth9-mean(dat.ru$wealth9) / sd(dat.ru$wealth9)) + 
  wealth_loadings_ru[10, 1] * (dat.ru$wealth10-mean(dat.ru$wealth10) / sd(dat.ru$wealth10)) 



# get pca for urban
wealth_pca_ur <- princomp(wealth.dat.ur, scores = T)
wealth_loadings_ur <- with(wealth_pca_ur, unclass(loadings))

# attach scores to dataset
dat.ur$wealth = 
  wealth_loadings_ur[1, 1] * (dat.ur$wealth1-mean(dat.ur$wealth1) / sd(dat.ur$wealth1)) + 
  wealth_loadings_ur[2, 1] * (dat.ur$wealth2-mean(dat.ur$wealth2) / sd(dat.ur$wealth2)) + 
  wealth_loadings_ur[3, 1] * (dat.ur$wealth3-mean(dat.ur$wealth3) / sd(dat.ur$wealth3)) + 
  wealth_loadings_ur[4, 1] * (dat.ur$wealth4-mean(dat.ur$wealth4) / sd(dat.ur$wealth4)) + 
  wealth_loadings_ur[5, 1] * (dat.ur$wealth5-mean(dat.ur$wealth5) / sd(dat.ur$wealth5)) + 
  wealth_loadings_ur[6, 1] * (dat.ur$wealth6-mean(dat.ur$wealth6) / sd(dat.ur$wealth6)) + 
  wealth_loadings_ur[7, 1] * (dat.ur$wealth7-mean(dat.ur$wealth7) / sd(dat.ur$wealth7)) + 
  wealth_loadings_ur[8, 1] * (dat.ur$wealth8-mean(dat.ur$wealth8) / sd(dat.ur$wealth8)) + 
  wealth_loadings_ur[9, 1] * (dat.ur$wealth9-mean(dat.ur$wealth9) / sd(dat.ur$wealth9)) + 
  wealth_loadings_ur[10, 1] * (dat.ur$wealth10-mean(dat.ur$wealth10) / sd(dat.ur$wealth10)) 


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
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(foreign)

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
pop.10.m = cut(dat$pop, breaks = c(0,
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
                                   quantile(dat$pop, 1)), 
               include.lowest=T, labels=c(
                 "0-10",
                 "11-20",
                 "21-30",
                 "31-40",
                 "41-50",
                 "51-60",
                 "61-70", 
                 "71-80", 
                 "81-90",
                 "91-100")
)

## attaching
dat$pop.10 = as.numeric(pop.10.r)


# save unmatched dataset
save(dat, file = "/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/dat.RData")


# Constructing Matched Set
set.seed(604)

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(MatchIt,optmatch)

# m.out <- matchit(large ~ wealth + munopp + polinv + pop.10,


m.out <- matchit(large ~ wealth + munopp + polinv + pop.10,
                 discard = "both", 
                 method = "full",
                 data = dat,
                 verbose = F)




#print. <- print(m.out)
sum.match = summary(m.out)


# Match Data
m.data <- match.data(m.out)


# Recode client1dummy after matching
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(car)

m.data$clien1dummy <- as.numeric(m.data$clien1dummy)
m.data$clien1dummy <- recode(m.data$clien1dummy, "1 = 0 ; 2 = 1")

# save matched dataset
save(m.data, file = "/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")


# Generating the Propensity Score 
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(CBPS)
#library(CBPS, quietly = T) # install.packages("CBPS")

fit <- CBPS(as.factor(wagehalf.4) ~  wealth + munopp + polinv + pop.10,
            #wealth,# + polinv,# + munopp + polinv + ing4,  # wealth + munopp + polinv
            data = dat, 
            iterations = 25000, 
            twostep = TRUE, # F
            method = "over", # EXACT
            ATT = 0, # 2
            standardize = F) # F


## transform the weight var. // Attaching weights to DF // sorting for GEE models
dat$weights = as.numeric(fit$weights)

save(dat, file = "/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/dat.RData")
## ----






#####################################################################
### PARAMETRIC models
#####################################################################



#####################################################################
###  M  O D E L S
#####################################################################

load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/dat.RData")


## ---- models ----
# load data
## Recode Before modeling
dat$clien1dummy <- as.numeric(dat$clien1dummy)

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(car)

dat$clien1dummy <- recode(dat$clien1dummy, "1 = 0 ; 2 = 1")

# formulas 
model.m = formula(clien1dummy ~ wealth*munopp*large + pop.10 + urban + polinv + ing4 + vb3 + exc7 + ed)
model.gps = formula(clien1dummy ~ wealth*munopp*wagehalf.4 + pop.10 + urban + polinv + ing4 + vb3 + exc7 + ed + weights)


if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(Zelig)

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
## ----


#####################################################################
###  T    A       B       L       E       
#####################################################################

### GEE: In gee there is no quasipossion, because gee is in a way already quasi.
### With GEE we do not fit a poisson glm, but use in the construction of the sandwich covariance 
### matrix the variance function of the poisson family. In Gee always an 'overdispersion' is estimated.



## ---- tab:results:data ----
# [tab:results]

load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/dat.RData")


# Recode client1dummy after matching
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(car)

dat$clien1dummy <- as.numeric(dat$clien1dummy)
dat$clien1dummy <- recode(dat$clien1dummy, "1 = 0 ; 2 = 1")



# models

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(texreg)

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


if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(geepack)

# formulas 
model.m = formula(clien1dummy ~ wealth*munopp*large + pop.10 + urban + polinv + ing4 + vb3 + exc7 + ed)
model.gps = formula(clien1dummy ~ wealth*munopp*wagehalf.4 + pop.10 + urban + polinv + ing4 + vb3 + exc7 + ed + weights)


options(scipen=999)

model.m.t = extract.geepack(model.m.model <- geeglm(model.m,
                               family = binomial(link = "logit"), 
                               id = municipality, 
                               weights = wt,
                               std.err = "san.se",
                               corstr = "exchangeable",
                               data = m.data))

model.gps.t = extract.geepack(model.gps.model <- geeglm(model.gps,
                                 family = binomial(link = "logit"), 
                                 id = municipality, 
                                 weights = wt,
                                 std.err = "san.se",
                                 corstr = "exchangeable",
                                 data = dat))

custom.coef.names = c(
        "(Intercept)", 
        "Wealth Index", 
        "Municipal Opposition", 
        "High Poor Density", 
        "Municipal Population", 
        "Urban", 
        "Political Involvement", 
        "Support for Democracy", 
        "Party Id.", 
        "Perception of Corruption", 
        "Years of Education", 
        "Wealth Index * Municipal Opposition", 
        "Wealth Index * High Poor Density", 
        "Municipal Opposition * High Poor Density", 
        "Wealth Index * Municipal Opposition * High Poor Density", 
        "Density of the Poor", 
        #"weights", 
        "Wealth Index * Density of the Poor", 
        "Municipal Opposition * Density of the Poor", 
        "Wealth Index * Municipal Opposition * Density of the Poor")
## ----


## ---- tab:results:table ----
# table
texreg(
  c(model.m.t,model.gps.t), 
  caption = "Generalized Estimating Logistic Equations: Clientelism",
custom.model.names = c("Matched","Weighted"),
custom.coef.names = custom.coef.names,
omit.coef = "weights",
label = "tab:1",
custom.note = ("\\parbox{.65\\linewidth}{\\vspace{2pt}%stars. Clustered standard errors at the municipality level. First column shows the estimates using the matched dataset. Second column shows the estimates of the weighted model (the generalized propensity score was omitted in the table). Both models are logit GEE.}"),
fontsize = "scriptsize",
digits = 3,
center = TRUE,
no.margin = TRUE, 
float.pos = "h"#,
#file = "/Users/hectorbahamonde/Desktop/Table_A3.html"
)
## ----




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

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2,grid)


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


if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(cowplot)

plot_grid(large.m1,large.m2, nrow = 1, align = "v", scale = 1)


##########################################################################
### S I M U L A T I O N S:      D  I S T R I B U T I O N   P L O T S  I I 
##########################################################################



##########################
##### BY Competition and Income
##########################


## ---- plot:four:quadrants:d ----
# [plot:four:quadrants]
set.seed(602); options(scipen=999)

N = 250

# simulation matched data
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(Zelig)


high.poor.lowcomp.m = data.frame(competition = rep("Low Competition", N), income = rep("Poor Individuals", N), x = sim(model.m.s, x = setx(model.m.s, cond = TRUE,large = max(m.data$large), wealth= quantile(m.data$wealth, .25), munopp = min(m.data$munopp)), num=N)$get_qi("ev"))
high.poor.highcomp.m = data.frame(competition = rep("High Competition", N),income = rep("Poor Individuals", N), x = sim(model.m.s, x = setx(model.m.s, cond = TRUE,large = max(m.data$large), wealth= quantile(m.data$wealth, .25), munopp = max(m.data$munopp)), num=N)$get_qi("ev"))
high.rich.lowcomp.m = data.frame(competition = rep("Low Competition", N),income = rep("Non-Poor Individuals", N), x= sim(model.m.s, x = setx(model.m.s, cond = TRUE,large = max(m.data$large), wealth= quantile(m.data$wealth, .75), munopp = min(m.data$munopp)), num=N)$get_qi("ev"))
high.rich.highcomp.m = data.frame(competition = rep("High Competition", N),income = rep("Non-Poor Individuals", N), x= sim(model.m.s, x = setx(model.m.s, cond = TRUE,large = max(m.data$large), wealth= quantile(m.data$wealth, .75), munopp = max(m.data$munopp)), num=N)$get_qi("ev"))
low.poor.lowcomp.m = data.frame(competition = rep("Low Competition", N),income = rep("Poor Individuals", N), x= sim(model.m.s, x = setx(model.m.s, cond = TRUE,large = min(m.data$large), wealth= quantile(m.data$wealth, .25), munopp = min(m.data$munopp)), num=N)$get_qi("ev"))
low.poor.highcomp.m = data.frame(competition = rep("High Competition", N),income = rep("Poor Individuals", N), x= sim(model.m.s, x = setx(model.m.s, cond = TRUE,large = min(m.data$large), wealth= quantile(m.data$wealth, .25), munopp = max(m.data$munopp)), num=N)$get_qi("ev"))
low.rich.lowcomp.m = data.frame(competition = rep("Low Competition", N),income = rep("Non-Poor Individuals", N), x= sim(model.m.s, x = setx(model.m.s, cond = TRUE,large = min(m.data$large), wealth= quantile(m.data$wealth, .75), munopp = min(m.data$munopp)), num=N)$get_qi("ev"))
low.rich.highcomp.m = data.frame(competition = rep("High Competition", N),income = rep("Non-Poor Individuals", N), x= sim(model.m.s, x = setx(model.m.s, cond = TRUE,large = min(m.data$large), wealth= quantile(m.data$wealth, .75), munopp = max(m.data$munopp)), num=N)$get_qi("ev"))



# simulation raw/GPS data
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(Zelig)

high.poor.lowcomp.gps = data.frame(competition = rep("Low Competition", N), income = rep("Poor Individuals", N), x = sim(model.gps.s, x = setx(model.gps.s, cond = TRUE, wagehalf.4 = quantile(dat$wagehalf.4, .75), wealth= quantile(dat$wealth, .25), munopp = min(dat$munopp)), num=N)$get_qi("ev"))
high.poor.highcomp.gps = data.frame(competition = rep("High Competition", N),income = rep("Poor Individuals", N), x = sim(model.gps.s, x = setx(model.gps.s, cond = TRUE, wagehalf.4 = quantile(dat$wagehalf.4, .75), wealth= quantile(dat$wealth, .25), munopp = max(dat$munopp)), num=N)$get_qi("ev"))
high.rich.lowcomp.gps = data.frame(competition = rep("Low Competition", N),income = rep("Non-Poor Individuals", N), x= sim(model.gps.s, x = setx(model.gps.s, cond = TRUE, wagehalf.4 = quantile(dat$wagehalf.4, .75), wealth= quantile(dat$wealth, .75), munopp = min(dat$munopp)), num=N)$get_qi("ev"))
high.rich.highcomp.gps = data.frame(competition = rep("High Competition", N),income = rep("Non-Poor Individuals", N), x= sim(model.gps.s, x = setx(model.gps.s, cond = TRUE, wagehalf.4 = quantile(dat$wagehalf.4, .75), wealth= quantile(dat$wealth, .75), munopp = max(dat$munopp)), num=N)$get_qi("ev"))
low.poor.lowcomp.gps = data.frame(competition = rep("Low Competition", N),income = rep("Poor Individuals", N), x= sim(model.gps.s, x = setx(model.gps.s, cond = TRUE, wagehalf.4 = quantile(dat$wagehalf.4, .25), wealth= quantile(dat$wealth, .25), munopp = min(dat$munopp)), num=N)$get_qi("ev"))
low.poor.highcomp.gps = data.frame(competition = rep("High Competition", N),income = rep("Poor Individuals", N), x= sim(model.gps.s, x = setx(model.gps.s, cond = TRUE, wagehalf.4 = quantile(dat$wagehalf.4, .25), wealth= quantile(dat$wealth, .25), munopp = max(dat$munopp)), num=N)$get_qi("ev"))
low.rich.lowcomp.gps = data.frame(competition = rep("Low Competition", N),income = rep("Non-Poor Individuals", N), x= sim(model.gps.s, x = setx(model.gps.s, cond = TRUE, wagehalf.4 = quantile(dat$wagehalf.4, .25), wealth= quantile(dat$wealth, .75), munopp = min(dat$munopp)), num=N)$get_qi("ev"))
low.rich.highcomp.gps = data.frame(competition = rep("High Competition", N),income = rep("Non-Poor Individuals", N), x= sim(model.gps.s, x = setx(model.gps.s, cond = TRUE, wagehalf.4 = quantile(dat$wagehalf.4, .25), wealth= quantile(dat$wealth, .75), munopp = max(dat$munopp)), num=N)$get_qi("ev"))





# data frame
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(Rmisc)

ci = .95

plot.d = data.frame(
        mean = c(
                as.numeric(CI(high.poor.lowcomp.gps$x, ci = ci)[2]),  # gps
                as.numeric(CI(high.poor.highcomp.gps$x, ci = ci)[2]),  # gps
                as.numeric(CI(high.rich.lowcomp.gps$x, ci = ci)[2]),  # gps
                as.numeric(CI(high.rich.highcomp.gps$x, ci = ci)[2]),  # gps
                as.numeric(CI(low.poor.lowcomp.gps$x, ci = ci)[2]),  # gps
                as.numeric(CI(low.poor.highcomp.gps$x, ci = ci)[2]),  # gps
                as.numeric(CI(low.rich.lowcomp.gps$x, ci = ci)[2]),  # gps
                as.numeric(CI(low.rich.highcomp.gps$x, ci = ci)[2]), # gps
                as.numeric(CI(high.poor.lowcomp.m$x, ci = ci)[2]),  # matched
                as.numeric(CI(high.poor.highcomp.m$x, ci = ci)[2]),  # matched
                as.numeric(CI(high.rich.lowcomp.m$x, ci = ci)[2]),  # matched
                as.numeric(CI(high.rich.highcomp.m$x, ci = ci)[2]),  # matched
                as.numeric(CI(low.poor.lowcomp.m$x, ci = ci)[2]),  # matched
                as.numeric(CI(low.poor.highcomp.m$x, ci = ci)[2]),  # matched
                as.numeric(CI(low.rich.lowcomp.m$x, ci = ci)[2]),  # matched
                as.numeric(CI(low.rich.highcomp.m$x, ci = ci)[2]) # matched
        ),
        upper = c(
                as.numeric(CI(high.poor.lowcomp.gps$x, ci = ci)[1]),  # gps
                as.numeric(CI(high.poor.highcomp.gps$x, ci = ci)[1]), # gps
                as.numeric(CI(high.rich.lowcomp.gps$x, ci = ci)[1]), # gps
                as.numeric(CI(high.rich.highcomp.gps$x, ci = ci)[1]), # gps
                as.numeric(CI(low.poor.lowcomp.gps$x, ci = ci)[1]), # gps
                as.numeric(CI(low.poor.highcomp.gps$x, ci = ci)[1]), # gps
                as.numeric(CI(low.rich.lowcomp.gps$x, ci = ci)[1]), # gps
                as.numeric(CI(low.rich.highcomp.gps$x, ci = ci)[1]), # gps
                as.numeric(CI(high.poor.lowcomp.m$x, ci = ci)[1]),  # matched
                as.numeric(CI(high.poor.highcomp.m$x, ci = ci)[1]), # matched
                as.numeric(CI(high.rich.lowcomp.m$x, ci = ci)[1]), # matched
                as.numeric(CI(high.rich.highcomp.m$x, ci = ci)[1]), # matched
                as.numeric(CI(low.poor.lowcomp.m$x, ci = ci)[1]), # matched
                as.numeric(CI(low.poor.highcomp.m$x, ci = ci)[1]), # matched
                as.numeric(CI(low.rich.lowcomp.m$x, ci = ci)[1]), # matched
                as.numeric(CI(low.rich.highcomp.m$x, ci = ci)[1]) # matched
        ),
        lower = c(
                as.numeric(CI(high.poor.lowcomp.gps$x, ci = ci)[3]),  # gps
                as.numeric(CI(high.poor.highcomp.gps$x, ci = ci)[3]), # gps
                as.numeric(CI(high.rich.lowcomp.gps$x, ci = ci)[3]), # gps
                as.numeric(CI(high.rich.highcomp.gps$x, ci = ci)[3]), # gps
                as.numeric(CI(low.poor.lowcomp.gps$x, ci = ci)[3]), # gps
                as.numeric(CI(low.poor.highcomp.gps$x, ci = ci)[3]), # gps
                as.numeric(CI(low.rich.lowcomp.gps$x, ci = ci)[3]), # gps
                as.numeric(CI(low.rich.highcomp.gps$x, ci = ci)[3]), # gps
                as.numeric(CI(high.poor.lowcomp.m$x, ci = ci)[3]),  # matched
                as.numeric(CI(high.poor.highcomp.m$x, ci = ci)[3]), # matched
                as.numeric(CI(high.rich.lowcomp.m$x, ci = ci)[3]), # matched
                as.numeric(CI(high.rich.highcomp.m$x, ci = ci)[3]), # matched
                as.numeric(CI(low.poor.lowcomp.m$x, ci = ci)[3]), # matched
                as.numeric(CI(low.poor.highcomp.m$x, ci = ci)[3]), # matched
                as.numeric(CI(low.rich.lowcomp.m$x, ci = ci)[3]), # matched
                as.numeric(CI(low.rich.highcomp.m$x, ci = ci)[3]) # matched
        ),
        Density = c(rep("High", 4), rep("Low", 4), rep("High", 4), rep("Low", 4)),
        Wealth = rep(c(rep("Poor Individual", 2), rep("Non-Poor Individual", 2)), 4),
        Competition = rep(c("Low Competition","High Competition"),8),
        Sample = c(rep("Weighted (GPS)", 8), rep("Matched", 8))
)

# plot
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)

plot.four.quadrants.plot = ggplot(plot.d, aes(Density, mean,
                   ymin = upper,
                   ymax=lower,
                   colour = Sample)) + 
  geom_errorbar(width=0.2) + 
  #scale_color_manual(values=c("grey70", "gray0")) +
  facet_grid(Competition~Wealth) +
        ylab("Probability of being Targeted") + xlab("Density of the Poor") +
        theme_bw() + #theme(legend.position="none") +
        theme(strip.text.x = element_text(size = 8), 
              strip.text.y = element_text(size = 8), 
              axis.title=element_text(size=10), 
              legend.text = element_text(size = 8), 
              legend.title = element_text(size = 10),
              axis.text.y = element_text(size = 8),
              legend.position="top")  #+ 
       # scale_colour_discrete(name = "Sample")
## ----


## ---- plot:four:quadrants ----
plot.four.quadrants.plot
plot.four.quadrants.plot.legend = paste(
        "{\\bf Simulated Expected Values of Clientelism}.",
        "\\\\\\hspace{\\textwidth}", 
        paste("{\\bf Note}:", paste("After fitting the models shown in \\autoref{tab:1}, this figure shows the predicted probabilities of being targeted under different scenarios, with", paste(ci*100,"\\%", sep = ""), "confidence intervals.")),"Substantively, the figure emulates the theoretical predictions of \\autoref{tab:strategy:set}. Clientelism is higher when non-poor individuals are nested in poor groups (``high'' density of the poor) in highly contested municipalities (Q1), when non-poor individuals are nested in non-poor groups (``low'' density of the poor) in scarcely contested municipalities (Q3), when poor individuals are nested in poor areas in highly contested municipalities (Q2), and when poor individuals are nested in non-poor areas in scarcely contested municipalities (Q4). For every quadrant, estimates from both the matched and weighted datasets are shown. The idea is to show that the decision of dichotomizing the density of the poor variable at its median (\\autoref{fig:tgraph:plot}) gives substantively exact results than using the continuous version of that variable via the GPS analysis.",
        "\n")
## ----








############################### OTHERS
# means highest values
## quadrant 1
mean(low.poor.lowcomp$x)
## quadrant 4
mean(high.rich.highcomp$x)

## quadrant 3
mean(high.poor.highcomp$x)
## quadrant 2
mean(low.rich.lowcomp$x)



## t test on these distributions
### 1
t.test(high.poor.lowcomp$x, low.poor.lowcomp$x,conf.level = 0.95, paired = TRUE) # significative pvalue = significantly different
### 2
t.test(high.rich.lowcomp$x, low.rich.lowcomp$x,conf.level = 0.95, paired = TRUE) # significative pvalue = significantly different
### 3
t.test(high.poor.highcomp$x, low.poor.highcomp$x,conf.level = 0.95, paired = TRUE) # significative pvalue = significantly different
### 4
t.test(high.rich.highcomp$x, low.rich.highcomp$x,conf.level = 0.95, paired = TRUE) # significative pvalue = significantly different


## high/low quadrant 1
# null hypothesis: the distributions of x and y differ by a location shift of mu
# if p value less than 1, reject the null
# alternative: x is shifted to the right of y
wilcox.test(low.poor.lowcomp$x,high.poor.lowcomp$x, paired = TRUE, alternative = "greater")

### quadrants 1 and 4
t.test(low.poor.lowcomp$x, high.rich.highcomp$x, alt="greater",conf.level = 0.95) # significative pvalue = significantly different

### quadrants 3-4
wilcox.test(high.poor.highcomp$x, high.rich.highcomp$x,paired = T, alternative = "greater") # significative pvalue = significantly different


### quadrant 2
wilcox.test(low.rich.lowcomp$x,high.rich.lowcomp$x, paired = TRUE, alternative = "greater")


### quadrant 4
wilcox.test(low.rich.lowcomp$x,high.rich.lowcomp$x, paired = TRUE, alternative = "greater")



### MAKE A TABLE IN RNW using this sequence.
t = t.test(high.rich.highcomp$x, low.rich.highcomp$x,conf.level = 0.99) # significative pvalue = significantly different
as.numeric(t$estimate[2])



######################################################
# Plot Wealthy Also Receive Clientelism
######################################################
# [wealth:client:plot]


## ---- wealth:client:plot:d ----
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/dat.RData")

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)

wealth.plot = ggplot() + geom_jitter(
  width = 4,
  height = .45, 
  alpha = 1/4,
  aes(
    y=as.factor(dat$clien1dummy), 
    x=as.numeric(dat$wealth), 
    colour=as.numeric(dat$clien1dummy))) +
  xlab("Wealth Index") + 
  ylab("Offered him/her to buy vote") + 
  theme_bw()+
  theme(strip.text.x = element_text(size = 8), 
        strip.text.y = element_text(size = 8), 
        axis.title=element_text(size=10), 
        legend.text = element_text(size = 10), 
        legend.title = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        legend.position="none")
## ---- 



## ---- wealth:client:plot ----
wealth.plot
wealth.plot.legend <- paste(
        "{\\bf Individual Wealth and Vote-Buying in Brazil}.",
        "\\\\\\hspace{\\textwidth}", 
        "{\\bf Note}: Following the advice of \\textcite{Cordova2008} and \\textcite{Cordova2009,Cordova2010}, different socio-economic variables in \\textcite{LAPOP2010} dataset were used to construct a relative wealth index. With this information, in addition to the frequency of clientelism question (\\texttt{clien1}), the figure shows that clientelist brokers target individuals at all levels of income.",
        "\n")
## ---- 




##########################
###### By Density, Income, COmpetition and Pop Size //
##########################
### PUT IN APPENDIX: SAY THAT I DIDNT FIND SUPPORT //
### ACTUALLY LARGER POPULATION< MORE CLIENTELISM // ATTENTION THIS IS **NOT** RUEDA'S argument

set.seed(602); options(scipen=999)


# simulation DISTRIBUTION PLOTS // low pop
high.poor.lowcomp.lowpop = data.frame(competition = rep("Low Competition", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE,large = max(m.data$large), pop.10 = min(m.data$pop.10) ,wealth= quantile(m.data$wealth, .25), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.poor.highcomp.lowpop = data.frame(competition = rep("High Competition", 1000000),income = rep("Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE,large = max(m.data$large), pop.10 = min(m.data$pop.10) ,wealth= quantile(m.data$wealth, .25), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.lowcomp.lowpop = data.frame(competition = rep("Low Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = max(m.data$large), pop.10 = min(m.data$pop.10) ,wealth= quantile(m.data$wealth, .75), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.highcomp.lowpop = data.frame(competition = rep("High Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = max(m.data$large), pop.10 = min(m.data$pop.10) ,wealth= quantile(m.data$wealth, .75), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.lowcomp.lowpop = data.frame(competition = rep("Low Competition", 1000000),income = rep("Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = min(m.data$large), pop.10 = min(m.data$pop.10) ,wealth= quantile(m.data$wealth, .25), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.highcomp.lowpop = data.frame(competition = rep("High Competition", 1000000),income = rep("Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = min(m.data$large), pop.10 = min(m.data$pop.10) ,wealth= quantile(m.data$wealth, .25), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.lowcomp.lowpop = data.frame(competition = rep("Low Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = min(m.data$large), pop.10 = min(m.data$pop.10) ,wealth= quantile(m.data$wealth, .75), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.highcomp.lowpop = data.frame(competition = rep("High Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = min(m.data$large), pop.10 = min(m.data$pop.10) ,wealth= quantile(m.data$wealth, .75), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))



# simulation DISTRIBUTION PLOTS // high pop
high.poor.lowcomp.highpop = data.frame(competition = rep("Low Competition", 1000000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE,large = max(m.data$large), pop.10 = max(m.data$pop.10) ,wealth= quantile(m.data$wealth, .25), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.poor.highcomp.highpop = data.frame(competition = rep("High Competition", 1000000),income = rep("Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE,large = max(m.data$large), pop.10 = max(m.data$pop.10) ,wealth= quantile(m.data$wealth, .25), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.lowcomp.highpop = data.frame(competition = rep("Low Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = max(m.data$large), pop.10 = max(m.data$pop.10) ,wealth= quantile(m.data$wealth, .75), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.highcomp.highpop = data.frame(competition = rep("High Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = max(m.data$large), pop.10 = max(m.data$pop.10) ,wealth= quantile(m.data$wealth, .75), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.lowcomp.highpop = data.frame(competition = rep("Low Competition", 1000000),income = rep("Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = min(m.data$large), pop.10 = max(m.data$pop.10) ,wealth= quantile(m.data$wealth, .25), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.highcomp.highpop = data.frame(competition = rep("High Competition", 1000000),income = rep("Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = min(m.data$large), pop.10 = max(m.data$pop.10) ,wealth= quantile(m.data$wealth, .25), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.lowcomp.highpop = data.frame(competition = rep("Low Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = min(m.data$large), pop.10 = max(m.data$pop.10) ,wealth= quantile(m.data$wealth, .75), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.highcomp.highpop = data.frame(competition = rep("High Competition", 1000000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = min(m.data$large), pop.10 = max(m.data$pop.10) ,wealth= quantile(m.data$wealth, .75), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))



# plot 1 //
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)

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
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)

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


if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(cowplot)

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
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)

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
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)

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

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(cowplot)

plot_grid(p1,p2, ncol = 1, align = "v", scale = 1)


#####################################################################
### S I M U L A T I O N S:      I N T E R A C T I O N   P L O T S 
#####################################################################




##########################
#  LARGE * WEALTH:
##########################


# METHOD 2
# library(devtools) # install.packages("devtools")
# install_github('IQSS/Zelig')
#library(Zelig) # install.packages("Zelig", dependencies=TRUE) # Models

# simulation
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(Zelig)

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
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(Rmisc)

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
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)

ggplot(wealth.d, aes(x=Wealth, y=mean, colour=Poverty)) + 
  stat_smooth() + 
  geom_ribbon(aes(ymin=Lower, ymax=Upper, linetype=NA), alpha=0.2) +
  stat_smooth(aes(x=Wealth,y=mean)) +
  xlab("Wealth Index") + ylab("Expected Value of Clientelism") + 
  theme_bw() + 
  theme(legend.position="top", legend.title=element_blank(), legend.key = element_rect())


#####################################################################
#####################################################################






##########################
#  LARGE * MUNOPP:


# simulation
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(Zelig)

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
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(Rmisc)

### df's
### low

# low
df.low = data.frame(
  mean = c(as.numeric(CI(model.m.s.low$`1`)["mean"]),as.numeric(CI(model.m.s.low$`2`)["mean"]),as.numeric(CI(model.m.s.low$`3`)["mean"]),as.numeric(CI(model.m.s.low$`4`)["mean"]),as.numeric(CI(model.m.s.low$`5`)["mean"]),as.numeric(CI(model.m.s.low$`6`)["mean"]),as.numeric(CI(model.m.s.low$`7`)["mean"]),as.numeric(CI(model.m.s.low$`8`)["mean"]),as.numeric(CI(model.m.s.low$`9`)["mean"]),as.numeric(CI(model.m.s.low$`10`)["mean"]),as.numeric(CI(model.m.s.low$`11`)["mean"]),as.numeric(CI(model.m.s.low$`12`)["mean"]),as.numeric(CI(model.m.s.low$`13`)["mean"]),as.numeric(CI(model.m.s.low$`14`)["mean"]),as.numeric(CI(model.m.s.low$`15`)["mean"]),as.numeric(CI(model.m.s.low$`16`)["mean"]),as.numeric(CI(model.m.s.low$`17`)["mean"]),as.numeric(CI(model.m.s.low$`18`)["mean"]),as.numeric(CI(model.m.s.low$`19`)["mean"]),as.numeric(CI(model.m.s.low$`20`)["mean"]),as.numeric(CI(model.m.s.low$`21`)["mean"]),as.numeric(CI(model.m.s.low$`22`)["mean"]),as.numeric(CI(model.m.s.low$`23`)["mean"]),as.numeric(CI(model.m.s.low$`24`)["mean"]),as.numeric(CI(model.m.s.low$`25`)["mean"]),as.numeric(CI(model.m.s.low$`26`)["mean"]),as.numeric(CI(model.m.s.low$`27`)["mean"]),as.numeric(CI(model.m.s.low$`28`)["mean"]),as.numeric(CI(model.m.s.low$`29`)["mean"]),as.numeric(CI(model.m.s.low$`30`)["mean"]),as.numeric(CI(model.m.s.low$`31`)["mean"]),as.numeric(CI(model.m.s.low$`32`)["mean"]),as.numeric(CI(model.m.s.low$`33`)["mean"]),as.numeric(CI(model.m.s.low$`34`)["mean"]),as.numeric(CI(model.m.s.low$`35`)["mean"]),as.numeric(CI(model.m.s.low$`36`)["mean"]),as.numeric(CI(model.m.s.low$`37`)["mean"]),as.numeric(CI(model.m.s.low$`38`)["mean"]),as.numeric(CI(model.m.s.low$`39`)["mean"]),as.numeric(CI(model.m.s.low$`40`)["mean"]),as.numeric(CI(model.m.s.low$`41`)["mean"]),as.numeric(CI(model.m.s.low$`42`)["mean"]),as.numeric(CI(model.m.s.low$`43`)["mean"]),as.numeric(CI(model.m.s.low$`44`)["mean"]),as.numeric(CI(model.m.s.low$`45`)["mean"]),as.numeric(CI(model.m.s.low$`46`)["mean"]),as.numeric(CI(model.m.s.low$`47`)["mean"]),as.numeric(CI(model.m.s.low$`48`)["mean"]),as.numeric(CI(model.m.s.low$`49`)["mean"]),as.numeric(CI(model.m.s.low$`50`)["mean"]),as.numeric(CI(model.m.s.low$`51`)["mean"]),as.numeric(CI(model.m.s.low$`52`)["mean"]),as.numeric(CI(model.m.s.low$`53`)["mean"]),as.numeric(CI(model.m.s.low$`54`)["mean"]),as.numeric(CI(model.m.s.low$`55`)["mean"]),as.numeric(CI(model.m.s.low$`56`)["mean"]),as.numeric(CI(model.m.s.low$`57`)["mean"]),as.numeric(CI(model.m.s.low$`58`)["mean"])),
  Type = rep("Low Density", ncol(model.m.s.low)),
  Opposition = min(m.data$munopp):max(m.data$munopp),
  Upper = c(as.numeric(CI(model.m.s.low$`1`)["upper"]),as.numeric(CI(model.m.s.low$`2`)["upper"]),as.numeric(CI(model.m.s.low$`3`)["upper"]),as.numeric(CI(model.m.s.low$`4`)["upper"]),as.numeric(CI(model.m.s.low$`5`)["upper"]),as.numeric(CI(model.m.s.low$`6`)["upper"]),as.numeric(CI(model.m.s.low$`7`)["upper"]),as.numeric(CI(model.m.s.low$`8`)["upper"]),as.numeric(CI(model.m.s.low$`9`)["upper"]),as.numeric(CI(model.m.s.low$`10`)["upper"]),as.numeric(CI(model.m.s.low$`11`)["upper"]),as.numeric(CI(model.m.s.low$`12`)["upper"]),as.numeric(CI(model.m.s.low$`13`)["upper"]),as.numeric(CI(model.m.s.low$`14`)["upper"]),as.numeric(CI(model.m.s.low$`15`)["upper"]),as.numeric(CI(model.m.s.low$`16`)["upper"]),as.numeric(CI(model.m.s.low$`17`)["upper"]),as.numeric(CI(model.m.s.low$`18`)["upper"]),as.numeric(CI(model.m.s.low$`19`)["upper"]),as.numeric(CI(model.m.s.low$`20`)["upper"]),as.numeric(CI(model.m.s.low$`21`)["upper"]),as.numeric(CI(model.m.s.low$`22`)["upper"]),as.numeric(CI(model.m.s.low$`23`)["upper"]),as.numeric(CI(model.m.s.low$`24`)["upper"]),as.numeric(CI(model.m.s.low$`25`)["upper"]),as.numeric(CI(model.m.s.low$`26`)["upper"]),as.numeric(CI(model.m.s.low$`27`)["upper"]),as.numeric(CI(model.m.s.low$`28`)["upper"]),as.numeric(CI(model.m.s.low$`29`)["upper"]),as.numeric(CI(model.m.s.low$`30`)["upper"]),as.numeric(CI(model.m.s.low$`31`)["upper"]),as.numeric(CI(model.m.s.low$`32`)["upper"]),as.numeric(CI(model.m.s.low$`33`)["upper"]),as.numeric(CI(model.m.s.low$`34`)["upper"]),as.numeric(CI(model.m.s.low$`35`)["upper"]),as.numeric(CI(model.m.s.low$`36`)["upper"]),as.numeric(CI(model.m.s.low$`37`)["upper"]),as.numeric(CI(model.m.s.low$`38`)["upper"]),as.numeric(CI(model.m.s.low$`39`)["upper"]),as.numeric(CI(model.m.s.low$`40`)["upper"]),as.numeric(CI(model.m.s.low$`41`)["upper"]),as.numeric(CI(model.m.s.low$`42`)["upper"]),as.numeric(CI(model.m.s.low$`43`)["upper"]),as.numeric(CI(model.m.s.low$`44`)["upper"]),as.numeric(CI(model.m.s.low$`45`)["upper"]),as.numeric(CI(model.m.s.low$`46`)["upper"]),as.numeric(CI(model.m.s.low$`47`)["upper"]),as.numeric(CI(model.m.s.low$`48`)["upper"]),as.numeric(CI(model.m.s.low$`49`)["upper"]),as.numeric(CI(model.m.s.low$`50`)["upper"]),as.numeric(CI(model.m.s.low$`51`)["upper"]),as.numeric(CI(model.m.s.low$`52`)["upper"]),as.numeric(CI(model.m.s.low$`53`)["upper"]),as.numeric(CI(model.m.s.low$`54`)["upper"]),as.numeric(CI(model.m.s.low$`55`)["upper"]),as.numeric(CI(model.m.s.low$`56`)["upper"]),as.numeric(CI(model.m.s.low$`57`)["upper"]),as.numeric(CI(model.m.s.low$`58`)["upper"])), 
  Lower = c(as.numeric(CI(model.m.s.low$`1`)["lower"]),as.numeric(CI(model.m.s.low$`2`)["lower"]),as.numeric(CI(model.m.s.low$`3`)["lower"]),as.numeric(CI(model.m.s.low$`4`)["lower"]),as.numeric(CI(model.m.s.low$`5`)["lower"]),as.numeric(CI(model.m.s.low$`6`)["lower"]),as.numeric(CI(model.m.s.low$`7`)["lower"]),as.numeric(CI(model.m.s.low$`8`)["lower"]),as.numeric(CI(model.m.s.low$`9`)["lower"]),as.numeric(CI(model.m.s.low$`10`)["lower"]),as.numeric(CI(model.m.s.low$`11`)["lower"]),as.numeric(CI(model.m.s.low$`12`)["lower"]),as.numeric(CI(model.m.s.low$`13`)["lower"]),as.numeric(CI(model.m.s.low$`14`)["lower"]),as.numeric(CI(model.m.s.low$`15`)["lower"]),as.numeric(CI(model.m.s.low$`16`)["lower"]),as.numeric(CI(model.m.s.low$`17`)["lower"]),as.numeric(CI(model.m.s.low$`18`)["lower"]),as.numeric(CI(model.m.s.low$`19`)["lower"]),as.numeric(CI(model.m.s.low$`20`)["lower"]),as.numeric(CI(model.m.s.low$`21`)["lower"]),as.numeric(CI(model.m.s.low$`22`)["lower"]),as.numeric(CI(model.m.s.low$`23`)["lower"]),as.numeric(CI(model.m.s.low$`24`)["lower"]),as.numeric(CI(model.m.s.low$`25`)["lower"]),as.numeric(CI(model.m.s.low$`26`)["lower"]),as.numeric(CI(model.m.s.low$`27`)["lower"]),as.numeric(CI(model.m.s.low$`28`)["lower"]),as.numeric(CI(model.m.s.low$`29`)["lower"]),as.numeric(CI(model.m.s.low$`30`)["lower"]),as.numeric(CI(model.m.s.low$`31`)["lower"]),as.numeric(CI(model.m.s.low$`32`)["lower"]),as.numeric(CI(model.m.s.low$`33`)["lower"]),as.numeric(CI(model.m.s.low$`34`)["lower"]),as.numeric(CI(model.m.s.low$`35`)["lower"]),as.numeric(CI(model.m.s.low$`36`)["lower"]),as.numeric(CI(model.m.s.low$`37`)["lower"]),as.numeric(CI(model.m.s.low$`38`)["lower"]),as.numeric(CI(model.m.s.low$`39`)["lower"]),as.numeric(CI(model.m.s.low$`40`)["lower"]),as.numeric(CI(model.m.s.low$`41`)["lower"]),as.numeric(CI(model.m.s.low$`42`)["lower"]),as.numeric(CI(model.m.s.low$`43`)["lower"]),as.numeric(CI(model.m.s.low$`44`)["lower"]),as.numeric(CI(model.m.s.low$`45`)["lower"]),as.numeric(CI(model.m.s.low$`46`)["lower"]),as.numeric(CI(model.m.s.low$`47`)["lower"]),as.numeric(CI(model.m.s.low$`48`)["lower"]),as.numeric(CI(model.m.s.low$`49`)["lower"]),as.numeric(CI(model.m.s.low$`50`)["lower"]),as.numeric(CI(model.m.s.low$`51`)["lower"]),as.numeric(CI(model.m.s.low$`52`)["lower"]),as.numeric(CI(model.m.s.low$`53`)["lower"]),as.numeric(CI(model.m.s.low$`54`)["lower"]),as.numeric(CI(model.m.s.low$`55`)["lower"]),as.numeric(CI(model.m.s.low$`56`)["lower"]),as.numeric(CI(model.m.s.low$`57`)["lower"]),as.numeric(CI(model.m.s.low$`58`)["lower"])))


# high
df.high = data.frame(
  mean = c(as.numeric(CI(model.m.s.high$`1`)["mean"]),as.numeric(CI(model.m.s.high$`2`)["mean"]),as.numeric(CI(model.m.s.high$`3`)["mean"]),as.numeric(CI(model.m.s.high$`4`)["mean"]),as.numeric(CI(model.m.s.high$`5`)["mean"]),as.numeric(CI(model.m.s.high$`6`)["mean"]),as.numeric(CI(model.m.s.high$`7`)["mean"]),as.numeric(CI(model.m.s.high$`8`)["mean"]),as.numeric(CI(model.m.s.high$`9`)["mean"]),as.numeric(CI(model.m.s.high$`10`)["mean"]),as.numeric(CI(model.m.s.high$`11`)["mean"]),as.numeric(CI(model.m.s.high$`12`)["mean"]),as.numeric(CI(model.m.s.high$`13`)["mean"]),as.numeric(CI(model.m.s.high$`14`)["mean"]),as.numeric(CI(model.m.s.high$`15`)["mean"]),as.numeric(CI(model.m.s.high$`16`)["mean"]),as.numeric(CI(model.m.s.high$`17`)["mean"]),as.numeric(CI(model.m.s.high$`18`)["mean"]),as.numeric(CI(model.m.s.high$`19`)["mean"]),as.numeric(CI(model.m.s.high$`20`)["mean"]),as.numeric(CI(model.m.s.high$`21`)["mean"]),as.numeric(CI(model.m.s.high$`22`)["mean"]),as.numeric(CI(model.m.s.high$`23`)["mean"]),as.numeric(CI(model.m.s.high$`24`)["mean"]),as.numeric(CI(model.m.s.high$`25`)["mean"]),as.numeric(CI(model.m.s.high$`26`)["mean"]),as.numeric(CI(model.m.s.high$`27`)["mean"]),as.numeric(CI(model.m.s.high$`28`)["mean"]),as.numeric(CI(model.m.s.high$`29`)["mean"]),as.numeric(CI(model.m.s.high$`30`)["mean"]),as.numeric(CI(model.m.s.high$`31`)["mean"]),as.numeric(CI(model.m.s.high$`32`)["mean"]),as.numeric(CI(model.m.s.high$`33`)["mean"]),as.numeric(CI(model.m.s.high$`34`)["mean"]),as.numeric(CI(model.m.s.high$`35`)["mean"]),as.numeric(CI(model.m.s.high$`36`)["mean"]),as.numeric(CI(model.m.s.high$`37`)["mean"]),as.numeric(CI(model.m.s.high$`38`)["mean"]),as.numeric(CI(model.m.s.high$`39`)["mean"]),as.numeric(CI(model.m.s.high$`40`)["mean"]),as.numeric(CI(model.m.s.high$`41`)["mean"]),as.numeric(CI(model.m.s.high$`42`)["mean"]),as.numeric(CI(model.m.s.high$`43`)["mean"]),as.numeric(CI(model.m.s.high$`44`)["mean"]),as.numeric(CI(model.m.s.high$`45`)["mean"]),as.numeric(CI(model.m.s.high$`46`)["mean"]),as.numeric(CI(model.m.s.high$`47`)["mean"]),as.numeric(CI(model.m.s.high$`48`)["mean"]),as.numeric(CI(model.m.s.high$`49`)["mean"]),as.numeric(CI(model.m.s.high$`50`)["mean"]),as.numeric(CI(model.m.s.high$`51`)["mean"]),as.numeric(CI(model.m.s.high$`52`)["mean"]),as.numeric(CI(model.m.s.high$`53`)["mean"]),as.numeric(CI(model.m.s.high$`54`)["mean"]),as.numeric(CI(model.m.s.high$`55`)["mean"]),as.numeric(CI(model.m.s.high$`56`)["mean"]),as.numeric(CI(model.m.s.high$`57`)["mean"]),as.numeric(CI(model.m.s.high$`58`)["mean"])),
  Type = rep("High Density", ncol(model.m.s.high)),
  Opposition = min(m.data$munopp):max(m.data$munopp),
  Upper = c(as.numeric(CI(model.m.s.high$`1`)["upper"]),as.numeric(CI(model.m.s.high$`2`)["upper"]),as.numeric(CI(model.m.s.high$`3`)["upper"]),as.numeric(CI(model.m.s.high$`4`)["upper"]),as.numeric(CI(model.m.s.high$`5`)["upper"]),as.numeric(CI(model.m.s.high$`6`)["upper"]),as.numeric(CI(model.m.s.high$`7`)["upper"]),as.numeric(CI(model.m.s.high$`8`)["upper"]),as.numeric(CI(model.m.s.high$`9`)["upper"]),as.numeric(CI(model.m.s.high$`10`)["upper"]),as.numeric(CI(model.m.s.high$`11`)["upper"]),as.numeric(CI(model.m.s.high$`12`)["upper"]),as.numeric(CI(model.m.s.high$`13`)["upper"]),as.numeric(CI(model.m.s.high$`14`)["upper"]),as.numeric(CI(model.m.s.high$`15`)["upper"]),as.numeric(CI(model.m.s.high$`16`)["upper"]),as.numeric(CI(model.m.s.high$`17`)["upper"]),as.numeric(CI(model.m.s.high$`18`)["upper"]),as.numeric(CI(model.m.s.high$`19`)["upper"]),as.numeric(CI(model.m.s.high$`20`)["upper"]),as.numeric(CI(model.m.s.high$`21`)["upper"]),as.numeric(CI(model.m.s.high$`22`)["upper"]),as.numeric(CI(model.m.s.high$`23`)["upper"]),as.numeric(CI(model.m.s.high$`24`)["upper"]),as.numeric(CI(model.m.s.high$`25`)["upper"]),as.numeric(CI(model.m.s.high$`26`)["upper"]),as.numeric(CI(model.m.s.high$`27`)["upper"]),as.numeric(CI(model.m.s.high$`28`)["upper"]),as.numeric(CI(model.m.s.high$`29`)["upper"]),as.numeric(CI(model.m.s.high$`30`)["upper"]),as.numeric(CI(model.m.s.high$`31`)["upper"]),as.numeric(CI(model.m.s.high$`32`)["upper"]),as.numeric(CI(model.m.s.high$`33`)["upper"]),as.numeric(CI(model.m.s.high$`34`)["upper"]),as.numeric(CI(model.m.s.high$`35`)["upper"]),as.numeric(CI(model.m.s.high$`36`)["upper"]),as.numeric(CI(model.m.s.high$`37`)["upper"]),as.numeric(CI(model.m.s.high$`38`)["upper"]),as.numeric(CI(model.m.s.high$`39`)["upper"]),as.numeric(CI(model.m.s.high$`40`)["upper"]),as.numeric(CI(model.m.s.high$`41`)["upper"]),as.numeric(CI(model.m.s.high$`42`)["upper"]),as.numeric(CI(model.m.s.high$`43`)["upper"]),as.numeric(CI(model.m.s.high$`44`)["upper"]),as.numeric(CI(model.m.s.high$`45`)["upper"]),as.numeric(CI(model.m.s.high$`46`)["upper"]),as.numeric(CI(model.m.s.high$`47`)["upper"]),as.numeric(CI(model.m.s.high$`48`)["upper"]),as.numeric(CI(model.m.s.high$`49`)["upper"]),as.numeric(CI(model.m.s.high$`50`)["upper"]),as.numeric(CI(model.m.s.high$`51`)["upper"]),as.numeric(CI(model.m.s.high$`52`)["upper"]),as.numeric(CI(model.m.s.high$`53`)["upper"]),as.numeric(CI(model.m.s.high$`54`)["upper"]),as.numeric(CI(model.m.s.high$`55`)["upper"]),as.numeric(CI(model.m.s.high$`56`)["upper"]),as.numeric(CI(model.m.s.high$`57`)["upper"]),as.numeric(CI(model.m.s.high$`58`)["upper"])), 
  Lower = c(as.numeric(CI(model.m.s.high$`1`)["lower"]),as.numeric(CI(model.m.s.high$`2`)["lower"]),as.numeric(CI(model.m.s.high$`3`)["lower"]),as.numeric(CI(model.m.s.high$`4`)["lower"]),as.numeric(CI(model.m.s.high$`5`)["lower"]),as.numeric(CI(model.m.s.high$`6`)["lower"]),as.numeric(CI(model.m.s.high$`7`)["lower"]),as.numeric(CI(model.m.s.high$`8`)["lower"]),as.numeric(CI(model.m.s.high$`9`)["lower"]),as.numeric(CI(model.m.s.high$`10`)["lower"]),as.numeric(CI(model.m.s.high$`11`)["lower"]),as.numeric(CI(model.m.s.high$`12`)["lower"]),as.numeric(CI(model.m.s.high$`13`)["lower"]),as.numeric(CI(model.m.s.high$`14`)["lower"]),as.numeric(CI(model.m.s.high$`15`)["lower"]),as.numeric(CI(model.m.s.high$`16`)["lower"]),as.numeric(CI(model.m.s.high$`17`)["lower"]),as.numeric(CI(model.m.s.high$`18`)["lower"]),as.numeric(CI(model.m.s.high$`19`)["lower"]),as.numeric(CI(model.m.s.high$`20`)["lower"]),as.numeric(CI(model.m.s.high$`21`)["lower"]),as.numeric(CI(model.m.s.high$`22`)["lower"]),as.numeric(CI(model.m.s.high$`23`)["lower"]),as.numeric(CI(model.m.s.high$`24`)["lower"]),as.numeric(CI(model.m.s.high$`25`)["lower"]),as.numeric(CI(model.m.s.high$`26`)["lower"]),as.numeric(CI(model.m.s.high$`27`)["lower"]),as.numeric(CI(model.m.s.high$`28`)["lower"]),as.numeric(CI(model.m.s.high$`29`)["lower"]),as.numeric(CI(model.m.s.high$`30`)["lower"]),as.numeric(CI(model.m.s.high$`31`)["lower"]),as.numeric(CI(model.m.s.high$`32`)["lower"]),as.numeric(CI(model.m.s.high$`33`)["lower"]),as.numeric(CI(model.m.s.high$`34`)["lower"]),as.numeric(CI(model.m.s.high$`35`)["lower"]),as.numeric(CI(model.m.s.high$`36`)["lower"]),as.numeric(CI(model.m.s.high$`37`)["lower"]),as.numeric(CI(model.m.s.high$`38`)["lower"]),as.numeric(CI(model.m.s.high$`39`)["lower"]),as.numeric(CI(model.m.s.high$`40`)["lower"]),as.numeric(CI(model.m.s.high$`41`)["lower"]),as.numeric(CI(model.m.s.high$`42`)["lower"]),as.numeric(CI(model.m.s.high$`43`)["lower"]),as.numeric(CI(model.m.s.high$`44`)["lower"]),as.numeric(CI(model.m.s.high$`45`)["lower"]),as.numeric(CI(model.m.s.high$`46`)["lower"]),as.numeric(CI(model.m.s.high$`47`)["lower"]),as.numeric(CI(model.m.s.high$`48`)["lower"]),as.numeric(CI(model.m.s.high$`49`)["lower"]),as.numeric(CI(model.m.s.high$`50`)["lower"]),as.numeric(CI(model.m.s.high$`51`)["lower"]),as.numeric(CI(model.m.s.high$`52`)["lower"]),as.numeric(CI(model.m.s.high$`53`)["lower"]),as.numeric(CI(model.m.s.high$`54`)["lower"]),as.numeric(CI(model.m.s.high$`55`)["lower"]),as.numeric(CI(model.m.s.high$`56`)["lower"]),as.numeric(CI(model.m.s.high$`57`)["lower"]),as.numeric(CI(model.m.s.high$`58`)["lower"])))



### munopp
df.munopp = data.frame(
  mean = c(as.numeric(CI(model.m.s.munopp$`1`)["mean"]),as.numeric(CI(model.m.s.munopp$`2`)["mean"]),as.numeric(CI(model.m.s.munopp$`3`)["mean"]),as.numeric(CI(model.m.s.munopp$`4`)["mean"]),as.numeric(CI(model.m.s.munopp$`5`)["mean"]),as.numeric(CI(model.m.s.munopp$`6`)["mean"]),as.numeric(CI(model.m.s.munopp$`7`)["mean"]),as.numeric(CI(model.m.s.munopp$`8`)["mean"]),as.numeric(CI(model.m.s.munopp$`9`)["mean"]),as.numeric(CI(model.m.s.munopp$`10`)["mean"]),as.numeric(CI(model.m.s.munopp$`11`)["mean"]),as.numeric(CI(model.m.s.munopp$`12`)["mean"]),as.numeric(CI(model.m.s.munopp$`13`)["mean"]),as.numeric(CI(model.m.s.munopp$`14`)["mean"]),as.numeric(CI(model.m.s.munopp$`15`)["mean"]),as.numeric(CI(model.m.s.munopp$`16`)["mean"]),as.numeric(CI(model.m.s.munopp$`17`)["mean"]),as.numeric(CI(model.m.s.munopp$`18`)["mean"]),as.numeric(CI(model.m.s.munopp$`19`)["mean"]),as.numeric(CI(model.m.s.munopp$`20`)["mean"]),as.numeric(CI(model.m.s.munopp$`21`)["mean"]),as.numeric(CI(model.m.s.munopp$`22`)["mean"]),as.numeric(CI(model.m.s.munopp$`23`)["mean"]),as.numeric(CI(model.m.s.munopp$`24`)["mean"]),as.numeric(CI(model.m.s.munopp$`25`)["mean"]),as.numeric(CI(model.m.s.munopp$`26`)["mean"]),as.numeric(CI(model.m.s.munopp$`27`)["mean"]),as.numeric(CI(model.m.s.munopp$`28`)["mean"]),as.numeric(CI(model.m.s.munopp$`29`)["mean"]),as.numeric(CI(model.m.s.munopp$`30`)["mean"]),as.numeric(CI(model.m.s.munopp$`31`)["mean"]),as.numeric(CI(model.m.s.munopp$`32`)["mean"]),as.numeric(CI(model.m.s.munopp$`33`)["mean"]),as.numeric(CI(model.m.s.munopp$`34`)["mean"]),as.numeric(CI(model.m.s.munopp$`35`)["mean"]),as.numeric(CI(model.m.s.munopp$`36`)["mean"]),as.numeric(CI(model.m.s.munopp$`37`)["mean"]),as.numeric(CI(model.m.s.munopp$`38`)["mean"]),as.numeric(CI(model.m.s.munopp$`39`)["mean"]),as.numeric(CI(model.m.s.munopp$`40`)["mean"]),as.numeric(CI(model.m.s.munopp$`41`)["mean"]),as.numeric(CI(model.m.s.munopp$`42`)["mean"]),as.numeric(CI(model.m.s.munopp$`43`)["mean"]),as.numeric(CI(model.m.s.munopp$`44`)["mean"]),as.numeric(CI(model.m.s.munopp$`45`)["mean"]),as.numeric(CI(model.m.s.munopp$`46`)["mean"]),as.numeric(CI(model.m.s.munopp$`47`)["mean"]),as.numeric(CI(model.m.s.munopp$`48`)["mean"]),as.numeric(CI(model.m.s.munopp$`49`)["mean"]),as.numeric(CI(model.m.s.munopp$`50`)["mean"]),as.numeric(CI(model.m.s.munopp$`51`)["mean"]),as.numeric(CI(model.m.s.munopp$`52`)["mean"]),as.numeric(CI(model.m.s.munopp$`53`)["mean"]),as.numeric(CI(model.m.s.munopp$`54`)["mean"]),as.numeric(CI(model.m.s.munopp$`55`)["mean"]),as.numeric(CI(model.m.s.munopp$`56`)["mean"]),as.numeric(CI(model.m.s.munopp$`57`)["mean"]),as.numeric(CI(model.m.s.munopp$`58`)["mean"])),
  Type = rep("Municipal Opposition", ncol(model.m.s.munopp)),
  Opposition = min(m.data$munopp):max(m.data$munopp),
  Upper = c(as.numeric(CI(model.m.s.munopp$`1`)["upper"]),as.numeric(CI(model.m.s.munopp$`2`)["upper"]),as.numeric(CI(model.m.s.munopp$`3`)["upper"]),as.numeric(CI(model.m.s.munopp$`4`)["upper"]),as.numeric(CI(model.m.s.munopp$`5`)["upper"]),as.numeric(CI(model.m.s.munopp$`6`)["upper"]),as.numeric(CI(model.m.s.munopp$`7`)["upper"]),as.numeric(CI(model.m.s.munopp$`8`)["upper"]),as.numeric(CI(model.m.s.munopp$`9`)["upper"]),as.numeric(CI(model.m.s.munopp$`10`)["upper"]),as.numeric(CI(model.m.s.munopp$`11`)["upper"]),as.numeric(CI(model.m.s.munopp$`12`)["upper"]),as.numeric(CI(model.m.s.munopp$`13`)["upper"]),as.numeric(CI(model.m.s.munopp$`14`)["upper"]),as.numeric(CI(model.m.s.munopp$`15`)["upper"]),as.numeric(CI(model.m.s.munopp$`16`)["upper"]),as.numeric(CI(model.m.s.munopp$`17`)["upper"]),as.numeric(CI(model.m.s.munopp$`18`)["upper"]),as.numeric(CI(model.m.s.munopp$`19`)["upper"]),as.numeric(CI(model.m.s.munopp$`20`)["upper"]),as.numeric(CI(model.m.s.munopp$`21`)["upper"]),as.numeric(CI(model.m.s.munopp$`22`)["upper"]),as.numeric(CI(model.m.s.munopp$`23`)["upper"]),as.numeric(CI(model.m.s.munopp$`24`)["upper"]),as.numeric(CI(model.m.s.munopp$`25`)["upper"]),as.numeric(CI(model.m.s.munopp$`26`)["upper"]),as.numeric(CI(model.m.s.munopp$`27`)["upper"]),as.numeric(CI(model.m.s.munopp$`28`)["upper"]),as.numeric(CI(model.m.s.munopp$`29`)["upper"]),as.numeric(CI(model.m.s.munopp$`30`)["upper"]),as.numeric(CI(model.m.s.munopp$`31`)["upper"]),as.numeric(CI(model.m.s.munopp$`32`)["upper"]),as.numeric(CI(model.m.s.munopp$`33`)["upper"]),as.numeric(CI(model.m.s.munopp$`34`)["upper"]),as.numeric(CI(model.m.s.munopp$`35`)["upper"]),as.numeric(CI(model.m.s.munopp$`36`)["upper"]),as.numeric(CI(model.m.s.munopp$`37`)["upper"]),as.numeric(CI(model.m.s.munopp$`38`)["upper"]),as.numeric(CI(model.m.s.munopp$`39`)["upper"]),as.numeric(CI(model.m.s.munopp$`40`)["upper"]),as.numeric(CI(model.m.s.munopp$`41`)["upper"]),as.numeric(CI(model.m.s.munopp$`42`)["upper"]),as.numeric(CI(model.m.s.munopp$`43`)["upper"]),as.numeric(CI(model.m.s.munopp$`44`)["upper"]),as.numeric(CI(model.m.s.munopp$`45`)["upper"]),as.numeric(CI(model.m.s.munopp$`46`)["upper"]),as.numeric(CI(model.m.s.munopp$`47`)["upper"]),as.numeric(CI(model.m.s.munopp$`48`)["upper"]),as.numeric(CI(model.m.s.munopp$`49`)["upper"]),as.numeric(CI(model.m.s.munopp$`50`)["upper"]),as.numeric(CI(model.m.s.munopp$`51`)["upper"]),as.numeric(CI(model.m.s.munopp$`52`)["upper"]),as.numeric(CI(model.m.s.munopp$`53`)["upper"]),as.numeric(CI(model.m.s.munopp$`54`)["upper"]),as.numeric(CI(model.m.s.munopp$`55`)["upper"]),as.numeric(CI(model.m.s.munopp$`56`)["upper"]),as.numeric(CI(model.m.s.munopp$`57`)["upper"]),as.numeric(CI(model.m.s.munopp$`58`)["upper"])), 
  Lower = c(as.numeric(CI(model.m.s.munopp$`1`)["lower"]),as.numeric(CI(model.m.s.munopp$`2`)["lower"]),as.numeric(CI(model.m.s.munopp$`3`)["lower"]),as.numeric(CI(model.m.s.munopp$`4`)["lower"]),as.numeric(CI(model.m.s.munopp$`5`)["lower"]),as.numeric(CI(model.m.s.munopp$`6`)["lower"]),as.numeric(CI(model.m.s.munopp$`7`)["lower"]),as.numeric(CI(model.m.s.munopp$`8`)["lower"]),as.numeric(CI(model.m.s.munopp$`9`)["lower"]),as.numeric(CI(model.m.s.munopp$`10`)["lower"]),as.numeric(CI(model.m.s.munopp$`11`)["lower"]),as.numeric(CI(model.m.s.munopp$`12`)["lower"]),as.numeric(CI(model.m.s.munopp$`13`)["lower"]),as.numeric(CI(model.m.s.munopp$`14`)["lower"]),as.numeric(CI(model.m.s.munopp$`15`)["lower"]),as.numeric(CI(model.m.s.munopp$`16`)["lower"]),as.numeric(CI(model.m.s.munopp$`17`)["lower"]),as.numeric(CI(model.m.s.munopp$`18`)["lower"]),as.numeric(CI(model.m.s.munopp$`19`)["lower"]),as.numeric(CI(model.m.s.munopp$`20`)["lower"]),as.numeric(CI(model.m.s.munopp$`21`)["lower"]),as.numeric(CI(model.m.s.munopp$`22`)["lower"]),as.numeric(CI(model.m.s.munopp$`23`)["lower"]),as.numeric(CI(model.m.s.munopp$`24`)["lower"]),as.numeric(CI(model.m.s.munopp$`25`)["lower"]),as.numeric(CI(model.m.s.munopp$`26`)["lower"]),as.numeric(CI(model.m.s.munopp$`27`)["lower"]),as.numeric(CI(model.m.s.munopp$`28`)["lower"]),as.numeric(CI(model.m.s.munopp$`29`)["lower"]),as.numeric(CI(model.m.s.munopp$`30`)["lower"]),as.numeric(CI(model.m.s.munopp$`31`)["lower"]),as.numeric(CI(model.m.s.munopp$`32`)["lower"]),as.numeric(CI(model.m.s.munopp$`33`)["lower"]),as.numeric(CI(model.m.s.munopp$`34`)["lower"]),as.numeric(CI(model.m.s.munopp$`35`)["lower"]),as.numeric(CI(model.m.s.munopp$`36`)["lower"]),as.numeric(CI(model.m.s.munopp$`37`)["lower"]),as.numeric(CI(model.m.s.munopp$`38`)["lower"]),as.numeric(CI(model.m.s.munopp$`39`)["lower"]),as.numeric(CI(model.m.s.munopp$`40`)["lower"]),as.numeric(CI(model.m.s.munopp$`41`)["lower"]),as.numeric(CI(model.m.s.munopp$`42`)["lower"]),as.numeric(CI(model.m.s.munopp$`43`)["lower"]),as.numeric(CI(model.m.s.munopp$`44`)["lower"]),as.numeric(CI(model.m.s.munopp$`45`)["lower"]),as.numeric(CI(model.m.s.munopp$`46`)["lower"]),as.numeric(CI(model.m.s.munopp$`47`)["lower"]),as.numeric(CI(model.m.s.munopp$`48`)["lower"]),as.numeric(CI(model.m.s.munopp$`49`)["lower"]),as.numeric(CI(model.m.s.munopp$`50`)["lower"]),as.numeric(CI(model.m.s.munopp$`51`)["lower"]),as.numeric(CI(model.m.s.munopp$`52`)["lower"]),as.numeric(CI(model.m.s.munopp$`53`)["lower"]),as.numeric(CI(model.m.s.munopp$`54`)["lower"]),as.numeric(CI(model.m.s.munopp$`55`)["lower"]),as.numeric(CI(model.m.s.munopp$`56`)["lower"]),as.numeric(CI(model.m.s.munopp$`57`)["lower"]),as.numeric(CI(model.m.s.munopp$`58`)["lower"])))



### combined two df's
munopp.d= rbind(df.high, df.low,df.munopp)


### plot
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)

ggplot(munopp.d, aes(x=Opposition, y=mean, colour=Type)) + 
  stat_smooth() + 
  geom_ribbon(aes(ymin=Lower, ymax=Upper, linetype=NA), alpha=0.2) +
  stat_smooth(aes(x=Opposition,y=mean)) +
  xlab("Municipal Opposition") + ylab("Expected Value of Clientelism") + 
  theme_bw() + 
  theme(legend.position="top", legend.title=element_blank(), legend.key = element_rect())


##########################
#  LARGE * POLINV:  // LARGE * POP


## ---- pol.inv:pop.size:plot:d ----
# [pol.inv:pop.size:plot]
# simulation
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(Zelig)

set.seed(602); options(scipen=999)


# low 
model.m.s.low = data.frame(
        zelig_qi_to_df(sim(model.m.s,
    x = setx(model.m.s, cond = TRUE,
             large = min(m.data$large), 
             polinv:large,
             polinv = min(m.data$polinv):max(m.data$polinv)), 
    num=700)))


model.m.s.low <- model.m.s.low[, c("polinv", "expected_value")]

df.low = data.frame(
        mean = data.frame(aggregate(cbind(expected_value)~polinv, data=model.m.s.low, FUN=CI)[,2])[,2], # mean
        Upper = data.frame(aggregate(cbind(expected_value)~polinv, data=model.m.s.low, FUN=CI)[,2])[,1], # upper
        Lower = data.frame(aggregate(cbind(expected_value)~polinv, data=model.m.s.low, FUN=CI)[,2])[,3], # lower
        Type = rep("Low Density", max(m.data$polinv)+1),
        Opposition = min(m.data$polinv):max(m.data$polinv)
        )
        



# high
model.m.s.high = data.frame(
        zelig_qi_to_df(sim(model.m.s,
    x = setx(model.m.s, cond = TRUE,
             large = max(m.data$large), 
             polinv:large,
             polinv = min(m.data$polinv):max(m.data$polinv)), 
    num=700))) ; 

model.m.s.high <- model.m.s.high[, c("polinv", "expected_value")]


df.high = data.frame(
        mean = data.frame(aggregate(cbind(expected_value)~polinv, data=model.m.s.high, FUN=CI)[,2])[,2], # mean
        Upper = data.frame(aggregate(cbind(expected_value)~polinv, data=model.m.s.high, FUN=CI)[,2])[,1], # upper
        Lower = data.frame(aggregate(cbind(expected_value)~polinv, data=model.m.s.high, FUN=CI)[,2])[,3], # lower
        Type = rep("High Density", max(m.data$polinv)+1),
        Opposition = min(m.data$polinv):max(m.data$polinv)
        )


# polinv 
model.m.s.polinv = data.frame(
        zelig_qi_to_df(sim(model.m.s, x = setx(model.m.s, cond = TRUE,
               large:munopp,
               polinv = min(m.data$polinv):max(m.data$polinv)), 
      num=300)))



model.m.s.polinv <- model.m.s.polinv[, c("polinv", "expected_value")]


model.m.s.polinv = data.frame(
        mean = data.frame(aggregate(cbind(expected_value)~polinv, data=model.m.s.polinv, FUN=CI)[,2])[,2], # mean
        Upper = data.frame(aggregate(cbind(expected_value)~polinv, data=model.m.s.polinv, FUN=CI)[,2])[,1], # upper
        Lower = data.frame(aggregate(cbind(expected_value)~polinv, data=model.m.s.polinv, FUN=CI)[,2])[,3], # lower
        Type = rep("Political Involvement", max(m.data$polinv)+1),
        Opposition = min(m.data$polinv):max(m.data$polinv)
)




### combined 3 df's
polinv.d= rbind(df.high, df.low,model.m.s.polinv)


### plot
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)

p1= ggplot(polinv.d, aes(x=Opposition, y=mean, colour=Type)) + 
  stat_smooth() + 
  geom_ribbon(aes(ymin=Lower, ymax=Upper, linetype=NA), alpha=0.2) +
  scale_color_manual(values=c("gray0", "grey70", "gray60")) +
  stat_smooth(aes(x=Opposition,y=mean)) +
  xlab("Political Involvement") + ylab("Expected Value of Clientelism") + 
  theme_bw() + 
  theme(legend.position="top", legend.title=element_blank(), legend.key = element_rect())

##########################
#  LARGE * POP
##########################

## low 
gee.dich.m.2.low = data.frame(
        zelig_qi_to_df(sim(model.m.s,x = setx(model.m.s, cond = TRUE,
                                              large = min(m.data$large), 
                                              pop.10 = min(m.data$pop.10):max(m.data$pop.10)), 
                           num=300)))

gee.dich.m.2.low <- gee.dich.m.2.low[, c("pop.10", "expected_value")]


gee.dich.m.2.low = data.frame(
        mean = data.frame(aggregate(cbind(expected_value)~pop.10, data=gee.dich.m.2.low, FUN=CI)[,2])[,2], # mean
        Upper = data.frame(aggregate(cbind(expected_value)~pop.10, data=gee.dich.m.2.low, FUN=CI)[,2])[,1], # upper
        Lower = data.frame(aggregate(cbind(expected_value)~pop.10, data=gee.dich.m.2.low, FUN=CI)[,2])[,3], # lower
        Poverty = rep("Low Density", max(m.data$pop.10)),
        Population = min(m.data$pop.10):max(m.data$pop.10)
        )




## high
gee.dich.m.2.high = data.frame(
        zelig_qi_to_df(sim(model.m.s, x = setx(model.m.s, cond = TRUE,
               large = max(m.data$large), 
               pop.10 = min(m.data$pop.10):max(m.data$pop.10)),num=300)))


gee.dich.m.2.high <- gee.dich.m.2.high[, c("pop.10", "expected_value")]


gee.dich.m.2.high = data.frame(
        mean = data.frame(aggregate(cbind(expected_value)~pop.10, data=gee.dich.m.2.high, FUN=CI)[,2])[,2], # mean
        Upper = data.frame(aggregate(cbind(expected_value)~pop.10, data=gee.dich.m.2.high, FUN=CI)[,2])[,1], # upper
        Lower = data.frame(aggregate(cbind(expected_value)~pop.10, data=gee.dich.m.2.high, FUN=CI)[,2])[,3], # lower
        Poverty = rep("High Density", max(m.data$pop.10)),
        Population = min(m.data$pop.10):max(m.data$pop.10)
        )


## pop.10
df.pop.alone = data.frame(
        zelig_qi_to_df(sim(model.m.s,x = setx(model.m.s, cond = TRUE,
                                              pop.10 = min(m.data$pop.10):max(m.data$pop.10)),num=300)))



df.pop.alone <- df.pop.alone[, c("pop.10", "expected_value")]

df.pop.alone = data.frame(
        mean = data.frame(aggregate(cbind(expected_value)~pop.10, data=df.pop.alone, FUN=CI)[,2])[,2], # mean
        Upper = data.frame(aggregate(cbind(expected_value)~pop.10, data=df.pop.alone, FUN=CI)[,2])[,1], # upper
        Lower = data.frame(aggregate(cbind(expected_value)~pop.10, data=df.pop.alone, FUN=CI)[,2])[,3], # lower
        Poverty = rep("Population Size", max(m.data$pop.10)),
        Population = min(m.data$pop.10):max(m.data$pop.10)
)


### combined 3 df's
pop.d= rbind(gee.dich.m.2.low, gee.dich.m.2.high,df.pop.alone)



### plot
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)

p2= ggplot(pop.d, aes(x=Population, y=mean, colour=Poverty)) + 
  stat_smooth() + 
  geom_ribbon(aes(ymin=Lower, ymax=Upper, linetype=NA), alpha=0.2) +
  #scale_color_manual(values=c("gray0", "grey70", "gray60")) +
  stat_smooth(aes(x=Population,y=mean)) +
  xlab("Municipal Population Size") + ylab("Probability of being Targeted") + 
  theme_bw() + 
  theme(axis.title.y=element_text(colour="white"), legend.position="top", legend.title=element_blank(), legend.key = element_rect())



if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(cowplot)

pol.inv.pop.size.plot = plot_grid(p1,p2,  nrow = 1, labels = "auto")
## ----



## ---- pol.inv:pop.size:plot ----
pol.inv.pop.size.plot
pol.inv.pop.size.plot.legend <- paste(
        "{\\bf Simulated Expected Probability of being Targeted: Political Involvement and Population Size}.",
        "\\\\\\hspace{\\textwidth}", 
        "{\\bf Note}: Using the estimations in \\autoref{tab:1}, the figure shows the probability of being targeted at different values of political involvement (a) and population size at the municipal level (b). The figure suggests that being nested in high-poor density areas contributes substantially more to explaining clientelism.",
        "\n")
## ---- 


## ----






######################################################
#  D  E S C R I P T I V E          P   L   O   T   S #
######################################################



########################################################
# Descriptive Stats Matched Set





## ---- tab:sum:stats:m:data ----
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")


if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(car)

## [tab:sum:stats:m:data]

# clien1dummy ~ wealth*munopp*large + pop.10 + urban + polinv + ing4 + vb3 + exc7 + ed

# matched sample
m.data.clien1dummy <- m.data$clien1dummy 
m.data.wealth <- m.data$wealth 
m.data.munopp <- m.data$munopp
m.data.large <- m.data$large 
m.data.pop.10 <- m.data$pop.10
m.data.urban <- as.numeric(recode(as.numeric(m.data$urban), "1 = 0 ; 2 = 1"))
m.data.polinv <- m.data$polinv 
m.data.ing4 <- m.data$ing4
m.data.vb3 <- m.data$vb3 
m.data.exc7 <- m.data$exc7 
m.data.ed <- m.data$ed

# df
dat.m <- data.frame(m.data.clien1dummy, m.data.wealth, m.data.munopp, m.data.large, m.data.pop.10, m.data.urban, m.data.polinv, m.data.ing4, m.data.vb3, m.data.exc7, m.data.ed)

# complete cases fx
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

dat.m = completeFun(dat.m)



labels.m = c("Clientelism",  "Wealth Index", "Municipal Opposition",  "High Density of the Poor", "Municipal Population", "Urban", "Political Involvement Index" ,  "Support for Democracy",  "Party Id.", "Perception of Corruption",  "Years of Education")
## ---- 






## ---- tab:sum:stats:m:table ----
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(stargazer)

#library(stargazer, quietly = T) # install.packages("stargazer")
stargazer(dat.m, 
          summary=T, 
          title = "Summary Statistics: Matched Sample.",
          label = "sumtab:matched",
          type = "latex", # 'text' for word // 'latex' for latex.
          font.size = "scriptsize",
          style= "apsr",
          covariate.labels=labels.m,
          table.placement = "h",
          notes.align = "c"#,
          #out = "/Users/hectorbahamonde/Desktop/matched_sum_stats.txt"
)
## ----







########################################################
# Descriptive Stats Raw Set






## ---- tab:sum:stats:r:data ----
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/dat.RData")

# [tab:sum:stats:r]

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(car)

# whole sample
r.data.clien1dummy <- as.numeric(recode(as.numeric(dat$clien1dummy), "1 = 0 ; 2 = 1"))
r.data.wealth <- dat$wealth 
r.data.munopp <- dat$munopp
r.data.wagehalf.4 <- dat$wagehalf.4 
r.data.pop.10 <- dat$pop.10
r.data.urban <- as.numeric(recode(as.numeric(dat$urban), "1 = 0 ; 2 = 1"))
r.data.polinv <- dat$polinv 
r.data.ing4 <- dat$ing4
r.data.vb3 <- dat$vb3 
r.data.exc7 <- dat$exc7 
r.data.ed <- dat$ed

# df
dat.r <- data.frame(r.data.clien1dummy, r.data.wealth, r.data.munopp, r.data.wagehalf.4, r.data.pop.10, r.data.urban, r.data.polinv, r.data.ing4, r.data.vb3, r.data.exc7, r.data.ed)


# complete cases fx
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

dat.r = completeFun(dat.r)


labels.r = c("Clientelism",  "Wealth Index", "Municipal Opposition",  "Density of the Poor", "Municipal Population", "Urban", "Political Involvement Index" ,  "Support for Democracy",  "Party Id.", "Perception of Corruption",  "Years of Education")
## ---- 







## ---- tab:sum:stats:r:table ----
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(stargazer)

#library(stargazer, quietly = T)
stargazer(dat.r, 
          summary=T, 
          title = "Summary Statistics: Raw Sample.",
          label = "sumtab:raw",
          type = "latex", # 'text' for word // 'latex' for latex.
          font.size = "scriptsize",
          style= "apsr",
          covariate.labels=labels.r,
          table.placement = "h",
          notes.align = "c"#,
          #out = "/Users/hectorbahamonde/Desktop/raw_sum_stats.txt"
          
)
## ----





############################################################
# Distribution of Individuals by Municipality 

## ---- municipality:sample:plot ----

# [municipality:sample:plot]

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

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)

#mun.p1 = 
  ggplot(municipality.d, aes(x = Municipality, y = Freq, fill = Sample)) + 
    geom_bar(stat = "identity", position=position_dodge()) + 
    #scale_fill_manual(values= c("gray32", "#999999")) + 
    xlab("") + 
    ylab("Frequency") + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          legend.key = element_rect(colour = NA, fill = NA, size = 0.5))
  
## ----

  
  
  
  
############################################################
# Distribution of Individuals by High/Low COnditions and municipality [municipality:income:large:plot]
# [municipality:income:large:plot:matched]
  
## ---- municipality:income:large:plot:matched:data ----
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")

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

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)

# The palette with grey:


municipality.income.large.plot.matched.plot = 
  ggplot(density.d, aes(factor(Municipality), fill = Density)) + 
    geom_bar() + 
    #scale_fill_manual(values= c("gray32", "#999999")) + 
    geom_point(data=density.d, 
                   position = position_jitter(width = 0.22, height = 5), 
                   size = I(1),
                   aes(
                           x=as.factor(Municipality), 
                           y=Wealth*10,
                           alpha=Wealth)) + 
    #coord_flip() +
    xlab("") + 
    ylab("Frequency") + 
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.key = element_rect(colour = NA, fill = NA, size = 0.5))
## ----


## ---- municipality:income:large:plot:matched:plot ----
municipality.income.large.plot.matched.plot 
municipality.income.large.plot.matched.plot.legend  <- paste(paste("{\\bf Distribution of Observations by Municipality, Wealth Index and Density of the Poor}."),
                                                             "\\\\\\hspace{\\textwidth}", 
                                                             paste("{\\bf Note}: The figure shows the municipalities in the analyses (matched set). For every municipality, the figure shows (1) the number of inhabitants (Y-axis), (2) whether the municipality is considered having a high or low density of the poor. High-density municipalities have more than half of their inhabitants living on less than half of the minimum wage. The figure also shows (3) individual wealth indexes." ), 
                                                                                "\n")
## ----


######
## Combine mun.p1, mun.p2
#library(cowplot) # install.packages("cowplot")
#plot_grid(mun.p1,mun.p2,  nrow = 2)


############################################################


## ---- municipality:wealth:large:plot ----


# Distribution of Individuals by High/Low COnditions and Wealth [municipality:wealth:large:plot]

load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/dat.RData")

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)


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

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(cowplot)

plot_grid(density.wealth.m,density.wealth.r,  nrow = 2)

## ----





############################################################
# Distribution Outcome Variable Binary Outcome
m.data$clien1dummy <- factor(m.data$clien1dummy, labels = c("No", "Yes"))

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)

ggplot(data=m.data, aes(x=clien1dummy)) + 
  geom_bar(width=.5, stat="count",size=1, alpha=.7) + 
  xlab("Clientelism") + 
  ylab("Frequency") + 
  theme_bw()


## Distribution Outcome Variable 3 outcomes
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)

ggplot(data=m.data, aes(x=clientelism)) + 
  geom_bar(width=.5, stat="count",size=1, alpha=.7) + 
  xlab("Clientelism") + 
  ylab("Frequency") + 
  theme_bw()


############################################################
## Distribution treatment var
# [tgraph:plot]


## ---- tgraph:plot:d ----
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")


dens <- density(m.data$wagehalf)
df <- data.frame(x=dens$x, y=dens$y)
df$quant <- factor(findInterval(df$x, median(m.data$wagehalf)))


if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)
tgraph.plot = ggplot(df, aes(x,y)) +
        geom_line() + 
        geom_ribbon(aes(ymin=0, ymax=y, fill=quant, alpha = 0.5)) + 
        xlab("Percentage of People Living on\nLess than Half of the Minimum Wage") + 
        ylab("Density") + 
        scale_fill_brewer(palette="Greens") +
        theme_bw() +
        theme(
                legend.position="none", 
                axis.title.y = element_text(size = 10),
                axis.title.x = element_text(size = 10)) +
        geom_segment(aes(
                x = (wagehalf=median(m.data$wagehalf)), 
                y = 0, 
                xend = (wagehalf=median(m.data$wagehalf)), 
                yend = df$y[df$x == max(df$x[df$x<=median(m.data$wagehalf)])]), 
                linetype="dotted", 
                size=.5, 
                colour = "red")
## ----

## ---- tgraph:plot ----
tgraph.plot
tgraph.plot.legend <- paste(paste("{\\bf Distribution of the Density of the Poor}."),
                                  "\\\\\\hspace{\\textwidth}", 
                            paste("{\\bf Note}: Employing Brazilian census data from the \\href{http://www.ibge.gov.br}{IBGE} (2010), the figure shows  the percentage of individuals who live on less than half of the minimum wage in a given municipality. While individual income is measured using the relative wealth index (in \\autoref{fig:wealth:client:plot}), the variable plotted here is used to measure economic development at the group level. Due to statistical reasons explained in the paper, the variable had to be dichotomized at its median", 
                                  paste("(",round(as.numeric(median(m.data$wagehalf)), 2),"\\%",").", sep = ""), "However, in separate statistical analyses shown in \\autoref{tab:1} (weighted model), the variable is used without dichotomizing it, showing the same results.", sep = " "), 
                            "\n")
## ---- 


# THis here is the same graph but with bars
## Plot BARS
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)

ggplot(m.data, aes(x=wagehalf)) + 
        geom_histogram(binwidth=2.5, alpha=.7) + 
        #geom_density(alpha=.1) +
        theme_bw() +
        geom_segment(data= m.data, aes(x = (wagehalf=median(m.data$wagehalf)), y = 0, xend = (wagehalf=median(m.data$wagehalf)), yend = 100), linetype="dashed", size=1.5, colour = "forestgreen") + 
        xlab("Density of the Poor") + ylab("Frequency") +
        geom_text(data = ggplot.labels1, aes(x = time, y = value, label = label), colour = "forestgreen")



######################################################
#  B   A   L   A   N   C   E       P   L   O   T   S #
######################################################

# load data
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/mdata.RData")
load("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/dat.RData")


# Density Plot: Propensity Scores, by Treatment Condition and By DIscarded 
Distance = as.vector(m.out$distance)

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(car)
Sample = recode(as.numeric(as.vector(m.out$discarded)), "0 = 'Matched' ; 1 = 'Raw' ")
Density = recode(as.numeric(as.vector(m.out$treat)), "0 = 'Low' ; 1 = 'High' ")


# [balance:distance:plot]
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)

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




## ---- balance:plot ----
# [balance:plot]
plot(m.out, type="hist")
## ----







## Matched
##
# [balance:plot2]

library(ggplot2)
library(gtable)
library(gridExtra)
library(cowplot)

# matched

balance.1.m=ggplot() + 
  geom_density(aes(x=as.numeric(m.data$wealth), colour =factor(m.data$large,levels = c(0,1),labels = c("Low", "High")))) + 
  ylab("Matched Set") + 
  xlab("Wealth Index") + 
  theme_bw() +
  scale_x_continuous(limits = c(min(dat$wealth), max(dat$wealth))) +
  theme(axis.title=element_text(size=10), legend.text = element_text(size = 10), legend.title = element_text(size = 10))  + scale_colour_discrete(name = "Density of the Poor")

#balance.2.m=ggplot() + 
  #geom_density(aes(x=as.numeric(m.data$pop.10), colour = factor(m.data$large,levels = c(0,1),labels = c("Low", "High")))) + 
  #ylab("") + 
  #xlab("Municipal Population") + 
  #theme_bw() +
  #scale_x_continuous(limits = c(min(dat$pop.10), max(dat$pop.10))) +
  #theme(axis.title=element_text(size=10), legend.text = element_text(size = 10), legend.title = element_text(size = 10))  + scale_colour_discrete(name = "Density of the Poor")

balance.3.m=ggplot() + 
  geom_density(aes(x=as.numeric(m.data$munopp), colour = factor(m.data$large,levels = c(0,1),labels = c("Low", "High")))) + 
  ylab("") + 
  xlab("Municipal Opposition") + 
  theme_bw() +
  scale_x_continuous(limits = c(min(dat$munopp), max(dat$munopp))) +
  theme(axis.title=element_text(size=10), legend.text = element_text(size = 10), legend.title = element_text(size = 10))  + scale_colour_discrete(name = "Density of the Poor")

balance.4.m=ggplot() + 
  geom_density(aes(x=as.numeric(m.data$polinv), colour =factor(m.data$large,levels = c(0,1),labels = c("Low", "High")))) + 
  ylab("") + 
  xlab("Political Involvement") + 
  theme_bw() + 
  scale_x_continuous(limits = c(min(dat$polinv), max(dat$polinv))) +
  theme(axis.title=element_text(size=10), legend.text = element_text(size = 10), legend.title = element_text(size = 10))  + scale_colour_discrete(name = "Density of the Poor")

# RAW

balance.1.r=ggplot() + 
  geom_density(aes(x=as.numeric(dat$wealth), colour =factor(dat$large,levels = c(0,1),labels = c("Low", "High")))) + 
  ylab("Raw Set") + 
  xlab("Wealth Index") + 
  theme_bw() +
  theme(axis.title=element_text(size=10), legend.text = element_text(size = 10), legend.title = element_text(size = 10))  + scale_colour_discrete(name = "Density of the Poor")

#balance.2.r=ggplot() + 
 # geom_density(aes(x=as.numeric(dat$pop.10), colour = factor(dat$large,levels = c(0,1),labels = c("Low", "High")))) + 
  #ylab("") + 
  #xlab("Municipal Population") + 
  #theme_bw() +
  #scale_x_continuous(limits = c(min(dat$pop.10), max(dat$pop.10))) +
  #theme(axis.title=element_text(size=10), legend.text = element_text(size = 10), legend.title = element_text(size = 10))  + scale_colour_discrete(name = "Density of the Poor")

balance.3.r=ggplot() + 
  geom_density(aes(x=as.numeric(dat$munopp), colour =factor(dat$large,levels = c(0,1),labels = c("Low", "High")))) + 
  ylab("") + 
  xlab("Municipal Opposition") + 
  theme_bw() +
  scale_x_continuous(limits = c(min(dat$munopp), max(dat$munopp))) +
  theme(axis.title=element_text(size=10), legend.text = element_text(size = 10), legend.title = element_text(size = 10))  + scale_colour_discrete(name = "Density of the Poor")

balance.4.r=ggplot() + 
  geom_density(aes(x=as.numeric(dat$polinv), colour =factor(dat$large,levels = c(0,1),labels = c("Low", "High")))) + 
  ylab("") + 
  xlab("Political Involvement") + 
  theme_bw() +
  scale_x_continuous(limits = c(min(dat$polinv), max(dat$polinv))) +
  theme(axis.title=element_text(size=10), legend.text = element_text(size = 10), legend.title = element_text(size = 10))  + scale_colour_discrete(name = "Density of the Poor")


plots <- plot_grid(
  balance.1.m + theme(legend.position="none"), 
  #balance.2.m + theme(legend.position="none"), 
  balance.3.m + theme(legend.position="none"), 
  balance.4.m + theme(legend.position="none"), 
  balance.1.r + theme(legend.position="none"), 
  #balance.2.r + theme(legend.position="none"), 
  balance.3.r + theme(legend.position="none"), 
  balance.4.r + theme(legend.position="none"),
  align = 'vh', 
  hjust = -1, 
  nrow = 2,
  ncol = 3
)

grobs <- ggplotGrob(balance.4.r + theme(legend.position="bottom"))$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]
plot_grid(plots, legend, ncol = 1, rel_heights = c(1, .2))




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
imb.m <- imbalance(
  group = m.data$large, 
  data = m.data[c("wealth", "polinv", "munopp")], 
  weights = m.data$wt)

imb.r <- imbalance(group = dat$large, data = dat[c("wealth", "polinv", "munopp")], weights = dat$wt)

imb.m.d=data.frame(
  L = round(as.numeric(imb.m$tab[,3]),3),
  Diff = round(as.numeric(imb.m$tab[,1]),3),
  Sample = rep("Matched",3),
  Variable = c(c("Wealth Index", 
                 "Political Involvement", 
                 "Municipal Opposition"))
)
  

imb.r.d=data.frame(
  L = round(as.numeric(imb.r$tab[,3]),3),
  Diff = round(as.numeric(imb.r$tab[,1]),3),
  Sample = rep("Raw",3),
  Variable = c(c("Wealth Index", 
                 "Political Involvement", 
                 "Municipal Opposition"))
  
)

data.frame(cbind(imb.m.d, imb.r.d))





