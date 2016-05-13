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

# splitting df in two
dat.ur <- subset(dat, dat$urban == "Ur")
dat.ru <- subset(dat, dat$urban == "Ru")


# computing wealth index in UR settings
dat.ur$wealth = 
        (princomp(dat.ur$wealth1)[[1]]*(dat.ur$wealth1-mean(dat.ur$wealth1)))/sd(dat.ur$wealth1) +
        (princomp(dat.ur$wealth2)[[1]]*(dat.ur$wealth2-mean(dat.ur$wealth2)))/sd(dat.ur$wealth2) +
        (princomp(dat.ur$wealth3)[[1]]*(dat.ur$wealth3-mean(dat.ur$wealth3)))/sd(dat.ur$wealth3) +
        (princomp(dat.ur$wealth4)[[1]]*(dat.ur$wealth4-mean(dat.ur$wealth4)))/sd(dat.ur$wealth4) +
        (princomp(dat.ur$wealth5)[[1]]*(dat.ur$wealth5-mean(dat.ur$wealth5)))/sd(dat.ur$wealth5) +
        (princomp(dat.ur$wealth6)[[1]]*(dat.ur$wealth6-mean(dat.ur$wealth6)))/sd(dat.ur$wealth6) +
        (princomp(dat.ur$wealth7)[[1]]*(dat.ur$wealth7-mean(dat.ur$wealth7)))/sd(dat.ur$wealth7) +
        (princomp(dat.ur$wealth8)[[1]]*(dat.ur$wealth8-mean(dat.ur$wealth8)))/sd(dat.ur$wealth8) +
        (princomp(dat.ur$wealth9)[[1]]*(dat.ur$wealth9-mean(dat.ur$wealth9)))/sd(dat.ur$wealth9) +
        (princomp(dat.ur$wealth10)[[1]]*(dat.ur$wealth10-mean(dat.ur$wealth10)))/sd(dat.ur$wealth10)


# computing wealth index in RU settings
dat.ru$wealth = 
        (princomp(dat.ru$wealth1)[[1]]*(dat.ru$wealth1-mean(dat.ru$wealth1)))/sd(dat.ru$wealth1) +
        (princomp(dat.ru$wealth2)[[1]]*(dat.ru$wealth2-mean(dat.ru$wealth2)))/sd(dat.ru$wealth2) +
        (princomp(dat.ru$wealth3)[[1]]*(dat.ru$wealth3-mean(dat.ru$wealth3)))/sd(dat.ru$wealth3) +
        (princomp(dat.ru$wealth4)[[1]]*(dat.ru$wealth4-mean(dat.ru$wealth4)))/sd(dat.ru$wealth4) +
        (princomp(dat.ru$wealth5)[[1]]*(dat.ru$wealth5-mean(dat.ru$wealth5)))/sd(dat.ru$wealth5) +
        (princomp(dat.ru$wealth6)[[1]]*(dat.ru$wealth6-mean(dat.ru$wealth6)))/sd(dat.ru$wealth6) +
        (princomp(dat.ru$wealth7)[[1]]*(dat.ru$wealth7-mean(dat.ru$wealth7)))/sd(dat.ru$wealth7) +
        (princomp(dat.ru$wealth8)[[1]]*(dat.ru$wealth8-mean(dat.ru$wealth8)))/sd(dat.ru$wealth8) +
        (princomp(dat.ru$wealth9)[[1]]*(dat.ru$wealth9-mean(dat.ru$wealth9)))/sd(dat.ru$wealth9) +
        (princomp(dat.ru$wealth10)[[1]]*(dat.ru$wealth10-mean(dat.ru$wealth10)))/sd(dat.ru$wealth10)

# combining the two DF's
dat = rbind(dat.ur, dat.ru)


# Merging Municipal Population Data
mun.pop = read.csv("/Users/hectorbahamonde/RU/research/Clientelism_paper/datasets/pop_mun.csv")


dat$municipality = as.character(dat$municipality)


library(plyr)
dat = revalue(dat$municipality, # local dataset, corrected neame
              c("Acopiara"="Acopiara",
                "Aloândia" = "Aloândia",
                "Aparecida de Goiânia"="Aparecida de Goiânia",
                "Belo Horizonte" = "Belo Horizonte",
                "Bel\x8em" = "Belém"
                ))





dat.muni = levels(dat$municipality)



dat = merge(dat, mun.pop, by="municipality")





# Constructing Matched Set
library(MatchIt) # install.packages("MatchIt", dependencies=TRUE)
#m.out <- matchit(large ~ income + ed + log(pop) + polinv, 
m.out <- matchit(large ~ income + ed, 
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

######################################################
#  D  E S C R I P T I V E          P   L   O   T   S #
######################################################

# Subset Data for Descriptive Stats Table

# matched sample
m.dat.clien1dummy <- m.data$clien1dummy 
m.dat.clien1dummy <- dat$clien1dummy 
m.dat.large <- dat$large 
m.dat.polinv <- dat$polinv 
m.dat.exc7 <- dat$exc7 
m.dat.vb3 <- dat$vb3 
m.dat.ed <- dat$ed 
m.dat.income <- dat$income 
m.dat.munopp <- dat$munopp
m.dat.pop = dat$pop
m.dat.logpop = log(m.dat.pop)
m.dat.urban <- dat$urban
m.dat.urban <- as.numeric(m.dat.urban)
m.dat.urban <- recode(m.dat.urban, "1 = 0 ; 2 = 1")
m.dat.polinv <-  dat$polinv
m.dat.ed <- dat$ed
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
stargazer(dat, 
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

# Plot

m.data$clien1dummy <- factor(m.data$clien1dummy, labels = c("No", "Yes"))

library(ggplot2)
ggplot(data=m.data, aes(x=clien1dummy)) + 
        geom_bar(width=.5, stat="count",size=1, alpha=.7) + 
        xlab("Clientelism") + 
        ylab("Frequency") + 
        theme_bw()

m.data <- match.data(m.out)
m.data$clien1dummy <- as.numeric(m.data$clien1dummy)
m.data$clien1dummy <- recode(m.data$clien1dummy, "1 = 0 ; 2 = 1")

## Distribution Outcome Variable 3 outcomes

# Plot
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

## Plot
library(ggplot2)
ggplot(m.data, aes(x=wagehalf)) + 
        geom_histogram(binwidth=2.5, alpha=.7) + 
        #geom_density(alpha=.1) +
        theme_bw() +
        geom_segment(data= m.data, aes(x = (wagehalf=median(m.data$wagehalf)), y = 0, xend = (wagehalf=median(m.data$wagehalf)), yend = 100), linetype="dashed", size=2, colour = "forestgreen") + 
        xlab("Density of the Poor") + ylab("Frequency") +
        geom_text(data = ggplot.labels1, aes(x = time, y = value, label = label), colour = "forestgreen")

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

######################################################
#              P  a r a m e t r i c                  #
######################################################

# Recode client1dummy after matching
m.data <- match.data(m.out)
m.data$clien1dummy <- as.numeric(m.data$clien1dummy)
m.data$clien1dummy <- recode(m.data$clien1dummy, "1 = 0 ; 2 = 1")

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
#install_github('IQSS/Zelig')
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
                      family = binomial, 
                      id = municipality, 
                      corstr = "exchangeable",
                      data = m.data)

logit.gee.2 <- geeglm(clien1dummy ~ large + exc7 + polinv + urban + logpop + ed,
                     family = binomial, 
                     id = municipality, 
                     corstr = "exchangeable",
                     data = m.data)

logit.gee.3 <- geeglm(clien1dummy ~ income + exc7 + polinv + urban + logpop + ed ,
                     family = binomial, 
                     id = municipality, 
                     corstr = "exchangeable",
                     data = m.data)

logit.gee.4 <- geeglm(clien1dummy ~ large + income + exc7 + polinv + urban + logpop + ed + munopp + vb3 , 
                     family = binomial, 
                     id = municipality, 
                     corstr = "exchangeable",
                     data = m.data)

logit.gee.5 <- geeglm(clien1dummy ~ large + income + exc7 + polinv + urban + logpop + ed + munopp + vb3 + logpop:polinv ,
                     family = binomial, 
                     id = municipality, 
                     corstr = "exchangeable",
                     data = m.data)

logit.gee.6 <- geeglm(clien1dummy ~ large + income + exc7 + polinv + urban + logpop + ed + munopp + vb3 + munopp:income , 
                     family = binomial, 
                     id = municipality, 
                     corstr = "exchangeable",
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



logit.gee.1.d = extract.geepack(logit.gee.1)
logit.gee.2.d = extract.geepack(logit.gee.2)
logit.gee.3.d = extract.geepack(logit.gee.3)
logit.gee.4.d = extract.geepack(logit.gee.4)
logit.gee.5.d = extract.geepack(logit.gee.5)
logit.gee.6.d = extract.geepack(logit.gee.6)




library(texreg)
screenreg(
        list(logit.gee.1.d, logit.gee.2.d, logit.gee.3.d, logit.gee.4.d, logit.gee.5.d, logit.gee.6.d),
        custom.coef.names = c(# this gotta be before OMIT.COEFF
                "(Intercept)",
                "High Density",
                "Perception of Corruption",
                "Political Involvement",
                "Urban",
                "Population (ln)",
                "Education",
                "Individual Income",
                "Municipal Opposition",
                "Political Id",
                "Political Involvement TIMES Population (ln)",
                "Individual Income TIMES Municipal Opposition"),
        caption = "Models using the Generalized Propensity Score as a weighting device - Unmatched Sample ",
        label = "tab:1",
        #override.se = logit.robust.se,
        #override.pvalues = logit.pval,
        #override.ci.low = logit.robust.se.upper,
        #override.ci.up = logit.robust.se.lower,
        stars = c(0.01, 0.05, 0.1),
        digits = 3,
        custom.note = "%stars. \n Robust Standard Errors in All Models. \n Raw sample. \n 95% Confidence Intervals in brackets.",
        fontsize = "scriptsize",
        float.pos = "h"
)


## Temporary Method to extract RELOGIT objects from Zelig.
#extract.Zeligrelogit <- function(model, include.aic = TRUE, include.bic = TRUE, 
#                                 include.loglik = TRUE, include.deviance = TRUE, include.nobs = TRUE, ...) {
#        g <- model$zelig.out$z.out[[1]]
#       class(g) <- "glm"
#       e <- extract(g, include.aic = include.aic, include.bic = include.bic, 
#                    include.loglik = include.loglik, include.deviance = include.deviance, 
#                    include.nobs = include.nobs, ...)
#       return(e)
#}

#setMethod("extract", signature = className("Zelig-relogit", "Zelig"), 
#         definition = extract.Zeligrelogit)




##########################
# Simulation and Plot 1
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


##########################
# Simulation and Plot 2
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

##########################
#   ROBUSTNESS CHECKS    #
##########################

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


##########################
#   GPS  Weighting MODELS
##########################

library(foreign)
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


# Generating the Propensity Score 
library(CBPS, quietly = T) # install.packages("CBPS")


fit <- CBPS(wagehalf ~ income, data = dat, iterations = 2500)
# Attaching weights to DF
dat$weights = round(fit$weights, digits=10)



# Recode Before modeling
dat$clien1dummy <- as.numeric(dat$clien1dummy)
library(car)
dat$clien1dummy <- recode(dat$clien1dummy, "1 = 0 ; 2 = 1")
dat$logpop = log(dat$pop)


# Models
gps.1 <- glm(clien1dummy ~ wagehalf + weights,
             family =binomial(link = "logit"),
             data = dat)

gps.2 <- glm(clien1dummy ~ wagehalf + exc7 + polinv + urban + logpop + ed+ weights,
             family =binomial(link = "logit"),
             data = dat)

gps.3 <- glm(clien1dummy ~ income + exc7 + polinv + urban + logpop + ed + weights,
             family =binomial(link = "logit"),
             data = dat)

gps.4 <- glm(clien1dummy ~ wagehalf + income + exc7 + polinv + urban + logpop + ed + munopp + vb3 + weights, 
             family =binomial(link = "logit"),
             data = dat)

gps.5 <- glm(clien1dummy ~ wagehalf + income + exc7 + polinv + urban + logpop + ed + munopp + vb3 + logpop:polinv + weights,
             family =binomial(link = "logit"),
             data = dat)

gps.6 <- glm(clien1dummy ~ wagehalf + income + exc7 + polinv + urban + logpop + ed + munopp + vb3 + munopp:income + weights, 
             family =binomial(link = "logit"),
             data = dat)

# clusterSEs BOOSTRAPPED/clustered
# library(clusterSEs) # install.packages("clusterSEs")
# 
# set.seed(602)
# gps.1.boost.robust = cluster.bs.glm(gps.1, dat, ~ municipality, ci.level = 0.95, boot.reps = 10000, stratify = T, cluster.se = TRUE, report = TRUE, prog.bar = TRUE)
# gps.2.boost.robust = cluster.bs.glm(gps.2, dat, ~ municipality, ci.level = 0.95, boot.reps = 10000, stratify = T, cluster.se = TRUE, report = TRUE, prog.bar = TRUE)
# gps.3.boost.robust = cluster.bs.glm(gps.3, dat, ~ municipality, ci.level = 0.95, boot.reps = 10000, stratify = T, cluster.se = TRUE, report = TRUE, prog.bar = TRUE)
# gps.4.boost.robust = cluster.bs.glm(gps.4, dat, ~ municipality, ci.level = 0.95, boot.reps = 10000, stratify = T, cluster.se = TRUE, report = TRUE, prog.bar = TRUE)
# gps.5.boost.robust = cluster.bs.glm(gps.5, dat, ~ municipality, ci.level = 0.95, boot.reps = 10000, stratify = T, cluster.se = TRUE, report = TRUE, prog.bar = TRUE)
# gps.6.boost.robust = cluster.bs.glm(gps.6, dat, ~ municipality, ci.level = 0.95, boot.reps = 10000, stratify = T, cluster.se = TRUE, report = TRUE, prog.bar = TRUE)


# gps.robust.se.lower.boost= list(c(gps.1.boost.robust$ci[,1]), 
#                           c(gps.2.boost.robust$ci[,1]), 
#                         c(gps.3.boost.robust$ci[,1]),
#                         c(gps.4.boost.robust$ci[,1]),
#                         c(gps.5.boost.robust$ci[,1]),
#                         c(gps.6.boost.robust$ci[,1])
#                         )
# gps.robust.se.upper.boost= list(c(gps.1.boost.robust$ci[,2]), 
#                         c(gps.2.boost.robust$ci[,2]), 
#                         c(gps.3.boost.robust$ci[,2]),
#                         c(gps.4.boost.robust$ci[,2]),
#                         c(gps.5.boost.robust$ci[,2]),
#                         c(gps.6.boost.robust$ci[,2])
#                         )


# screenreg(
#       list(gps.1, gps.2, gps.3, gps.4, gps.5, gps.6),
#       omit.coef = "weights", # this gotta be AFTER custo.coef.names
#       caption = "Models using the Generalized Propensity Score as a weighting device - Unmatched Sample ",
#       label = "results.gps:1",
#       override.se = gps.robust.se,
#       override.ci.low = gps.robust.se.upper.boost,
#       override.ci.up = gps.robust.se.lower.boost,
#       digits = 3,
#       #override.pvalues = gps.pval,
#       custom.note = "Robust Standard Errors in All Models. \n Raw sample. \n 95% Confidence Intervals in brackets.",
#       fontsize = "scriptsize",
#       float.pos = "h"
# )





# CLUSTERED STD ERRORS (but IDK what's the cluster)
# gps.1.d = data.frame(cbind(Estimate= coef(gps.1), "Robust SE" = sqrt(diag(vcovHC(gps.1, type="HC0"))),
#       "Pr(>|z|)" = 2 * pnorm(abs(coef(gps.1)/sqrt(diag(vcovHC(gps.1, type="HC0")))), lower.tail=FALSE),
# LL = coef(gps.1) - 1.96 * sqrt(diag(vcovHC(gps.1, type="HC0"))),
#     UL = coef(gps.1) + 1.96 * sqrt(diag(vcovHC(gps.1, type="HC0"))))
#     )

# gps.2.d = data.frame(cbind(Estimate= coef(gps.2), "Robust SE" = sqrt(diag(vcovHC(gps.2, type="HC0"))),
# "Pr(>|z|)" = 2 * pnorm(abs(coef(gps.2)/sqrt(diag(vcovHC(gps.2, type="HC0")))), lower.tail=FALSE),
#     LL = coef(gps.2) - 1.96 * sqrt(diag(vcovHC(gps.2, type="HC0"))),
#     UL = coef(gps.2) + 1.96 * sqrt(diag(vcovHC(gps.2, type="HC0"))))
# )



# gps.3.d = data.frame(cbind(Estimate= coef(gps.3), "Robust SE" = sqrt(diag(vcovHC(gps.3, type="HC0"))),
#     "Pr(>|z|)" = 2 * pnorm(abs(coef(gps.3)/sqrt(diag(vcovHC(gps.3, type="HC0")))), lower.tail=FALSE),
#     LL = coef(gps.3) - 1.96 * sqrt(diag(vcovHC(gps.3, type="HC0"))),
#     UL = coef(gps.3) + 1.96 * sqrt(diag(vcovHC(gps.3, type="HC0"))))
# )



# gps.4.d = data.frame(cbind(Estimate= coef(gps.4), "Robust SE" = sqrt(diag(vcovHC(gps.4, type="HC0"))),
#       "Pr(>|z|)" = 2 * pnorm(abs(coef(gps.4)/sqrt(diag(vcovHC(gps.4, type="HC0")))), lower.tail=FALSE),
#     LL = coef(gps.4) - 1.96 * sqrt(diag(vcovHC(gps.4, type="HC0"))),
#     UL = coef(gps.4) + 1.96 * sqrt(diag(vcovHC(gps.4, type="HC0"))))
# )



# gps.5.d = data.frame(cbind(Estimate= coef(gps.5), "Robust SE" = sqrt(diag(vcovHC(gps.5, type="HC0"))),
#     "Pr(>|z|)" = 2 * pnorm(abs(coef(gps.5)/sqrt(diag(vcovHC(gps.5, type="HC0")))), lower.tail=FALSE),
#     LL = coef(gps.5) - 1.96 * sqrt(diag(vcovHC(gps.5, type="HC0"))),
#     UL = coef(gps.5) + 1.96 * sqrt(diag(vcovHC(gps.5, type="HC0"))))
# )



# gps.6.d = data.frame(cbind(Estimate= coef(gps.6), "Robust SE" = sqrt(diag(vcovHC(gps.6, type="HC0"))),
#     "Pr(>|z|)" = 2 * pnorm(abs(coef(gps.6)/sqrt(diag(vcovHC(gps.6, type="HC0")))), lower.tail=FALSE),
#     LL = coef(gps.6) - 1.96 * sqrt(diag(vcovHC(gps.6, type="HC0"))),
#     UL = coef(gps.6) + 1.96 * sqrt(diag(vcovHC(gps.6, type="HC0"))))
# )



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
gps.6.robust = coeftest(gps.6, vcov = vcovCluster(gps.6, cluster = as.numeric(dat$municipality)))


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
        custom.coef.names = c(# this gotta be before OMIT.COEFF
                "(Intercept)",
                "High Density",
                "weights",
                "Perception of Corruption",
                "Political Involvement",
                "Urban",
                "Population (ln)",
                "Education",
                "Individual Income",
                "Municipal Opposition",
                "Political Id",
                "Political Involvement TIMES Population (ln)",
                "Individual Income TIMES Municipal Opposition"),
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






##########################
#   GPS  Weighting PLOT [gps:fig]
##########################
# Generating the Propensity Score 
library(CBPS, quietly = T) # install.packages("CBPS")


fit <- CBPS(wagehalf ~ income, data = dat, iterations = 2500)
# Attaching weights to DF
dat$weights = round(fit$weights, digits=10)



# Recode Before modeling
dat$clien1dummy <- as.numeric(dat$clien1dummy)
dat$logpop = log(dat$pop)
library(car)
dat$clien1dummy <- recode(dat$clien1dummy, "1 = 0 ; 2 = 1")


# model
## sort data first
m.data$municipality = sort(m.data$municipality)

library(Zelig, quietly = T)

# Model but using Zelig
gps.logit.gee.4.z <- zelig(clien1dummy ~ wagehalf + income + exc7 + polinv + urban + logpop + ed + munopp + vb3 + weights, 
                           model = "logit.gee", 
                           id = "municipality",
                           cite = FALSE,
                           data = dat)

# Transformation
income.range <- min(dat$income):max(dat$income)

# Simulation x0.gps
low.polinv <- setx(gps.logit.gee.4.z, wagehalf = quantile(dat$wagehalf, .15), income= income.range)
high.polinv <- setx(gps.logit.gee.4.z, wagehalf = quantile(dat$wagehalf, .95), income= income.range)


set.seed(602)
logit.gee.4.s1.gps <- sim(gps.logit.gee.4.z, x=low.polinv, x1=high.polinv, num=500)

logit.gee.4.low.gps = data.frame(logit.gee.4.s1.gps$getqi(qi="ev", xvalue="range")); colnames(logit.gee.4.low.gps) <- seq(1:ncol(as.data.frame(t(income.range))))  # low
logit.gee.4.high.gps = data.frame(logit.gee.4.s1.gps$getqi(qi="ev", xvalue="range1")); colnames(logit.gee.4.high.gps) <- seq(1:ncol(as.data.frame(t(income.range))))  # high



library(ggplot2)
set.seed(602)
ggplot() + 
        geom_point(aes(x=income.range[1], y=logit.gee.4.low.gps[1]), position = position_jitter(width = 3), size = I(5), color = "red", alpha = 1/100) +
        geom_point(aes(x=income.range[2], y=logit.gee.4.low.gps[2]), position = position_jitter(width = 3), size = I(5), color = "red", alpha = 1/100) +
        geom_point(aes(x=income.range[3], y=logit.gee.4.low.gps[3]), position = position_jitter(width = 3), size = I(5), color = "red", alpha = 1/100) +
        geom_point(aes(x=income.range[4], y=logit.gee.4.low.gps[4]), position = position_jitter(width = 3), size = I(5), color = "red", alpha = 1/100) +
        geom_point(aes(x=income.range[5], y=logit.gee.4.low.gps[5]), position = position_jitter(width = 3), size = I(5), color = "red", alpha = 1/100) +
        geom_point(aes(x=income.range[6], y=logit.gee.4.low.gps[6]), position = position_jitter(width = 3), size = I(5), color = "red", alpha = 1/100) +
        geom_point(aes(x=income.range[7], y=logit.gee.4.low.gps[7]), position = position_jitter(width = 3), size = I(5), color = "red", alpha = 1/100) +
        geom_point(aes(x=income.range[8], y=logit.gee.4.low.gps[8]), position = position_jitter(width = 3), size = I(5), color = "red", alpha = 1/100) +
        geom_point(aes(x=income.range[9], y=logit.gee.4.low.gps[9]), position = position_jitter(width = 3), size = I(5), color = "red", alpha = 1/100) +
        geom_point(aes(x=income.range[10], y=logit.gee.4.low.gps[10]), position = position_jitter(width = 3), size = I(5), color = "red", alpha = 1/100) +
        geom_point(aes(x=income.range[11], y=logit.gee.4.low.gps[11]), position = position_jitter(width = 3), size = I(5), color = "red", alpha = 1/100) +
        geom_point(aes(x=income.range[1], y=logit.gee.4.high.gps[1]), position = position_jitter(width = 3), size = I(5), color = "blue", alpha = 1/100) +
        geom_point(aes(x=income.range[2], y=logit.gee.4.high.gps[2]), position = position_jitter(width = 3), size = I(5), color = "blue", alpha = 1/100) +
        geom_point(aes(x=income.range[3], y=logit.gee.4.high.gps[3]), position = position_jitter(width = 3), size = I(5), color = "blue", alpha = 1/100) +
        geom_point(aes(x=income.range[4], y=logit.gee.4.high.gps[4]), position = position_jitter(width = 3), size = I(5), color = "blue", alpha = 1/100) +
        geom_point(aes(x=income.range[5], y=logit.gee.4.high.gps[5]), position = position_jitter(width = 3), size = I(5), color = "blue", alpha = 1/100) +
        geom_point(aes(x=income.range[6], y=logit.gee.4.high.gps[6]), position = position_jitter(width = 3), size = I(5), color = "blue", alpha = 1/100) +
        geom_point(aes(x=income.range[7], y=logit.gee.4.high.gps[7]), position = position_jitter(width = 3), size = I(5), color = "blue", alpha = 1/100) +
        geom_point(aes(x=income.range[8], y=logit.gee.4.high.gps[8]), position = position_jitter(width = 3), size = I(5), color = "blue", alpha = 1/100) +
        geom_point(aes(x=income.range[9], y=logit.gee.4.high.gps[9]), position = position_jitter(width = 3), size = I(5), color = "blue", alpha = 1/100) +
        geom_point(aes(x=income.range[10], y=logit.gee.4.high.gps[10]), position = position_jitter(width = 3), size = I(5), color = "blue", alpha = 1/100) +
        geom_point(aes(x=income.range[11], y=logit.gee.4.high.gps[11]), position = position_jitter(width = 3), size = I(5), color = "blue", alpha = 1/100) +
        xlab("Individual Income") + ylab("Expected Value of Clientelism") + theme_bw()


##########################
#   Plotting Interactions 2 [int:2]
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
#   Plotting Interactions 1 [int:1]
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
# TECHNICAL INTERACTION TERMS PLOTS
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