# model.m.s = zelig(clien1dummy ~ wealth * munopp * large + pop.10 + polinv + ing4 + vb3 + exc7 + ed, 
model.m.s = zelig(clien1dummy ~ wealth * munopp * large + pop.10 + polinv + ing4 + vb3 + exc7 + ed, 
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



# simulation DISTRIBUTION PLOTS
library(Zelig)
high.poor.lowcomp = data.frame(competition = rep("Low Competition", 10000), income = rep("Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE,large = max(m.data$large), wealth= quantile(m.data$wealth, .25), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.poor.highcomp = data.frame(competition = rep("High Competition", 10000),income = rep("Poor Individuals", 1000000), x = sim(x = setx(model.m.s, cond = TRUE,large = max(m.data$large), wealth= quantile(m.data$wealth, .25), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.lowcomp = data.frame(competition = rep("Low Competition", 10000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = max(m.data$large), wealth= quantile(m.data$wealth, .75), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
high.rich.highcomp = data.frame(competition = rep("High Competition", 10000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = max(m.data$large), wealth= quantile(m.data$wealth, .75), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.lowcomp = data.frame(competition = rep("Low Competition", 10000),income = rep("Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = min(m.data$large), wealth= quantile(m.data$wealth, .25), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.poor.highcomp = data.frame(competition = rep("High Competition", 10000),income = rep("Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = min(m.data$large), wealth= quantile(m.data$wealth, .25), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.lowcomp = data.frame(competition = rep("Low Competition", 10000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = min(m.data$large), wealth= quantile(m.data$wealth, .75), munopp = min(m.data$munopp)), num=1000000)$getqi(qi="ev"))
low.rich.highcomp = data.frame(competition = rep("High Competition", 10000),income = rep("Non-Poor Individuals", 1000000), x= sim(x = setx(model.m.s, cond = TRUE,large = min(m.data$large), wealth= quantile(m.data$wealth, .75), munopp = max(m.data$munopp)), num=1000000)$getqi(qi="ev"))

# plot
library(ggplot2)
ggplot() + 
  geom_density(aes(x=x, colour="High"), data= high.poor.lowcomp) + 
  geom_density(aes(x=x, colour="High"), data= high.poor.highcomp) + 
  geom_density(aes(x=x, colour="High"), data= high.rich.lowcomp) + 
  geom_density(aes(x=x, colour="High"), data= high.rich.highcomp) + 
  geom_density(aes(x=x, colour="Low"), data= low.poor.lowcomp) + 
  geom_density(aes(x=x, colour="Low"), data= low.poor.highcomp) + 
  geom_density(aes(x=x, colour="Low"), data= low.rich.lowcomp) + 
  geom_density(aes(x=x, colour="Low"), data= low.rich.highcomp) + 
  ylab("Estimated Density") + xlab("Expected Value of Clientelism") +
  theme_bw() +
  theme(axis.title=element_text(size=10), legend.text = element_text(size = 10), legend.title = element_text(size = 10))  + scale_colour_discrete(name = "Density of the Poor") +
  facet_grid(competition~income, scales ="free")
