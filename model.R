#install package + instal library
install.packages("EpiModel", dependencies = TRUE)
library("EpiModel")

memory.limit(size = 15000)

#lijst met aantal active cases per dag
active$active = (10000/11400000)*active$active

#fase 1: voor de lockdown: van  10/03 - 16/03

#estimate graph
nw1 <- network::network.initialize(n = (10000), directed = FALSE)
formation <- ~edges + concurrent
target.stats <- c(50000,7500)
coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = 2)
est1 <- netest(nw, formation, target.stats, coef.diss)

#simuleer epidemie
init <- init.net(i.num = 18,r.num = 0)
param <- param.net(inf.prob = 0.0275, act.rate = 1, rec.rate = 0.1)
control <- control.net(type = "SIR", nsteps = 7, nsims = 10, ncores = 12)
sim1 <- netsim(est1, param, init, control)

print(sim1)

#plot resultaten van fase 1
x_active = c(13: (12 + length(active$active)))
plot(x_active, active$active, xlim = c(0,length(x_active)), ylim = c(0,max(active$active)))
par(new = TRUE)
plot(sim1, y = "i.num", axes = FALSE, xlim = c(0,length(x_active)), ylim = c(0,max(active$active)), xlab = '', ylab = '')
par(new = FALSE)


#fase 2: lockdown 17/03 - 14/4
#estimate graph
nw2 <- network::network.initialize(n = (10000), directed = FALSE)
#nw2 <- network::set.vertex.attribute(nw2, "household", rep(1:(2500), each = 4))

formation <- ~edges
target.stats <- c(20000)
coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = 5)
est2 <- netest(nw2, formation, target.stats, coef.diss)

#simuleer epidemie
init <- init.net(i.num = 50, r.num = 30)
param <- param.net(inf.prob = 0.19, act.rate = 1, rec.rate = 0.4)
control <- control.net(type = "SIR", nsteps = 60, nsims = 10, ncores = 5)
sim2 <- netsim(est2, param, init, control)

#plot resultaten van fase 2
x_active = c(13: (12 + length(active$active)))
plot(x_active, active$active, xlim = c(0,length(x_active)), ylim = c(0,max(active$active)))
par(new = TRUE)
ysim2 = c(1:60)
for(day in 1:60){ysim2[day] = sum(sim2$epi$i.num[day,])/10}
xsim2 = c(8:67)
par(new = TRUE)
lines(xsim2, ysim2, xlim = c(0,length(x_active)), ylim = c(0,max(active$active)), xlab = '', ylab = '')
par(new = FALSE)
