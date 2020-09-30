#independent SIS model

 install.packages("EpiModel", dependencies = TRUE)
 library("EpiModel")

#Estimating network structure

#maak een netwerk (zonder verbindingen) nw met 1000 nodes (personen in dit model) 
nw <- network::network.initialize(n = 1000, directed = FALSE)
#voeg aan elke node een een attribute toe (hier "risk", een binaire variabele)
nw <- network::set.vertex.attribute(nw, "risk", rep(0:1, each = 500))

#Network model estimation and diagnostics

#formation is een formule object in r (hier staat "formation wordt gegeven door edges, nodefactor('risk') 
#, nodematch('risk') en concurrent)
#deze betekenen:  edges = aantal verbindingen
#                 nodefactor('risk') = het aantal verbindingen voor nodes met een bepaalde waarde voor 'risk'
#                 nodematch("risk") = aantal verbindingen tussen nodes met een bepaalde waarde voor 'risk'
#                 concurrent = aantal nodes met 2 of meer verbindingen op elk tijdstip
#andere factoren in de "ergm-terms" help-file
formation <- ~edges + nodefactor("risk") + nodematch("risk") + concurrent

#ik neem aan dat we het model willen fitten aan deze data
#de nodefactor en nodematch zijn voor de high risk groep, maar de waarden voor de low risk groep zijn met 
#de waarde voor edges ook vastgesteld
target.stats <- c(250, 375, 225, 100)

coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = 80)

print(coef.diss)

est1 <- netest(nw, formation, target.stats, coef.diss)

dx <- netdx(est1, nsims = 10, nsteps = 1000)

print(dx)

plot(dx)

par(mfrow = c(1, 2))
plot(dx, type = "duration")
plot(dx, type = "dissolution")

init <- init.net(i.num = 50,r.num = 0)

param <- param.net(inf.prob = 0.1, act.rate = 5, rec.rate = 0.02)

control <- control.net(type = "SIR", nsteps = 500, nsims = 10, epi.by = "risk")

sim1 <- netsim(est1, param, init, control)

print(sim1)

summary(sim1, at = 500)

plot(sim1)

plot(sim1, y = c("si.flow", "is.flow"))

plot(sim1, y = c("i.num.risk0", "i.num.risk1"))

par(mfrow = c(1,2), mar = c(0,0,1,0))
plot(sim1, type = "network", at = 1, sims = "mean", col.status = TRUE, main = "Prevalence at t1")
plot(sim1, type = "network", at = 500, sims = "mean", col.status = TRUE, main = "Prevalence at t500")
par(mfrow = c(1,1), mar = c(0,0,1,0))












