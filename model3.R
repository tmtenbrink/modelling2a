#install package + instal library
install.packages("EpiModel", dependencies = TRUE)
library("EpiModel")

tot_inf = vector()
#number of people
n_ppl = 10000

#alle data belgie van IHME 04-02 tem 23-10
belgie_data = read.table("belgie_data.CSV", header = TRUE, sep = ",", stringsAsFactors = FALSE)

#totaal aantal infecties per dag: som estimated infections over 10 dagen
#van 13-02 tem 23-10
for (row in 10:length(belgie_data$est_infections_mean)){
  print(sum(belgie_data$est_infections_mean[(row-9):row]))
  
  tot_inf = append(tot_inf, sum(belgie_data$est_infections_mean[(row-9):row]))
  
}

#schalen data 
sc_tot_inf = (n_ppl/11400000)*tot_inf

#fase 1: voor de lockdown 04-02 tem 17-03 (simulatie vanaf 10-03, dus 8 dagen vanaf dag 36 tem dag 43)
#_________________________________________________________________________________________________
#estimate graph
nw1 <- network::network.initialize(n = (10000), directed = FALSE)
formation <- ~edges
target.stats <- c(50000)
coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = 5)
est1 <- netest(nw, formation, target.stats, coef.diss)

#simuleer epidemie
init <- init.net(i.num = 15, r.num = 0)
param <- param.net(inf.prob = 0.025, act.rate = 1, rec.rate = 0.1)
control <- control.net(type = "SIR", nsteps = 8, nsims = 10, ncores = 12)
sim1 <- netsim(est1, param, init, control)

#plot resultaten van fase 1
plot(c(10:263), sc_tot_inf)
par(new = TRUE)

ysim1 = c(1:7)
for(day in 1:7){ysim1[day] = sum(sim1$epi$i.num[day,])/10}

xsim1 = c(36:42)
par(new = TRUE)
lines(xsim1, ysim1, xlim = c(0,275), ylim = c(0, 200), xlab = '', ylab = '', col = "red")
par(new = FALSE)

#errorbars
par(new = TRUE)
yerror1 = c(1:7)
for (day in 1:7){yerror[day] = sd(sim1$epi$i.num[day,]) }
arrows(xsim1, (ysim1-yerror1), xsim1,(ysim1 + yerror1), col = "red", length = 0)



#fase 2: lockdown 17-03 tem 14-04 (29 dagen , dag 43 tem 71)
#_________________________________________________________________________________________________

#estimate graph
nw2 <- network::network.initialize(n = (10000), directed = FALSE)
formation <- ~edges
target.stats <- c(20000)
coef.diss <- dissolution_coefs(dissolution = ~offset(edges), duration = 5)
est2 <- netest(nw2, formation, target.stats, coef.diss)

#simuleer epidemie
#initiaal conditions uit fase 1
init <- init.net(i.num = 47, r.num = 30)
#Vorige 0.23 - 0.44, 0.30 - 0.48, 0.25 -.046, 0.265 - 0.445, 0.25 - 0.45, 0.25 - 0.455
#0.25 - 0.45, 0.255 - 0.47, 0.26 - 0.47
param <- param.net(inf.prob = 0.26, act.rate = 1, rec.rate = 0.47)
control <- control.net(type = "SIR", nsteps = 29, nsims = 10, ncores = 12)
sim2 <- netsim(est2, param, init, control)


#plot resultaten van fase 2
plot(c(10:263), sc_tot_inf)
par(new = TRUE)

ysim2 = c(1:29)
for(day in 1:29){ysim2[day] = sum(sim2$epi$i.num[day,])/10}#nsteps zou 29 moeten zijn

xsim2 = c(43:71)
par(new = TRUE)
lines(xsim2, ysim2, xlim = c(0,275), ylim = c(0, 200), xlab = '', ylab = '')
par(new = FALSE)

#errorbars
par(new = TRUE)
yerror2 = c(1:29)
for (day in 1:29){yerror[day] = sd(sim1$epi$i.num[day,]) }
arrows(xsim2, (ysim2-yerror2), xsim2,(ysim2 + yerror2), col = "blue", length = 0)

#fase 3: heropening 14-04 (dag 71) tem 02-05 (dag 89), 19 dagen
#_________________________________________________________________________________________________

#we gebruiken hetzelfde netwerk als in fase 2

#simuleer epidemie
#initiaal conditions uit fase 1
init <- init.net(i.num = 78, r.num = 2908)
#Vorige 0.26 - 0.45 
param <- param.net(inf.prob = 0.26, act.rate = 1, rec.rate = 0.43)
control <- control.net(type = "SIR", nsteps = 40, nsims = 10, ncores = 12)
sim3 <- netsim(est2, param, init, control)

#plot resultaten van fase 3
plot(c(10:263), sc_tot_inf)
par(new = TRUE)

ysim3 = c(1:40)
for(day in 1:40){ysim3[day] = sum(sim3$epi$i.num[day,])/10}

xsim3 = c(71:110)
par(new = TRUE)
lines(xsim3, ysim3, xlim = c(0,275), ylim = c(0, 200), xlab = '', ylab = '')
par(new = FALSE)

#errorbars
par(new = TRUE)
yerror3 = c(1:40)
for (day in 1:40){yerror[day] = sd(sim1$epi$i.num[day,]) }
arrows(xsim3, (ysim3-yerror3), xsim3,(ysim3 + yerror3), col = "green", length = 0)


#fase 4: begin tweede golf 20-09 (ongeveer start universiteiten, dag 230) tem 06-10 (dag 245, totaal 16 dagen)
#_________________________________________________________________________________________________
#we gebruiken hetzelfde netwerk als in fase 2

#simuleer epidemie
#initiaal conditions uit fase 1
init <- init.net(i.num = 15, r.num = 1000)
#Vorige 0.26-0.43, 0.23 - 0.38
param <- param.net(inf.prob = 0.20, act.rate = 1, rec.rate = 0.43)
control <- control.net(type = "SIR", nsteps = 40, nsims = 10, ncores = 12)
sim4 <- netsim(est2, param, init, control)

#plot resultaten van fase 4
plot(c(10:263), sc_tot_inf)
par(new = TRUE)

ysim4 = c(1:16)
for(day in 1:16){ysim4[day] = sum(sim4$epi$i.num[day,])/10}

xsim4 = c(240:255)
par(new = TRUE)
lines(xsim4, ysim4, xlim = c(0,275), ylim = c(0, 200), xlab = '', ylab = '')
par(new = FALSE)


#fase 5: verstrenging maatregelen 06-10 tem 13-10
#_________________________________________________________________________________________________

#20 september, dag 230
#6 oktober, dag 245
