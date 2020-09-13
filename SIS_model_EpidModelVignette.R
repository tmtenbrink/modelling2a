#independent SIS model

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