#lezen hospitalizaties
table_hosp = read.table("COVID19BE_HOSP.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

#lijst maken nieuwe hospitalizaties per dag
new_hosp = data.frame('day' = "2020-01-01", 'hosp' = 0)
prev_day = "2020-03-15"
tot_hosp = 0

for (row in 1:length(table_hosp[1:2113, 1])){
  
  day = table_hosp[row,1]
  #("the day is")
  #print(day)
  
  if(day != prev_day){
    # print('test')
    new_hosp = rbind(new_hosp, data.frame('day' = prev_day, 'hosp' = tot_hosp))
    # print(data.frame('day' = prev_day, 'hospitalizations' = tot_hosp))
    tot_hosp = table_hosp[row,9]
    prev_day = day
    
  }
  else{
    tot_hosp = tot_hosp + table_hosp[row,9]
  }
}
new_hosp[, 'day'] <- as.Date(new_hosp[, 'day'], "%Y-%m-%d")
new_hosp = new_hosp[-1,]

plot(new_hosp)

#de geschatte piek in het aantal nieuwe infecties ligt op 26 maart, die in de hospitalizaties op 28 maart
#we verschuiven de piek van de hospitalisaties dus niet (te klein verschil)
#het geschatte maximum is 16943, en het maximale gemeten aantal hospitalizaties is 629

new_active = data.frame(new_hosp$day , (16943/629)*new_hosp$hosp)


#bepalen active cases

active = data.frame('day' = as.Date('2020-01-01', "%Y-%m-%d"), 'active' = 0)

for (row in 11:length(new_active[,1])){
  act = sum(new_active[(row-10):row,2])
  active = rbind(active, data.frame('day' = as.Date(new_active[row,1], "%Y-%m-%d"), 'active' = act))
}
active = active[-1,]
plot(active$day, active$active, type="l", main="Estimated active cases versus modelled active cases", xlab="Date", 
     ylab="Number of active cases", col='indianred4', xlim=as.Date(c('2020-03-20', '2020-06-03'), "%Y-%m-%d"))
legend("topright",legend=c("Estimated cases", "Modeled cases"),lty=1:2, col=c("indianred4","red"))

sim = read.table(text = gsub("\\s+", "\t", readLines("inum1.txt")))

sim_l = length(sim$sim1)
avg_sim = numeric(sim_l)
for (i in 1:sim_l){
  avg_sim[i] = mean(as.numeric(sim[i,]))*11400000/10000
}

dateseq = seq(as.Date('2020-03-17', "%Y-%m-%d"), by = "day", length.out = sim_l)

lines(dateseq, avg_sim, lty='dashed', col='red')