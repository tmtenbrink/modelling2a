library(ggplot2)

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
dateseqest = seq(as.Date('2020-02-13', "%Y-%m-%d"), by = "day", length.out = length(sc_tot_inf))



sim_f1 = read.table(text = gsub("\\s+", "\t", readLines("inum1.txt")))

sim_f1_l = length(sim_f1$sim1)
avg_sim_f1 = numeric(sim_f1_l)
err_sim_f1 = numeric(sim_f1_l)
for (i in 1:sim_f1_l){
  avg_sim_f1[i] = mean(as.numeric(sim_f1[i,]))
  err_sim_f1[i] = sd(as.numeric(sim_f1[i,]))
}

dateseqsim_f1 = seq(as.Date('2020-03-10', "%Y-%m-%d"), by = "day", length.out = sim_f1_l)

sim_f2 = read.table(text = gsub("\\s+", "\t", readLines("inum2.txt")))

sim_f2_l = 29
avg_sim_f2 = numeric(sim_f2_l)
err_sim_f2 = numeric(sim_f2_l)
for (i in 1:sim_f2_l){
  avg_sim_f2[i] = mean(as.numeric(sim_f2[i,]))
  err_sim_f2[i] = sd(as.numeric(sim_f2[i,]))
}

dateseqsim_f2 = seq(as.Date('2020-03-16', "%Y-%m-%d"), by = "day", length.out = sim_f2_l)

sim_f3 = read.table(text = gsub("\\s+", "\t", readLines("inum3.txt")))

sim_f3_l = 19
avg_sim_f3 = numeric(sim_f3_l)
err_sim_f3 = numeric(sim_f3_l)
for (i in 1:sim_f3_l){
  avg_sim_f3[i] = mean(as.numeric(sim_f3[i,]))
  err_sim_f3[i] = sd(as.numeric(sim_f3[i,]))
}

err_high = c(err_sim_f1, err_sim_f2[2:length(err_sim_f2)], err_sim_f3[2:length(err_sim_f3)]) + 
  c(avg_sim_f1, avg_sim_f2[2:length(err_sim_f2)], avg_sim_f3[2:length(err_sim_f3)])
err_low = c(avg_sim_f1, avg_sim_f2[2:length(err_sim_f2)], avg_sim_f3[2:length(err_sim_f3)]) -
  c(err_sim_f1, err_sim_f2[2:length(err_sim_f2)], err_sim_f3[2:length(err_sim_f3)])

dateseqsim_f3 = seq(as.Date('2020-04-13', "%Y-%m-%d"), by = "day", length.out = sim_f3_l)
dateseq_tot = c(dateseqsim_f1, dateseqsim_f2[2:length(dateseqsim_f2)], dateseqsim_f3[2:length(dateseqsim_f3)])

plot(dateseq_tot,err_high, type="n", main="Estimated active cases versus modelled active cases", xlab="Date", 
     ylab="Number of active cases per 10.000", col='blue')
polygon(c(dateseq_tot,rev(dateseq_tot)),c(err_low,rev(err_high)),col=rgb(1, 0, 0,0.3))
lines(dateseqest, sc_tot_inf, col='blue')
legend("topright",legend=c("Estimated cases", "Modeled cases", "Phase delineation"),lty=1:2, col=c("blue","red", "green"))
lines(dateseqsim_f1, avg_sim_f1, lty='dashed', col='red')
lines(dateseqsim_f2, avg_sim_f2, lty='dashed', col='red')
lines(dateseqsim_f3, avg_sim_f3, lty='dashed', col='red')
abline(v=as.Date('2020-03-16', "%Y-%m-%d"),col='green')
abline(v=as.Date('2020-04-13', "%Y-%m-%d"),col='green')