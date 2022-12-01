###rarefaction table
Gdata = read.csv("table.csv", header = T)

##define rarefaction function

rare = function(k){
#extracting data of target sample
Ldata = Gdata[,k][which(Gdata[,k] != 0)]

#generating a vector
n = rep(c(1:length(Ldata)),Ldata)

#subsample
est = rep(NA, 10000)
for (i in 1:10000){
m = sample(n,size = 25,replace = F)
est[i] = length(levels(factor(m)))
}
hist(est)
return(c(mean(est),sd(est),quantile(est,0.025),quantile(est,0.975)))
}

results = matrix(NA,6,4)
for (j in 1:6){
  results[j,] = rare(j)
}

write.csv(results,file = "file directory/name.csv")
