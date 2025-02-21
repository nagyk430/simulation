###Simulation for the number of the religious people in Hungary

#Creating database of the settlement
telepules <- data.frame(id=1:3155, pop=round(rnorm(3155, 3070, 3000)))
telepules$pop <- abs(telepules$pop)
summary(telepules$pop)

telepules$telep_tip <- ifelse(telepules$pop < 1490, 4,
                              ifelse(telepules$pop >= 1490 & telepules$pop < 3106, 3,
                                     ifelse(telepules$pop >= 3106 & telepules$pop <= 5034, 2, 1)))


populacio 	<- NULL
for (i in unique(telepules$id)) {
  populacio 	<- rbind(populacio, data.frame(telep_id=rep(i, telepules[telepules$id==i,"pop"])))
}

#Sample reception
vszeg <- 1/3000
minta <- sample(1:nrow(populacio), nrow(populacio)*vszeg)

populacio$vallas 			<- 0 
populacio[minta, "vallas"] 	<- 1 #who are in the sample, give 1 values

#Aggregate values 
vallasi_aggr <- aggregate(vallas ~ telep_id, data=populacio, sum)
vallasi_aggr

vallasi_aggr <- cbind(vallasi_aggr, pop=telepules$pop)

#Category of the settlement
vallasi_aggr$telep_tip <- ifelse(vallasi_aggr$pop < 1490, 4,
                                 ifelse(vallasi_aggr$pop >= 1490 & vallasi_aggr$pop < 3106, 3,
                                        ifelse(vallasi_aggr$pop >= 3106 & vallasi_aggr$pop <= 5034, 2, 1)))

#Expected values
vallasi_aggr$vart <- vallasi_aggr$pop*vszeg
head(vallasi_aggr)

cor(vallasi_aggr)

poiss.base <- glm(vallas ~ 1 + offset(log(pop)), family="poisson", data=vallasi_aggr)
summary(poiss.base)

mod1 <- lm(vallas ~ as.factor(telep_tip), data=vallasi_aggr)
summary(mod1)

vallasi_aggr$predicted0 <- exp(predict(poiss.base))
head(vallasi_aggr, 10)

plot(vallasi_aggr$vallas, vallasi_aggr$pop)

poiss.base2 <- glm(vallas ~ as.factor(telep_tip) + offset(log(pop)), family ="poisson", data=vallasi_aggr)
summary(poiss.base2)


