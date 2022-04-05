colnames(weather[, sapply(weather, class) == "factor"])


ggplot(weather,aes(climate_type,fill=TempLabel)) + geom_bar()
ggplot(weather,aes(date,fill=TempLabel)) + geom_bar()
ggplot(weather,aes(stn_type,fill=TempLabel)) + geom_bar()
ggplot(weather,aes(climate_type,fill=TempLabel)) + geom_bar()
ggplot(weather,aes(stn_elev_2000,fill=TempLabel)) + geom_bar()
ggplot(weather,aes(district,fill=TempLabel)) + geom_bar()
ggplot(weather,aes(lg_aqua_night_lst_binary,fill=TempLabel)) + geom_bar()

#types:
ggplot(weather,aes(climate_type,stn_type,color = TempLabel)) + geom_jitter() +
  scale_color_manual(values=c("red", "blue", "green","black","yellow"))
#jitter:
ggplot(weather,aes(date,district,color = TempLabel)) + geom_jitter() +
  scale_color_manual(values=c("red", "blue", "green","black","yellow"))
ggplot(weather,aes(date,lg_aqua_night_lst_binary,color = TempLabel)) + geom_jitter() +
  scale_color_manual(values=c("red", "blue", "green","black","yellow"))
# date vs. Temperature
ggplot(weather,aes(date,TempLabel,color = TempLabel)) + geom_jitter() +
  scale_color_manual(values=c("red", "blue", "green","black","yellow"))



ggplot(weather,aes(lg_aqua_night_lst_binary,TempLabel,color = TempLabel)) + geom_jitter() +
  scale_color_manual(values=c("red", "blue", "green","black","yellow"))

ggplot(weather,aes(lg_aqua_night_lst_binary,stn_elev_2000,color = TempLabel)) + geom_jitter() +
  scale_color_manual(values=c("red", "blue", "green","black","yellow"))
comp <- c()
for(i in 1:length(weather$lg_aqua_night_lst_binary)){
  vec2000 <- weather$stn_elev_2000
  vec_lg <- weather$lg_aqua_night_lst_binary
  if(vec2000[i]>0 & vec_lg[i]>0){
    comp[i] <- 1
  }
  else if(vec2000[i]<1 & vec_lg[i]>0){
    comp[i] <- 2
  }
  if(vec2000[i]>0 & vec_lg[i]<1){
    comp[i] <- 3
  }
  else{
    comp[i] <- 4
  }
}
table(comp)
weather$comp <- as.factor(comp)

 
