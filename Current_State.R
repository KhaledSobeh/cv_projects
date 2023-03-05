##############################
##### Simulation Project #####
##############################

##---------- 0.  loading libraries ----------##

library(rlang)
library(MASS)
library(fitdistrplus)
library(magrittr)
library(dplyr)
library(lazyeval)
library(parallel)
library(e1071)
library(plotly)
library(ggplot2)
library(triangle)
library(sqldf)
library(readxl)
library(knitr)
library(rmarkdown)
library(simmer)
library(simmer.plot)
library(stringr)
library(purrr)


##---------- 1.  all functions ----------##

## trimmed norm
trimmedNorm<-function(mu,sd){
  while(TRUE){
    sample<-rnorm(1,mu,sd)
    if (sample>0)
      return (sample)
  }
}

mainDishTime<-function(elements){ 
  #waiting for last person to finish eating(max time)
  ans <- c()
  for(i in 1:elements){
    ans <- c(ans,ifelse(rdiscrete(1,c(0.7,0.3),c(1,2)==1),trimmedNorm(12,3),trimmedNorm(7,4)))
  }
  return(max(ans))
}

nextStand <- function(stand_vec){
  ## return which stands a guest did not go to yet.
  stand_names <- c("focaccia_stand","banni_stand","sushi_stand1", "sushi_stand2","vegetarian_stand", 
                   "tortilla_stand")
  ans <- c()
  for(i in 1:6){
    if(stand_vec[i]==0){
      ans <- c(ans,stand_names[i])
    }
  }
  return(ans)
}

toBarOrNot <- function(type){
  #return if a person wants to go to the bar
  if(type==0){ #singles
    return(rdiscrete(1,c(0.8,0.2),c(0,1)))
  }
  if(type==1){ #couples
    return(rdiscrete(1,c(0.6,0.4),c(0,1)))
  }
  return(rdiscrete(1,c(0.4,0.6),c(0,1)))
}

SatietyLevel <- function(){
  #ramat sovaa: for one person
  while(T){
    y <- 4*runif(1,0,1) + 1 #algorithm for r(x)
    u2 <- runif(1,0,1)
    if(y <= 2){
      if(u2<=(((3*y*y)/14) + ((2*y)/15) + (1/15))/(25/21)){
        return(y)
      }
    }
    else if(y <= 3){
      if(u2<=((2/3)-(y/5))/(25/21)){
        return(y)
      }
    }
    else{
      if(u2<=((1/6)-(y/30))/(25/21)){
        return(y)
      }
    }
  }
}

SatietyLevelForGroup <- function(members){
  #get the ramat sovaa for the whole group, then check if it's less than 4.5
  #if so, you go in queue before those who have >=4.5.
  ans <- c()
  for(i in 1:members){
    ans <- c(ans ,SatietyLevel())
  }
  if(sum(ans)<4.5){
    return(c(1,1,F))
  }
  return(c(0,1,F))
}


##---------- 2.  all simulation parameters ----------##

simulationTime <- 6*60

##---------- 3.  Init Simulation and add all resources  ----------##

wedding <- simmer("wedding") %>%
  add_resource("canopy",capacity = 0, queue_size=Inf) %>%
  add_resource("reception_desk1",capacity= 2, queue_size=Inf) %>%
  add_resource("reception_desk2",capacity= 2, queue_size=Inf) %>%
  add_resource("focaccia_stand",capacity= 3, queue_size=Inf) %>%   #1
  add_resource("banni_stand",capacity= 3, queue_size=Inf) %>%      #2
  add_resource("sushi_stand1",capacity= 2, queue_size=Inf) %>%     #3
  add_resource("sushi_stand2",capacity= 1, queue_size=Inf) %>%     #4
  add_resource("vegetarian_stand",capacity= 4, queue_size=Inf) %>% #5
  add_resource("tortilla_stand",capacity= 4, queue_size=Inf) %>%   #6
  add_resource("outside_bar",capacity= 3, queue_size=Inf) %>%
  add_resource("inside_bar1",capacity= 5, queue_size=Inf) %>%
  add_resource("inside_bar2",capacity= 7, queue_size=Inf) %>%
  add_resource("dessert_stand",capacity= 8, queue_size=15, preemptive = F)

##---------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ----------## 

dessert_traj <- trajectory("dessert_traj") %>% 
  #inter only if there is a place for you
  branch(option =  function() ifelse(get_queue_count(wedding,"dessert_stand")<15,1,0),c(F),
         trajectory() %>% seize("dessert_stand",1) %>%
           timeout(function() runif(1,2.5,4)) %>%
           set_global("found_dessert",1, mod = "+") %>%
           release("dessert_stand",1))

hall_traj <- trajectory("hall_traj") %>%
  #first singles and couples go to the bar.
  branch(option = function() ifelse(get_attribute(wedding,"guest_type")==2,0,1), continue = c(T),
         trajectory() %>%  simmer::select(resources= c("inside_bar1","inside_bar2"), policy=c("shortest-queue") , id = 2)%>%
           seize_selected(amount = 1, id = 2) %>%
           timeout(function() rexp(1,2.4)) %>%
           release_selected(amount = 1, id = 2)) %>%
  ## find table
  timeout(function() rexp(1,1/3.5)) %>%
  ##eating time: main dishes
  timeout(function() mainDishTime(get_attribute(wedding,"group_members"))) %>%
  ##dessert time
  set_prioritization(function() SatietyLevelForGroup(get_attribute(wedding,"group_members"))) %>%
  ##rollback can be misleading, and we have to track it steps if we add code,
  ##so we will join three times, and this also easier to understand.
  join(dessert_traj) %>%
  timeout(function() rtriangle(1,a=5,b=10,c=7)) %>%
  join(dessert_traj) %>%
  timeout(function() rtriangle(1,a=5,b=10,c=7)) %>%
  join(dessert_traj) %>%
  log_("go home sad")

canopy_traj <- trajectory("canopy_traj") %>%
  seize(resource = "canopy", amount = 1)%>%
  release(resource = "canopy",amount = 1)%>%
  # those who cloned, will wait for everyone, else will continue to the hall.
  synchronize(wait = T,mon_all = T) %>%
  join(hall_traj)

reception_traj <- trajectory("reception_traj") %>%
  ## from seize to rollback: this loop will end if the capony starts, or a guest ate all he can eat. first they eat:
  simmer::select(resources=function() nextStand(get_attribute(wedding,keys = c("focaccia_stand","banni_stand", "sushi_stand1", "sushi_stand2", "vegetarian_stand", "tortilla_stand"))),
                 policy=c("shortest-queue") , id = 1)%>%
  seize_selected(amount = 1, id = 1) %>%
  timeout(function() trimmedNorm(1.5,0.7)) %>%
  branch(option = function() ifelse(get_selected(wedding,id=1) %in% c("sushi_stand1","sushi_stand2"),1,2),continue = c(T,T),
         trajectory() %>% set_attribute("sushi_stand1",1,mod = "+") %>% set_attribute("sushi_stand2",1,mod = "+"),
         trajectory() %>% set_attribute(function() get_selected(wedding,id = 1),1,mod = "+")) %>%
  set_global("served_to",1,mod = "+") %>%
  release_selected(amount = 1, id = 1) %>%
  
  timeout(function() rexp(1,0.8)) %>% #eating time
  branch(option = function() ifelse(get_global(wedding,"is_bar_open")==1,0,1),continue=c(F), trajectory() %>% join(canopy_traj)) %>%
  ## then they drink(if they want to).
  branch(option =  function() toBarOrNot(get_attribute(wedding,"guest_type")),c(T),
         trajectory() %>% seize("outside_bar") %>% timeout(function() rexp(1,2)) %>% release("outside_bar")) %>%
  branch(option = function() ifelse(get_global(wedding,"is_reception_open")==1,0,1),continue=c(F), trajectory() %>% join(canopy_traj)) %>%
  rollback(amount = 10, check = function() 6 != sum(get_attribute(wedding,keys = c("focaccia_stand","banni_stand", "sushi_stand1", 
                                                                                   "sushi_stand2", "vegetarian_stand", "tortilla_stand")))) %>%
  join(canopy_traj)


to_reception_or_canopy_or_hall_traj <- trajectory("to_reception_or_canopy_or_hall_traj") %>%
  ##if canopy finished, go to hall
  branch(option = function() get_global(wedding,"is_canopy_ended"),continue=c(F),
         trajectory() %>% join(hall_traj)) %>%
  ## if the canopy not started, then continue, else means it's happening, then go to canopy traj.
  branch(option = function() ifelse(get_global(wedding,"is_reception_open")==1,0,1),continue=c(F),
         trajectory() %>% join(canopy_traj)) %>%
  ## family and couple seperate
  branch(option=function() get_attribute(wedding,"guest_type") ,continue= c(T,T), 
         trajectory() %>% clone(n = 2),
         trajectory() %>% clone(n = function() get_attribute(wedding,"group_members"))) %>%
  ## single won't go to banni and focaccia stands
  branch(option = function() ifelse(get_attribute(wedding,"guest_type")==0,1,2), continue = c(T,T),
         trajectory() %>% set_attribute(keys = c("focaccia_stand","banni_stand","sushi_stand1", "sushi_stand2",
                                                 "vegetarian_stand", "tortilla_stand"), values = c(1,1,0,0,0,0)),
         trajectory() %>% set_attribute(keys = c("focaccia_stand","banni_stand","sushi_stand1", "sushi_stand2",
                                                 "vegetarian_stand","tortilla_stand"), values = c(0,0,0,0,0,0))) %>%
  join(reception_traj)

reception_desk_traj <- trajectory("reception_desk_traj") %>%
  simmer::select(resources= c("reception_desk1","reception_desk2"), policy=c("shortest-queue") , id = 0)%>%
  seize_selected(amount = 1, id = 0) %>%
  timeout(function() rexp(1,2.4)) %>%
  release_selected(amount = 1, id = 0) %>%
  branch(option = function() rdiscrete(1,c(0.07,0.93),c(1,0)),continue=c(F), trajectory()) %>% # 7%:wrong event
  set_attribute("table_number", function() rdunif(1,1,95)) %>%
  join(to_reception_or_canopy_or_hall_traj)


family_traj <- trajectory("family_traj") %>%
  set_attribute("guest_type", 2) %>%
  set_attribute("group_members", function() rdiscrete(1,c(0.33,0.4,0.27),c(3,4,5))) %>%
  timeout(function() rtriangle(1,a=3,b=5,c=4)) %>%
  join(reception_desk_traj)

couple_traj <- trajectory("couple_traj") %>%
  set_attribute("group_members", 2) %>%
  set_attribute("guest_type", 1) %>%
  timeout(function() rtriangle(1,a=3,b=5,c=4)) %>%
  join(reception_desk_traj)

single_traj <- trajectory("single_traj") %>%
  set_attribute("group_members", 1) %>%
  set_attribute("guest_type", 0) %>%
  join(reception_desk_traj)

canopy_traj_setting <- trajectory("canopy_traj_setting") %>%
  set_global("is_canopy_ended",0) %>% #canopy not started yet
  set_capacity(resource = "canopy", value = 0) %>%
  timeout(function() 140 - 10) %>%
  timeout(function() runif(1,0,20)) %>% #10 mins before the canopy, we will close the bar.
  set_global("is_bar_open",0) %>% #bar closed 10 mins befor the canopy.
  timeout(10) %>%
  set_global("is_reception_open",0) %>%
  timeout(function() runif(1,20,35)) %>% #canopy time
  set_capacity(resource = "canopy", value = Inf) %>%
  set_global("is_canopy_ended",1) 

start_traj_setting <- trajectory('start_traj_setting') %>%
  set_global("found_dessert",0) %>%
  set_global("served_to",0) %>%
  set_global("is_reception_open",1) %>%
  set_global("is_bar_open",1) %>%
  timeout(60) %>%
  timeout(function() runif(1,0,15)) %>% #in 19:30-19:45 the bus will arrive.
  activate("single")

##---------- 5.  All Generators, ALWAYS LAST. ----------##

wedding %>%
  add_generator("start" , start_traj_setting , distribution = at(0), mon = 2) %>%
  add_generator("canopy", canopy_traj_setting, distribution = at(0), mon = 2) %>%
  add_generator("family", family_traj,         distribution = to(stop_time=4*60, dist=function() rexp(1,0.811765)),mon = 2) %>%
  add_generator("couple", couple_traj,         distribution = to(stop_time=4*60, dist=function() rexp(1,0.5587186)), mon = 2) %>%
  add_generator("single", single_traj,         when_activated(n =  function() rdunif(1,100,150)), mon = 2)

##---------- 6.  reset, run, plots, outputs ----------##

mm0envs <- mclapply(1:50, function(i) {
  set.seed(((i+100)^2)*3-13)
  reset(wedding)%>%run(until=simulationTime) %>%
  wrap()
})

##---------- Answers ----------##

weddingAttributeData0 <- get_mon_attributes(mm0envs)
weddingArrivalData0 <- get_mon_arrivals(mm0envs, per_resource=T, ongoing = T)


ServedData0 <- sqldf("select max(value) as served
                     from weddingAttributeData0
                     where key = 'served_to'
                     group by replication")

happy0 <- sqldf("select max(value) as dessert
                     from weddingAttributeData0
                     where key = 'found_dessert'
                     group by replication")

(data0 <- data.frame(ServedData0,happy0))

write.csv(data0, "data0.csv")
