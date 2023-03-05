##############################
### Comparing Alternatives ###
##############################

# Loading the data
final_data <- read.csv("final_data.csv",header = T)[,2:5]


alpha_total <- 0.1
alpha_i <- alpha_total/2

#------------------------------------------------------------served KPI

# alternative 1 -- Current
pairdTest1 <- t.test(x=final_data$served1,y=final_data$served0,alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=alpha_i)
print(pairdTest1)

#------------------------------------------------------------got dessert KPI

# alternative 1 -- Current
pairdTest2 <- t.test(x=final_data$dessert1,y=final_data$dessert0,alternative="two.sided",paired=TRUE,var.equal=TRUE,conf.level=alpha_i)
print(pairdTest2)

#------------------------------------------------------------


# one-way t-test state
## served0
test1 <- t.test(x= final_data$served0,y=NULL, alternative="two.sided",conf.level=alpha_total)
print(test1)
sd(final_data$served0)
## dessert0
test2 <- t.test(x= final_data$dessert0,y=NULL, alternative="two.sided",conf.level=alpha_total)
print(test2)
sd(final_data$dessert0)

# one-way t-test alternative 1
## served1
test3<- t.test(x= final_data$served1,y=NULL, alternative="two.sided",conf.level=alpha_total)
print(test3)
sd(final_data$served1)
## dessert1
test4<- t.test(x= final_data$dessert1,y=NULL, alternative="two.sided",conf.level=alpha_total)
print(test4)
sd(final_data$dessert1)

