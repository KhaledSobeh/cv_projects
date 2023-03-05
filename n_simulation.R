########################################
### Finding the number of iterations ###
########################################


# Loading the data
sim_data <- read.csv("final_data.csv",header = T)[,2:5]

# Check if the alternative meet the relative accuracy requirement n (Checking)
find_n <- function(alphA,kpi_number,gammA,KPI){
  n <- length(KPI)
  alpha_i <- alphA/kpi_number
  delta <- abs(qt(p=alpha_i/2, df=n-1))*(sd(KPI)/sqrt(n))
  Checking <- delta/mean(KPI) <= gammA/(1+gammA)
  delta_mean <- delta/mean(KPI)
  gama <- gammA/(1+gammA)
  return(c(delta_mean,gama,Checking))
}

kpi01 <- find_n(0.1,2,0.09,sim_data$served0)
kpi02 <- find_n(0.1,2,0.09,sim_data$dessert0)

kpi11 <- find_n(0.1,2,0.09,sim_data$served1)
kpi12 <- find_n(0.1,2,0.09,sim_data$dessert1)


Final_data_checking_n <- data.frame(served0 = kpi01, dessert0 = kpi02,
                                    served1 = kpi11, dessert0 = kpi12)
row.names(Final_data_checking_n) <- c('delta/mean','gama/gama+1','is delta/mean <= gama/gama+1 ?')

(Final_data_checking_n)
#as we can see , n=50 is good enough.

