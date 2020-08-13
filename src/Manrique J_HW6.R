rm(list=ls())
library(tidyverse)
data <- data.frame(h=0,a=0,u_ha=0,w_ha=0)
data <- data[0,]

data <-data %>% add_row(h=1,a=1,u_ha=2062.50,w_ha=275)
data <-data %>% add_row(h=1,a=2,u_ha=2062.50,w_ha=275)
data <-data %>% add_row(h=2,a=1,u_ha=1744.875,w_ha=258.5)
data <-data %>% add_row(h=2,a=2,u_ha=2392.500,w_ha=319.0)
data <-data %>% add_row(h=3,a=1,u_ha=1725.000,w_ha=230.0)
data <-data %>% add_row(h=3,a=2,u_ha=2100.000,w_ha=280.0)
data <-data %>% add_row(h=4,a=1,u_ha=1491.750,w_ha=229.5)
data <-data %>% add_row(h=4,a=2,u_ha=1164.375,w_ha=202.5)
data <-data %>% add_row(h=5,a=1,u_ha=2117.500,w_ha=302.5)
data <-data %>% add_row(h=5,a=2,u_ha=2272.875,w_ha=313.5)
data <-data %>% add_row(h=6,a=1,u_ha=3213.000,w_ha=459.0)
data <-data %>% add_row(h=6,a=2,u_ha=2010.250,w_ha=365.5)

ratio_mean = sum(data$u_ha)/sum(data$w_ha)

#### Taylor series approximation ####
var_u_ha <- 0
var_w_ha <- 0
cov_u_w_ha <- 0

for(i in 1:6){
  taylor <- data[data$h==i,]
  var_u_ha <- var_u_ha + (taylor$u_ha[1]-taylor$u_ha[2])**2
  var_w_ha <- var_w_ha + (taylor$w_ha[1]-taylor$w_ha[2])**2
  cov_u_w_ha <- cov_u_w_ha + (taylor$u_ha[1]-taylor$u_ha[2])*(taylor$w_ha[1]-taylor$w_ha[2])
}

rpta_var_taylor = (1/(sum(data$w_ha)**2))*(var_u_ha+(ratio_mean**2)*(var_w_ha)-2*ratio_mean*cov_u_w_ha); rpta_var_taylor
rpta_se_taylor = sqrt(rpta_var_taylor);rpta_se_taylor

### Confidence Interval Taylor
ratio_mean + (rpta_se_taylor*qt(1-0.05/2,6))
ratio_mean - (rpta_se_taylor*qt(1-0.05/2,6))

#### Balanced repeated replication ####
set.seed(13920)
#u/w
hadamard=matrix(nrow=8,ncol = 6,data = c(1,1,1,-1,1,-1,-1,1,1,1,-1,1,-1,-1,1,1,1,-1,1,1,-1,1,1,1,-1,1,-1,-1,1,1,1,-1,1,-1,-1,1,1,1,-1,1,1,-1,-1,-1,-1,-1,-1,-1),byrow = T)
hadamard[4,2]=-1
hadamard[7,5]=-1
value_hadamard_u = hadamard

for(i in 1:6){
  strata <- data[data$h==i,]
  row <- hadamard[,i]
  for(j in 1:8){
    element <- ifelse(row[j]==1,strata[1,"u_ha"],strata[2,"u_ha"])
    value_hadamard_u[j,i]=element
  }
}

value_hadamard_w = hadamard

for(i in 1:6){
  strata <- data[data$h==i,]
  row <- hadamard[,i]
  for(j in 1:8){
    element <- ifelse(row[j]==1,strata[1,"w_ha"],strata[2,"w_ha"])
    value_hadamard_w[j,i]=element
  }
}
value_hadamard_u= cbind(value_hadamard_u,rowSums(value_hadamard_u))
value_hadamard_u=cbind(value_hadamard_u,sum(data$u_ha)-rowSums(value_hadamard_u[,1:6]))

value_hadamard_w=cbind(value_hadamard_w,rowSums(value_hadamard_w))
value_hadamard_w=cbind(value_hadamard_w,sum(data$w_ha)-rowSums(value_hadamard_w[,1:6]))

hd_ratios = data.frame(z_y=0,z2_y=0)
hd_ratios = hd_ratios[0,]
for(i in 1:8){
  hd_ratios <- hd_ratios %>% add_row(z_y = value_hadamard_u[i,7]/value_hadamard_w[i,7],z2_y = value_hadamard_u[i,8]/value_hadamard_w[i,8])
}

var = vector()
for(k in 1:8){
  for(l in 1:2){
    pl = 0
    pl = pl + (hd_ratios[k,l]-ratio_mean)**2
  }
 var = append(var,pl) 
}
rpta_var_brr = 1/(2*8)*sum(var);rpta_var_brr
rpta_se_brr = sqrt(rpta_var_brr);rpta_se_brr

### Confidence Interval BRR
ratio_mean + (rpta_se_brr*qt(1-0.05/2,6))
ratio_mean - (rpta_se_brr*qt(1-0.05/2,6))

#### Jackknife repeated replication ####

jk_matrix_u = matrix(ncol = 6*2,nrow = 6)
jk_matrix_w = matrix(ncol = 6*2,nrow = 6)

for(p in 1:6){
  random <- round(runif(1,1,2))
  strata <- data[data$h==p,]
  if (random==1){
    jk_matrix_u[p,p*2-1]=0
    jk_matrix_u[p,p*2]=2*strata[2,"u_ha"]
    jk_matrix_u[,p*2-1] = replace_na(jk_matrix_u[,p*2-1],strata[1,"u_ha"])
    jk_matrix_u[,p*2] = replace_na(jk_matrix_u[,p*2],strata[2,"u_ha"])
    
    jk_matrix_w[p,p*2-1]=0
    jk_matrix_w[p,p*2]=2*strata[2,"w_ha"]
    jk_matrix_w[,p*2-1] = replace_na(jk_matrix_w[,p*2-1],strata[1,"w_ha"])
    jk_matrix_w[,p*2] = replace_na(jk_matrix_w[,p*2],strata[2,"w_ha"])
  }
  if (random == 2){
    jk_matrix_u[p,p*2-1]=0
    jk_matrix_u[p,p*2]=2*strata[1,"u_ha"]
    jk_matrix_u[,p*2-1] = replace_na(jk_matrix_u[,p*2-1],strata[2,"u_ha"])
    jk_matrix_u[,p*2] = replace_na(jk_matrix_u[,p*2],strata[1,"u_ha"])
    
    jk_matrix_w[p,p*2-1]=0
    jk_matrix_w[p,p*2]=2*strata[1,"w_ha"]
    jk_matrix_w[,p*2-1] = replace_na(jk_matrix_w[,p*2-1],strata[2,"w_ha"])
    jk_matrix_w[,p*2] = replace_na(jk_matrix_w[,p*2],strata[1,"w_ha"])
  }
}

jk_matrix_u = cbind(jk_matrix_u,rowSums(jk_matrix_u))
jk_matrix_w = cbind(jk_matrix_w,rowSums(jk_matrix_w))

jrr_var = 0
for (m in 1:6) {
  jrr_ratio = jk_matrix_u[m,13]/jk_matrix_w[m,13]
  jrr_var = jrr_var + (jrr_ratio - ratio_mean)**2
}

rpta_var_jrr = jrr_var
rpta_se_jrr = sqrt(rpta_var_jrr)

### Confidence Interval JRR
ratio_mean + (rpta_se_jrr*qt(1-0.05/2,6))
ratio_mean - (rpta_se_jrr*qt(1-0.05/2,6))
