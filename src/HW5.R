rm(list=ls())

library(readxl)
library(tidyverse)
hw5 <- read_excel("C:/Users/jaman/Dropbox/Sampling Program for Survey Statisticians/SurvMeth 625/HW5/SM 625 HW5 data summer 2020.xlsx")

### 1: sampling interval
k = max(hw5$`Cumulative Housing units:  Occupied`)/30
k

### 2:
random_no = 1225.6
select = vector()

for(i in 0:29){
  select[i+1]=trunc(random_no + i*k)
}

select_id = vector()

for(i in 1:30){
  mw <- hw5[hw5$`Cumulative Housing units:  Occupied`< select[i],]
  select_id[i]=max(mw$ID)
}

selected_frame <- hw5[select_id,]

ID_selection <-selected_frame$ID

## linking initialization
hw5$linked <- "no"

## linking

for(i in 1:length(ID_selection)){
  id <- hw5[ID_selection[i],]
  tag <- 1
  if(hw5[ID_selection[i],2] < 25){
    sum <- hw5[ID_selection[i],2]
    hw5[ID_selection[i],5] ="yes"
    
    j = ID_selection[i]
    while (sum < 25) {
      sum = sum + hw5[j+1,2]
      j = j+1
      hw5[j,5]="yes"
    }
  }
}

hw5[ID_selection,5]="yes"

final_selection <- hw5[hw5$linked=="yes",]

final_selection$cluster = 1
p <- 1

for(r in 1:dim(final_selection)[1]){
  final_selection[r,6] <-p
  if((final_selection[r+1,1]-final_selection[r,1]) > 2){
    p <- p + 1
  }
}

colnames(final_selection) <- c('id','mos','ba','cumtot','link','cluster')

x_a = final_selection %>% group_by(cluster) %>% summarise(MOS=sum(mos),BA=sum(ba)) %>% mutate(x_a=25/MOS*BA)

print(x_a,n=30)

options(max.print = 500000)
### 2
rm(list=ls())
mat2 <- data.frame()
mat2 <- rbind(mat2,c('005','Allegan',38165,3593))
mat2 <- rbind(mat2,c('077','Kalamazoo',93479,4494))
colnames(mat2) <- c('County','County name','OHU','Bloccs')
mat2$OHU = as.numeric(mat2$OHU)
mat2$Bloccs = as.numeric(mat2$Bloccs)
## a
mat2 <- mat2 %>% mutate(alloc = round(OHU/sum(OHU)*30,0))
print(mat2)

## b
mat2 <- mat2 %>% mutate(f=alloc*25/sum(OHU))
print(mat2)

sum(mat2$f)
1/sum(mat2$f)
## c
mat2 <- mat2 %>% mutate(real_b=OHU*25/(sum(OHU)/2))
print(mat2)
