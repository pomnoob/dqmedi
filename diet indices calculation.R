##首先需要手动输入钠含量，把盐摄入单独列出来。把脂肪酸含量数据加进去。

library(tidyverse)
fatty2002 <- read.csv("data/fatty2002.csv",stringsAsFactors = F)
fatty2002 <- fatty2002 %>% dplyr::select(-FD_name)
fatty2002[is.na(fatty2002)] <- 0

fatty2004 <- read.csv("data/fatty2004.csv",stringsAsFactors = F)
fatty2004 <- fatty2004 %>% dplyr::select(-FD_name)
fatty2004[is.na(fatty2004)] <- 0

fct2002 <- read.csv("data/fct2002.csv",stringsAsFactors = F)
fct2002 <- fct2002 %>% dplyr::select(-FD_name)
fct2002 <- left_join(fct2002,fatty2002,by="Fcode")
fct2002[is.na(fct2002)] <- 0

fct2004 <- read.csv("data/fct2004.csv",stringsAsFactors = F)
fct2004 <- fct2004 %>% dplyr::select(-FD_name)
fct2004 <- left_join(fct2004,fatty2004,by="Fcode")
fct2004[is.na(fct2004)] <- 0

fct_all <- rbind(fct2002,fct2004)
fct_all <- dplyr::rename(fct_all,foodcode=Fcode)
write.csv(fct_all,"chns/fct_all.csv",row.names = F)

###########################################################################
###########################################################################

## import chns data
nutr1 <- read.csv("chns/NUTR1_00.csv",stringsAsFactors = F)
nutr3 <- read.csv("chns/NUTR3_00.csv",stringsAsFactors = F)
fct <- read.csv("chns/fct_all.csv",stringsAsFactors = F)

###########################################################################

### Year 2004

fi2004 <- dplyr::filter(nutr1,WAVE==2004)
fr2004 <- dplyr::filter(nutr3,WAVE==2004)

fi2004 <- left_join(fi2004,fct,by="FOODCODE")
fr2004 <- left_join(fr2004,fct,by="FOODCODE")

## Eat from home
fr2004.home <- fr2004 %>% dplyr::filter(V43==1) %>% dplyr::filter(type==4|type==5|type==8|
                                                                  type==9|type==12)

## Intake of veg and meat by interview day
fr2004.pro.day <- fr2004.home %>% dplyr::group_by(IDind,VD,hhid) %>% summarize(new_amt=sum(V39))

