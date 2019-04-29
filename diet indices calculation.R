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
###########################################################################

## import chns data
nutr1 <- read.csv("chns/NUTR1_00.csv",stringsAsFactors = F)
nutr3 <- read.csv("chns/NUTR3_00.csv",stringsAsFactors = F)
fct <- read.csv("chns/fct_all.csv",stringsAsFactors = F)

###########################################################################
###########################################################################
###########################################################################

### Year 2004

fi2004 <- dplyr::filter(nutr1,WAVE==2004)
fr2004 <- dplyr::filter(nutr3,WAVE==2004)

fi2004 <- left_join(fi2004,fct,by="FOODCODE")
fr2004 <- left_join(fr2004,fct,by="FOODCODE")

## Eat from home
fr2004.home <- fr2004 %>% dplyr::filter(V43==1) %>% dplyr::filter(type==4|type==5|type==8|
                                                                  type==9|type==12)

## Intake of veg and meat by individual
fr2004.ind <- fr2004.home %>% dplyr::group_by(IDind,VD,hhid) %>% summarize(ind_amt=sum(V39))
## Intake of veg and meat by family
fr2004.fam <- fr2004.home %>% dplyr::group_by(hhid, VD) %>% summarize(fam_amt=sum(V39))
## Proportion
fr2004.pro <- left_join(fr2004.ind,fr2004.fam,by=c("hhid","VD"))
fr2004.pro <- dplyr::mutate(fr2004.pro,pro=ind_amt/fam_amt)

## Family food inventory file
fi2004.fats <- dplyr::filter(fi2004,dash==11)
fi2004.salt <- dplyr::filter(fi2004,cmfp==10)

fi2004.fats.amt <-fi2004.fats %>% dplyr::group_by(hhid,dash) %>% summarize(total_fats=sum(V22))
fi2004.salt.amt <-fi2004.salt %>% dplyr::group_by(hhid,dash) %>% summarize(total_salt=sum(V22))

fr2004.pro.fats <- left_join(fr2004.pro,fi2004.fats.amt,by="hhid")
fr2004.pro.fats <- dplyr::mutate(fr2004.pro.fats,fats=pro*total_fats)
fr2004.pro.salt <- left_join(fr2004.pro,fi2004.salt.amt,by="hhid")
fr2004.pro.salt <- dplyr::mutate(fr2004.pro.salt,salt=pro*total_salt)
fr2004.pro.salt2<- dplyr::select(fr2004.pro.salt,IDind,VD,salt)
fr2004.pro.fs <- left_join(fr2004.pro.fats,fr2004.pro.salt2,by=c("IDind","VD"))

fr2004.fs.md <- fr2004.pro.fs %>% dplyr::group_by(IDind) %>% summarize(md_fo=mean(fats),md_salt=mean(salt),na.rm=T)

###########################################################################
 
fr2004 <- fr2004 %>% dplyr::mutate(fg_fiber=(V39*edible)/100*(fiber/100),
                                   fg_na=(V39*edible)/100*(sodium/100),
                                   fg_pufa=(V39*edible)/100*(PUFA/100),
                                   fg_n3=(V39*edible)/100*((EPA+DHA)*Total/100))

##计算每个人钠、纤维素、pufa和n3的摄入量

fr2004.nu <- fr2004 %>% dplyr::group_by(IDind,VD) %>% summarize(fiber=sum(fg_fiber),
                                                                na=sum(fg_na),
                                                                pufa=sum(fg_pufa),
                                                                n3=sum(fg_n3),na.rm=T)

fr2004.nu.md <- fr2004.nu %>% dplyr::group_by(IDind) %>% summarize(md_fiber=mean(fiber),
                                                                   md_na=mean(na),
                                                                   md_pufa=mean(pufa),
                                                                   md_n3=mean(n3),na.rm=T) %>%
  dplyr::select(-na.rm)

##计算膳食质量各个组分的摄入量
#CMFP
fr2004.cmfp <- fr2004 %>% 
  dplyr::filter(cmfp!=0) %>% 
  dplyr::group_by(IDind,VD,cmfp) %>% 
  summarize(vd_amt=sum(V39))

fr2004.cmfp.md <- fr2004.cmfp %>%
  dplyr::group_by(IDind,cmfp) %>%
  summarize(md_amt=mean(vd_amt),na.rm=T)%>%
  spread(key=cmfp,value=md_amt,fill=0)

#DASH
fr2004.dash<- fr2004 %>% 
  dplyr::filter(dash!=0) %>% 
  dplyr::group_by(IDind,VD,dash) %>% 
  summarize(vd_amt=sum(V39))

fr2004.dash.md <- fr2004.dash %>%
  dplyr::group_by(IDind,dash) %>%
  summarize(md_amt=mean(vd_amt),na.rm=T)%>%
  spread(key=dash,value=md_amt,fill=0)

fr2004.s.md <- fr2004.fs.md %>%
  dplyr::select(IDind,md_salt)

fr2004.dash.md <- left_join(fr2004.dash.md,fr2004.s.md,by="IDind")

#AHEI
fr2004.ahei<- fr2004 %>% 
  dplyr::filter(ahei!=0) %>% 
  dplyr::group_by(IDind,VD,ahei) %>% 
  summarize(vd_amt=sum(V39))

fr2004.ahei.md <- fr2004.ahei %>%
  dplyr::group_by(IDind,ahei) %>%
  summarize(md_amt=mean(vd_amt),na.rm=T)%>%
  spread(key=ahei,value=md_amt,fill=0)

fr2004.ahei.md <- left_join(fr2004.ahei.md,fr2004.nu.md,by="IDind")
  



