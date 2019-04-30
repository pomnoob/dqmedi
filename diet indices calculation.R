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
fct_all <- dplyr::rename(fct_all,FOODCODE=Fcode)
write.csv(fct_all,"chns/fct_all.csv",row.names = F)

###########################################################################
###########################################################################
###########################################################################

## import chns data
nutr1 <- read.csv("chns/NUTR1_00.csv",stringsAsFactors = F)
nutr3 <- read.csv("chns/NUTR3_00.csv",stringsAsFactors = F)
fct <- read.csv("chns/fct_all.csv",stringsAsFactors = F)
fct_type <- read.csv("chns/fct type.csv",stringsAsFactors = F)
fct_type <- fct_type %>%
  dplyr::select(Foodcode,type) %>%
  dplyr::rename(FOODCODE=Foodcode)

fct <- left_join(fct,fct_type,by="FOODCODE")

###########################################################################
###########################################################################
###########################################################################
########################################################################## 
## Covariates
#macronutrients
macro_md <- read.csv("chns/c12diet.csv",stringsAsFactors = F)
macro_md.2004 <- macro_md %>% dplyr::filter(wave==2004) %>% dplyr::select(IDind,hhid,commid,d3kcal,d3carbo,d3fat,d3protn)

#gender
gender.all <- read.csv("chns/mast pub.csv",stringsAsFactors = F)
gender.all <- gender.all %>% 
  dplyr::select(Idind,GENDER) %>%
  dplyr::rename(gender=GENDER,IDind=Idind)

#Education
educ.all <- read.csv("chns/Educ 12.csv",stringsAsFactors = F)
educ.all <- educ.all %>%
  dplyr::select(IDind,hhid,A12) %>%
  dplyr::rename(educ=A12)

#Individual annual income
income.all <- read.csv("chns/indinc 12.csv",stringsAsFactors = F)
income.all <- income.all %>%
  dplyr::select(IDind,hhid,wave,indinc)

#Urbanization 
urban.all <- read.csv("chns/urban.csv",stringsAsFactors = F)
urban.all <- dplyr::select(urban.all,1:8)

#Age
age.all <- read.csv("chns/surveys pub 12.csv",stringsAsFactors = F)
age.all <- dplyr::select(age.all,Idind,hhid,wave,age)

#Biomarker
biom2009 <- read.csv("chns/biomarker.csv",stringsAsFactors = F)
biom2009 <- biom2009 %>%
  dplyr::select(IDind,HS_CRP,HDL_C,LDL_C,INS,HbA1c,GLUCOSE,TG,TC) %>%
  dplyr::mutate(homa_ir=INS*GLUCOSE/22.5,quicki=1/(log(INS)+log(GLUCOSE)))

#Physical exam
pe.all <- read.csv("chns/pexam 12.csv",stringsAsFactors = F)
pe.all <- pe.all %>%
  dplyr::select(IDind,hhid,WAVE,SYSTOL1,SYSTOL2,SYSTOL3,DIASTOL1,DIASTOL2,DIASTOL3,HEIGHT,WEIGHT,U9,U10,
                U24A,U24J,U24W,U24V,U25,U41,U56,U230) %>%
  dplyr::rename(hip_c=U9,waist_c=U10,diabetes=U24A,MI=U24J,stroke=U24V,
                cancer=U24W,smoke=U25,alcohol_d=U41,pregnant=U56,SSB_d=U230) %>%
  dplyr::mutate(bmi=WEIGHT/(HEIGHT/100)^2,hwr=waist_c/hip_c)

#Physical activity
pa.all <- read.csv("chns/pa.csv",stringsAsFactors = F)

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

cmfp.c <- left_join(macro_md.2004,fr2004.cmfp.md,by="IDind")
cmfp.c <- left_join(cmfp.c,fr2004.fs.md,by="IDind")
cmfp.c <- cmfp.c %>%
  dplyr::mutate(cmfp1=cmfp.c$"1"/(d3kcal/2000),
                cmfp2=cmfp.c$"2"/(d3kcal/2000),
                cmfp3=cmfp.c$"3"/(d3kcal/2000),
                cmfp4=cmfp.c$"4"/(d3kcal/2000),
                cmfp5=cmfp.c$"5"/(d3kcal/2000),
                cmfp6=cmfp.c$"6"/(d3kcal/2000),
                cmfp7=cmfp.c$"7"/(d3kcal/2000),
                cmfp8=cmfp.c$"8"/(d3kcal/2000),
                cmfp9=md_fo/(d3kcal/2000),
                cmfp10=md_salt/(d3kcal/2000)) %>%
  dplyr::mutate(score1=case_when(cmfp1>300~5,cmfp1<300~cmfp1*5/300),
                score2=case_when(cmfp2>400~5,cmfp2<400~cmfp2*5/400),
                score3=case_when(cmfp3>100~5,cmfp3<100~cmfp3*5/100),
                score4=case_when(cmfp4>300~5,cmfp4<300~cmfp4*5/300),
                score5=case_when(cmfp5>30~5,cmfp5<30~cmfp5*5/30),
                score6=case_when(cmfp6<=100~4,cmfp6>=150~0,cmfp6>100 & cmfp6<150~4-(cmfp6-100)*4/50),
                score7=case_when(cmfp7>50~3,cmfp7<50~cmfp7*3/50),
                score8=case_when(cmfp8<=50~3,cmfp8>=75~0,cmfp8>50 & cmfp8<75~3-(cmfp8-50)*3/25),
                score9=case_when(md_fo<=30~5,md_fo>=45~0,md_fo>30 & md_fo<45~5-(md_fo-30)*5/15),
                score10=case_when(md_salt<=6~5,md_salt>=9~0,md_salt>6 & md_salt<9~5-(md_salt-6)*5/3)) %>%
  dplyr::mutate(cmfp_score=select(.,score1:score10) %>%
                  rowSums(na.rm = T))

##DASH
fr2004.dash<- fr2004 %>% 
  dplyr::filter(dash!=0) %>% 
  dplyr::group_by(IDind,VD,dash) %>% 
  summarize(vd_amt=sum(V39))

fr2004.dash.md <- fr2004.dash %>%
  dplyr::group_by(IDind,dash) %>%
  summarize(md_amt=mean(vd_amt),na.rm=T)%>%
  spread(key=dash,value=md_amt,fill=0)

# For fats and oils
fr2004.f.md <- fr2004.fs.md %>%
  dplyr::select(IDind,md_fo)
fr2004.dash.md <- left_join(fr2004.dash.md,fr2004.f.md,by="IDind")

#For sodium
fr2004.na.md <- dplyr::select(fr2004.nu.md,IDind,md_na)
fr2004.dash.md <- left_join(fr2004.dash.md,fr2004.na.md,by="IDind")

#For SSB
ssb.2004 <- pe.all %>%
  dplyr::filter(WAVE==2004) %>%
  dplyr::select(IDind,SSB_d) %>%
  dplyr::mutate(ssb=case_when(SSB_d==1~7,SSB_d==2~4,SSB_d==3~2,SSB_d==4~0.5,SSB_d==5~0.1,SSB_d==9~NA_real_)) %>%
  dplyr::select(-SSB_d)
                
fr2004.dash.md <- left_join(fr2004.dash.md,ssb.2004,by="IDind")

#Serving size
dash.c <- left_join(macro_md.2004,fr2004.dash.md, by="IDind")


dash.c <-  dash.c %>%
  dplyr::mutate(dash1=(dash.c$"1"/(d3kcal/2000))/28,
                dash2=(dash.c$"2"/(d3kcal/2000))/80,
                dash3=(dash.c$"3"/(d3kcal/2000))/80,
                dash4=(dash.c$"4"/(d3kcal/2000))/245,
                dash5=(dash.c$"5"/(d3kcal/2000))*7/28,
                dash6=(dash.c$"6"/(d3kcal/2000))*7/50,
                dash7=(dash.c$"7"/(d3kcal/2000))*7/43,
                dash8=(dash.c$"8"/(d3kcal/2000))*7/28,
                dash9=(dash.c$"9"/(d3kcal/2000))*7/113,
                dash10=(md_fo/(d3kcal/2000))/15,
                dash11=(dash.c$"11"/(d3kcal/2000))*5/15) %>%
  dplyr::mutate(dash_mpfe=select(.,dash5:dash6) %>% rowSums(na.rm = T),
                dash_nsl=select(.,dash7:dash9) %>% rowSums(na.rm = T),
                dash_ss=select(.,dash11,ssb) %>% rowSums(na.rm = T)) %>%
  dplyr::mutate(score1=case_when(dash1>=6~10,dash1<6~dash1/6*10),
                score2=case_when(dash2>=4~10,dash2<4~dash2/4*10), 
                score3=case_when(dash3>=4~10,dash3<4~dash3/4*10),
                score4=case_when(dash4>=6~10,dash4<6~dash4/6*10),
                score5=case_when(dash_mpfe<=6~10,dash_mpfe>=9~0,dash_mpfe>6 & dash_mpfe<9~10-(dash_mpfe-6)/3*10),
                score6=case_when(dash_nsl>=4~10,dash_nsl<4~dash_nsl/4*10),
                score7=case_when(dash10<=3~10,dash10>=4.5~0,dash10>3 & dash10<4.5~10-(dash10-3)/1.5*10),
                score8=case_when(dash_ss<=5~10,dash_ss>=7~0,dash_ss>5 & dash_ss<7~10-(dash_ss-5)/2*10),
                score9=case_when(md_na<=1500~10,md_na>=2300~0,md_na>1500 & md_na<2300~10-(md_na-1500)/800*10)) %>%
    dplyr::mutate(dash_score=select(.,score1:score9) %>% rowSums(na.rm = T))
                                
  



##AHEI
fr2004.ahei<- fr2004 %>% 
  dplyr::filter(ahei!=0) %>% 
  dplyr::group_by(IDind,VD,ahei) %>% 
  summarize(vd_amt=sum(V39))

fr2004.ahei.md <- fr2004.ahei %>%
  dplyr::group_by(IDind,ahei) %>%
  summarize(md_amt=mean(vd_amt),na.rm=T)%>%
  spread(key=ahei,value=md_amt,fill=0)

fr2004.ahei.md <- left_join(fr2004.ahei.md,fr2004.nu.md,by="IDind")

