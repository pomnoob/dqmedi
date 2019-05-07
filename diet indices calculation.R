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
library(tidyverse)
nutr1 <- read.csv("chns/NUTR1_00.csv",stringsAsFactors = F)
nutr3 <- read.csv("chns/NUTR3_00.csv",stringsAsFactors = F)
fct <- read.csv("chns/fct_all.csv",stringsAsFactors = F)

##No Need to run this on XPS13
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


#gender
gender.all <- read.csv("chns/mast pub.csv",stringsAsFactors = F)
gender.all <- gender.all %>% 
  dplyr::select(Idind,GENDER) %>%
  dplyr::rename(gender=GENDER,IDind=Idind)

#Education
educ.all <- read.csv("chns/Educ 12.csv",stringsAsFactors = F)
educ.all <- educ.all %>%
  dplyr::select(IDind,hhid,WAVE,A12) %>%
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
  dplyr::mutate(bmi=WEIGHT/(HEIGHT/100)^2,hwr=waist_c/hip_c)%>%
  dplyr::mutate(diastol=dplyr::select(.,DIASTOL1,DIASTOL2,DIASTOL3) %>% rowMeans(na.rm = T),
                systol=dplyr::select(.,SYSTOL1,SYSTOL2,SYSTOL3) %>% rowMeans(na.rm = T))
pe.all[pe.all=="NaN"] <- NA

#Physical activity
pa.all <- read.csv("chns/pa.csv",stringsAsFactors = F)

########################################################################### 

### Year 2004
macro_md.2004 <- macro_md %>% dplyr::filter(wave==2004) %>% dplyr::select(IDind,hhid,commid,d3kcal,d3carbo,d3fat,d3protn)
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

cmfp.c.2004 <- left_join(macro_md.2004,fr2004.cmfp.md,by="IDind")
cmfp.c.2004 <- left_join(cmfp.c.2004,fr2004.fs.md,by="IDind")
cmfp.c.2004 <- cmfp.c.2004 %>%
  dplyr::mutate(cmfp1=cmfp.c.2004$"1"/(d3kcal/2000),
                cmfp2=cmfp.c.2004$"2"/(d3kcal/2000),
                cmfp3=cmfp.c.2004$"3"/(d3kcal/2000),
                cmfp4=cmfp.c.2004$"4"/(d3kcal/2000),
                cmfp5=cmfp.c.2004$"5"/(d3kcal/2000),
                cmfp6=cmfp.c.2004$"6"/(d3kcal/2000),
                cmfp7=cmfp.c.2004$"7"/(d3kcal/2000),
                cmfp8=cmfp.c.2004$"8"/(d3kcal/2000),
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
dash.c.2004 <- left_join(macro_md.2004,fr2004.dash.md, by="IDind")


dash.c.2004 <-  dash.c.2004 %>%
  dplyr::mutate(dash1=(dash.c.2004$"1"/(d3kcal/2000))/28,
                dash2=(dash.c.2004$"2"/(d3kcal/2000))/80,
                dash3=(dash.c.2004$"3"/(d3kcal/2000))/80,
                dash4=(dash.c.2004$"4"/(d3kcal/2000))/245,
                dash5=(dash.c.2004$"5"/(d3kcal/2000))*7/28,
                dash6=(dash.c.2004$"6"/(d3kcal/2000))*7/50,
                dash7=(dash.c.2004$"7"/(d3kcal/2000))*7/43,
                dash8=(dash.c.2004$"8"/(d3kcal/2000))*7/28,
                dash9=(dash.c.2004$"9"/(d3kcal/2000))*7/113,
                dash10=(md_fo/(d3kcal/2000))/15,
                dash11=(dash.c.2004$"11"/(d3kcal/2000))*5/15) %>%
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

#For SSB and alcohol from questionaire
sh.2004 <- pe.all %>%
  dplyr::filter(WAVE==2004) %>%
  dplyr::select(IDind,SSB_d,alcohol_d)
fr2004.ahei.md <- left_join(fr2004.ahei.md,sh.2004,by="IDind")

#Gender and macro
fr2004.ahei.md <- left_join(fr2004.ahei.md,gender.all,by="IDind")
ahei.c.2004 <- left_join(fr2004.ahei.md,macro_md.2004,by="IDind")

#Score for alcohol
ahei.c.2004 <- ahei.c.2004 %>%
  dplyr::mutate(d51=`51`/(d3kcal/2000)/(236*4),d52=`52`/(d3kcal/2000)/(236*12),
                d53=`53`/(d3kcal/2000)/(236*1.5)) %>%
  dplyr::mutate(s_alcohol=d51+d52+d53) %>%
  dplyr::mutate(score_alcohol=case_when(gender==1 & s_alcohol>=3.5~0,gender==2 & s_alcohol>=2.5~0,s_alcohol<0.5~2.5,
                                        gender==1 & s_alcohol>=0.5 & s_alcohol<=2.0~10, gender==2 & s_alcohol>=0.5 & s_alcohol<=1.5~10,
                                        gender==1 & s_alcohol>2.0 & s_alcohol<3.5~10-(s_alcohol-2.0)*(10/1.5),
                                        gender==2 & s_alcohol>1.5 & s_alcohol<2.5~10-(s_alcohol-1.5)*(10/1)),na.rm=T)

#Score for SSB
ahei.c.2004 <- ahei.c.2004 %>%
  dplyr::mutate(score_ssb=case_when(SSB_d==1~0,SSB_d==2~4,SSB_d==3~6,SSB_d==4~8,SSB_d==5|SSB_d==9 | is.na(SSB_d)~10))

#Score for pufa
ahei.c.2004 <- ahei.c.2004 %>%
  dplyr::mutate(pufa_p=md_pufa*9/d3kcal*100,na.rm=T) %>%
  dplyr::mutate(score_pufa=case_when(pufa_p>=10~10,pufa_p<=2~0,pufa_p<10 & pufa_p>2~(pufa_p-2)*(10/8)),na.rm=T)

#Score for sodium
ahei.c.2004$md_na[is.na(ahei.c.2004$md_na)] <- round(mean(ahei.c.2004$md_na,na.rm = T))
ahei.c.2004$score_na <- ntile(desc(ahei.c.2004$md_na),10)

#others

ahei.c.2004 <- ahei.c.2004 %>%
  dplyr::mutate(score_veg=case_when(`1`==0~0,`1`>=591~10,`1`>0 & `1`<591~`1`*10/591),
               score_fruit=case_when(`2`==0~0,`2`>=473~10,`2`>0 & `2`<473~`2`*10/473),
               score_fiber=case_when(md_fiber==0~0,md_fiber>=15~10,md_fiber>0 & md_fiber<15~md_fiber*10/15),
               score_nuts=case_when(`3`==0~0,`3`>=28~10,`3`>0 & `3`<28~`3`*10/28),
               score_n3=case_when(md_n3==0~0,md_n3>=250~10,md_n3>0 & md_n3<250~md_n3*10/250),
               score_meat=case_when(`4`==0~10,`4`>=170~0,`4`>0 & `4`<170~10-(`4`*10/170))) %>%
  dplyr::mutate(ahei_score=score_alcohol+score_ssb+score_na+score_pufa+score_veg+score_fruit+score_fiber+score_nuts+score_n3+score_meat)

#diet quality 2004
cmfp.c.c2004 <- dplyr::select(cmfp.c.2004,IDind,cmfp_score)
dash.c.c2004 <- dplyr::select(dash.c.2004,IDind,dash_score)
ahei.c.c2004 <- dplyr::select(ahei.c.2004,IDind,ahei_score)

dq2004 <- left_join(cmfp.c.c2004,dash.c.c2004,by="IDind")
dq2004 <- left_join(dq2004,ahei.c.c2004,by="IDind")


###########################################################################
###########################################################################
###########################################################################
### Year 2006
macro_md.2006 <- macro_md %>% dplyr::filter(wave==2006) %>% dplyr::select(IDind,hhid,commid,d3kcal,d3carbo,d3fat,d3protn)
fi2006 <- dplyr::filter(nutr1,WAVE==2006)
fr2006 <- dplyr::filter(nutr3,WAVE==2006)

fi2006 <- left_join(fi2006,fct,by="FOODCODE")
fr2006 <- left_join(fr2006,fct,by="FOODCODE")

## Eat from home
fr2006.home <- fr2006 %>% dplyr::filter(V43==1) %>% dplyr::filter(type==4|type==5|type==8|
                                                                    type==9|type==12)

## Intake of veg and meat by individual
fr2006.ind <- fr2006.home %>% dplyr::group_by(IDind,VD,hhid) %>% summarize(ind_amt=sum(V39))
## Intake of veg and meat by family
fr2006.fam <- fr2006.home %>% dplyr::group_by(hhid, VD) %>% summarize(fam_amt=sum(V39))
## Proportion
fr2006.pro <- left_join(fr2006.ind,fr2006.fam,by=c("hhid","VD"))
fr2006.pro <- dplyr::mutate(fr2006.pro,pro=ind_amt/fam_amt)

## Family food inventory file
fi2006.fats <- dplyr::filter(fi2006,dash==11)
fi2006.salt <- dplyr::filter(fi2006,cmfp==10)

fi2006.fats.amt <-fi2006.fats %>% dplyr::group_by(hhid,dash) %>% summarize(total_fats=sum(V22))
fi2006.salt.amt <-fi2006.salt %>% dplyr::group_by(hhid,dash) %>% summarize(total_salt=sum(V22))

fr2006.pro.fats <- left_join(fr2006.pro,fi2006.fats.amt,by="hhid")
fr2006.pro.fats <- dplyr::mutate(fr2006.pro.fats,fats=pro*total_fats)
fr2006.pro.salt <- left_join(fr2006.pro,fi2006.salt.amt,by="hhid")
fr2006.pro.salt <- dplyr::mutate(fr2006.pro.salt,salt=pro*total_salt)
fr2006.pro.salt2<- dplyr::select(fr2006.pro.salt,IDind,VD,salt)
fr2006.pro.fs <- left_join(fr2006.pro.fats,fr2006.pro.salt2,by=c("IDind","VD"))

fr2006.fs.md <- fr2006.pro.fs %>% dplyr::group_by(IDind) %>% summarize(md_fo=mean(fats),md_salt=mean(salt),na.rm=T)

###########################################################################

fr2006 <- fr2006 %>% dplyr::mutate(fg_fiber=(V39*edible)/100*(fiber/100),
                                   fg_na=(V39*edible)/100*(sodium/100),
                                   fg_pufa=(V39*edible)/100*(PUFA/100),
                                   fg_n3=(V39*edible)/100*((EPA+DHA)*Total/100))

##计算每个人钠、纤维素、pufa和n3的摄入量

fr2006.nu <- fr2006 %>% dplyr::group_by(IDind,VD) %>% summarize(fiber=sum(fg_fiber),
                                                                na=sum(fg_na),
                                                                pufa=sum(fg_pufa),
                                                                n3=sum(fg_n3),na.rm=T)

fr2006.nu.md <- fr2006.nu %>% dplyr::group_by(IDind) %>% summarize(md_fiber=mean(fiber),
                                                                   md_na=mean(na),
                                                                   md_pufa=mean(pufa),
                                                                   md_n3=mean(n3),na.rm=T) %>%
  dplyr::select(-na.rm)

##计算膳食质量各个组分的摄入量
#CMFP
fr2006.cmfp <- fr2006 %>% 
  dplyr::filter(cmfp!=0) %>% 
  dplyr::group_by(IDind,VD,cmfp) %>% 
  summarize(vd_amt=sum(V39))

fr2006.cmfp.md <- fr2006.cmfp %>%
  dplyr::group_by(IDind,cmfp) %>%
  summarize(md_amt=mean(vd_amt),na.rm=T)%>%
  spread(key=cmfp,value=md_amt,fill=0)

cmfp.c.2006 <- left_join(macro_md.2006,fr2006.cmfp.md,by="IDind")
cmfp.c.2006 <- left_join(cmfp.c.2006,fr2006.fs.md,by="IDind")
cmfp.c.2006 <- cmfp.c.2006 %>%
  dplyr::mutate(cmfp1=cmfp.c.2006$"1"/(d3kcal/2000),
                cmfp2=cmfp.c.2006$"2"/(d3kcal/2000),
                cmfp3=cmfp.c.2006$"3"/(d3kcal/2000),
                cmfp4=cmfp.c.2006$"4"/(d3kcal/2000),
                cmfp5=cmfp.c.2006$"5"/(d3kcal/2000),
                cmfp6=cmfp.c.2006$"6"/(d3kcal/2000),
                cmfp7=cmfp.c.2006$"7"/(d3kcal/2000),
                cmfp8=cmfp.c.2006$"8"/(d3kcal/2000),
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
fr2006.dash<- fr2006 %>% 
  dplyr::filter(dash!=0) %>% 
  dplyr::group_by(IDind,VD,dash) %>% 
  summarize(vd_amt=sum(V39))

fr2006.dash.md <- fr2006.dash %>%
  dplyr::group_by(IDind,dash) %>%
  summarize(md_amt=mean(vd_amt),na.rm=T)%>%
  spread(key=dash,value=md_amt,fill=0)

# For fats and oils
fr2006.f.md <- fr2006.fs.md %>%
  dplyr::select(IDind,md_fo)
fr2006.dash.md <- left_join(fr2006.dash.md,fr2006.f.md,by="IDind")

#For sodium
fr2006.na.md <- dplyr::select(fr2006.nu.md,IDind,md_na)
fr2006.dash.md <- left_join(fr2006.dash.md,fr2006.na.md,by="IDind")

#For SSB
ssb.2006 <- pe.all %>%
  dplyr::filter(WAVE==2006) %>%
  dplyr::select(IDind,SSB_d) %>%
  dplyr::mutate(ssb=case_when(SSB_d==1~7,SSB_d==2~4,SSB_d==3~2,SSB_d==4~0.5,SSB_d==5~0.1,SSB_d==9~NA_real_)) %>%
  dplyr::select(-SSB_d)

fr2006.dash.md <- left_join(fr2006.dash.md,ssb.2006,by="IDind")

#Serving size
dash.c.2006 <- left_join(macro_md.2006,fr2006.dash.md, by="IDind")


dash.c.2006 <-  dash.c.2006 %>%
  dplyr::mutate(dash1=(dash.c.2006$"1"/(d3kcal/2000))/28,
                dash2=(dash.c.2006$"2"/(d3kcal/2000))/80,
                dash3=(dash.c.2006$"3"/(d3kcal/2000))/80,
                dash4=(dash.c.2006$"4"/(d3kcal/2000))/245,
                dash5=(dash.c.2006$"5"/(d3kcal/2000))*7/28,
                dash6=(dash.c.2006$"6"/(d3kcal/2000))*7/50,
                dash7=(dash.c.2006$"7"/(d3kcal/2000))*7/43,
                dash8=(dash.c.2006$"8"/(d3kcal/2000))*7/28,
                dash9=(dash.c.2006$"9"/(d3kcal/2000))*7/113,
                dash10=(md_fo/(d3kcal/2000))/15,
                dash11=(dash.c.2006$"11"/(d3kcal/2000))*5/15) %>%
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
fr2006.ahei<- fr2006 %>% 
  dplyr::filter(ahei!=0) %>% 
  dplyr::group_by(IDind,VD,ahei) %>% 
  summarize(vd_amt=sum(V39))

fr2006.ahei.md <- fr2006.ahei %>%
  dplyr::group_by(IDind,ahei) %>%
  summarize(md_amt=mean(vd_amt),na.rm=T)%>%
  spread(key=ahei,value=md_amt,fill=0)

fr2006.ahei.md <- left_join(fr2006.ahei.md,fr2006.nu.md,by="IDind")

#For SSB and alcohol from questionaire
sh.2006 <- pe.all %>%
  dplyr::filter(WAVE==2006) %>%
  dplyr::select(IDind,SSB_d,alcohol_d)
fr2006.ahei.md <- left_join(fr2006.ahei.md,sh.2006,by="IDind")

#Gender and macro
fr2006.ahei.md <- left_join(fr2006.ahei.md,gender.all,by="IDind")
ahei.c.2006 <- left_join(fr2006.ahei.md,macro_md.2006,by="IDind")

#Score for alcohol
ahei.c.2006 <- ahei.c.2006 %>%
  dplyr::mutate(d51=`51`/(d3kcal/2000)/(236*4),d52=`52`/(d3kcal/2000)/(236*12),
                d53=`53`/(d3kcal/2000)/(236*1.5)) %>%
  dplyr::mutate(s_alcohol=d51+d52+d53) %>%
  dplyr::mutate(score_alcohol=case_when(gender==1 & s_alcohol>=3.5~0,gender==2 & s_alcohol>=2.5~0,s_alcohol<0.5~2.5,
                                        gender==1 & s_alcohol>=0.5 & s_alcohol<=2.0~10, gender==2 & s_alcohol>=0.5 & s_alcohol<=1.5~10,
                                        gender==1 & s_alcohol>2.0 & s_alcohol<3.5~10-(s_alcohol-2.0)*(10/1.5),
                                        gender==2 & s_alcohol>1.5 & s_alcohol<2.5~10-(s_alcohol-1.5)*(10/1)),na.rm=T)

#Score for SSB
ahei.c.2006 <- ahei.c.2006 %>%
  dplyr::mutate(score_ssb=case_when(SSB_d==1~0,SSB_d==2~4,SSB_d==3~6,SSB_d==4~8,SSB_d==5|SSB_d==9 | is.na(SSB_d)~10))

#Score for pufa
ahei.c.2006 <- ahei.c.2006 %>%
  dplyr::mutate(pufa_p=md_pufa*9/d3kcal*100,na.rm=T) %>%
  dplyr::mutate(score_pufa=case_when(pufa_p>=10~10,pufa_p<=2~0,pufa_p<10 & pufa_p>2~(pufa_p-2)*(10/8)),na.rm=T)

#Score for sodium
ahei.c.2006$md_na[is.na(ahei.c.2006$md_na)] <- round(mean(ahei.c.2006$md_na,na.rm = T))
ahei.c.2006$score_na <- ntile(desc(ahei.c.2006$md_na),10)

#others

ahei.c.2006 <- ahei.c.2006 %>%
  dplyr::mutate(score_veg=case_when(`1`==0~0,`1`>=591~10,`1`>0 & `1`<591~`1`*10/591),
                score_fruit=case_when(`2`==0~0,`2`>=473~10,`2`>0 & `2`<473~`2`*10/473),
                score_fiber=case_when(md_fiber==0~0,md_fiber>=15~10,md_fiber>0 & md_fiber<15~md_fiber*10/15),
                score_nuts=case_when(`3`==0~0,`3`>=28~10,`3`>0 & `3`<28~`3`*10/28),
                score_n3=case_when(md_n3==0~0,md_n3>=250~10,md_n3>0 & md_n3<250~md_n3*10/250),
                score_meat=case_when(`4`==0~10,`4`>=170~0,`4`>0 & `4`<170~10-(`4`*10/170))) %>%
  dplyr::mutate(ahei_score=score_alcohol+score_ssb+score_na+score_pufa+score_veg+score_fruit+score_fiber+score_nuts+score_n3+score_meat)

#diet quality 2006
cmfp.c.c2006 <- dplyr::select(cmfp.c.2006,IDind,cmfp_score)
dash.c.c2006 <- dplyr::select(dash.c.2006,IDind,dash_score)
ahei.c.c2006 <- dplyr::select(ahei.c.2006,IDind,ahei_score)

dq2006 <- left_join(cmfp.c.c2006,dash.c.c2006,by="IDind")
dq2006 <- left_join(dq2006,ahei.c.c2006,by="IDind")

###########################################################################
###########################################################################
###########################################################################
### Year 2009
macro_md.2009 <- macro_md %>% dplyr::filter(wave==2009) %>% dplyr::select(IDind,hhid,commid,d3kcal,d3carbo,d3fat,d3protn)
fi2009 <- dplyr::filter(nutr1,WAVE==2009)
fr2009 <- dplyr::filter(nutr3,WAVE==2009)

fi2009 <- left_join(fi2009,fct,by="FOODCODE")
fr2009 <- left_join(fr2009,fct,by="FOODCODE")

## Eat from home
fr2009.home <- fr2009 %>% dplyr::filter(V43==1) %>% dplyr::filter(type==4|type==5|type==8|
                                                                    type==9|type==12)

## Intake of veg and meat by individual
fr2009.ind <- fr2009.home %>% dplyr::group_by(IDind,VD,hhid) %>% summarize(ind_amt=sum(V39))
## Intake of veg and meat by family
fr2009.fam <- fr2009.home %>% dplyr::group_by(hhid, VD) %>% summarize(fam_amt=sum(V39))
## Proportion
fr2009.pro <- left_join(fr2009.ind,fr2009.fam,by=c("hhid","VD"))
fr2009.pro <- dplyr::mutate(fr2009.pro,pro=ind_amt/fam_amt)

## Family food inventory file
fi2009.fats <- dplyr::filter(fi2009,dash==11)
fi2009.salt <- dplyr::filter(fi2009,cmfp==10)

fi2009.fats.amt <-fi2009.fats %>% dplyr::group_by(hhid,dash) %>% summarize(total_fats=sum(V22))
fi2009.salt.amt <-fi2009.salt %>% dplyr::group_by(hhid,dash) %>% summarize(total_salt=sum(V22))

fr2009.pro.fats <- left_join(fr2009.pro,fi2009.fats.amt,by="hhid")
fr2009.pro.fats <- dplyr::mutate(fr2009.pro.fats,fats=pro*total_fats)
fr2009.pro.salt <- left_join(fr2009.pro,fi2009.salt.amt,by="hhid")
fr2009.pro.salt <- dplyr::mutate(fr2009.pro.salt,salt=pro*total_salt)
fr2009.pro.salt2<- dplyr::select(fr2009.pro.salt,IDind,VD,salt)
fr2009.pro.fs <- left_join(fr2009.pro.fats,fr2009.pro.salt2,by=c("IDind","VD"))

fr2009.fs.md <- fr2009.pro.fs %>% dplyr::group_by(IDind) %>% summarize(md_fo=mean(fats),md_salt=mean(salt),na.rm=T)

###########################################################################

fr2009 <- fr2009 %>% dplyr::mutate(fg_fiber=(V39*edible)/100*(fiber/100),
                                   fg_na=(V39*edible)/100*(sodium/100),
                                   fg_pufa=(V39*edible)/100*(PUFA/100),
                                   fg_n3=(V39*edible)/100*((EPA+DHA)*Total/100))

##计算每个人钠、纤维素、pufa和n3的摄入量

fr2009.nu <- fr2009 %>% dplyr::group_by(IDind,VD) %>% summarize(fiber=sum(fg_fiber),
                                                                na=sum(fg_na),
                                                                pufa=sum(fg_pufa),
                                                                n3=sum(fg_n3),na.rm=T)

fr2009.nu.md <- fr2009.nu %>% dplyr::group_by(IDind) %>% summarize(md_fiber=mean(fiber),
                                                                   md_na=mean(na),
                                                                   md_pufa=mean(pufa),
                                                                   md_n3=mean(n3),na.rm=T) %>%
  dplyr::select(-na.rm)

##计算膳食质量各个组分的摄入量
#CMFP
fr2009.cmfp <- fr2009 %>% 
  dplyr::filter(cmfp!=0) %>% 
  dplyr::group_by(IDind,VD,cmfp) %>% 
  summarize(vd_amt=sum(V39))

fr2009.cmfp.md <- fr2009.cmfp %>%
  dplyr::group_by(IDind,cmfp) %>%
  summarize(md_amt=mean(vd_amt),na.rm=T)%>%
  spread(key=cmfp,value=md_amt,fill=0)

cmfp.c.2009 <- left_join(macro_md.2009,fr2009.cmfp.md,by="IDind")
cmfp.c.2009 <- left_join(cmfp.c.2009,fr2009.fs.md,by="IDind")
cmfp.c.2009 <- cmfp.c.2009 %>%
  dplyr::mutate(cmfp1=cmfp.c.2009$"1"/(d3kcal/2000),
                cmfp2=cmfp.c.2009$"2"/(d3kcal/2000),
                cmfp3=cmfp.c.2009$"3"/(d3kcal/2000),
                cmfp4=cmfp.c.2009$"4"/(d3kcal/2000),
                cmfp5=cmfp.c.2009$"5"/(d3kcal/2000),
                cmfp6=cmfp.c.2009$"6"/(d3kcal/2000),
                cmfp7=cmfp.c.2009$"7"/(d3kcal/2000),
                cmfp8=cmfp.c.2009$"8"/(d3kcal/2000),
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
fr2009.dash<- fr2009 %>% 
  dplyr::filter(dash!=0) %>% 
  dplyr::group_by(IDind,VD,dash) %>% 
  summarize(vd_amt=sum(V39))

fr2009.dash.md <- fr2009.dash %>%
  dplyr::group_by(IDind,dash) %>%
  summarize(md_amt=mean(vd_amt),na.rm=T)%>%
  spread(key=dash,value=md_amt,fill=0)

# For fats and oils
fr2009.f.md <- fr2009.fs.md %>%
  dplyr::select(IDind,md_fo)
fr2009.dash.md <- left_join(fr2009.dash.md,fr2009.f.md,by="IDind")

#For sodium
fr2009.na.md <- dplyr::select(fr2009.nu.md,IDind,md_na)
fr2009.dash.md <- left_join(fr2009.dash.md,fr2009.na.md,by="IDind")

#For SSB
ssb.2009 <- pe.all %>%
  dplyr::filter(WAVE==2009) %>%
  dplyr::select(IDind,SSB_d) %>%
  dplyr::mutate(ssb=case_when(SSB_d==1~7,SSB_d==2~4,SSB_d==3~2,SSB_d==4~0.5,SSB_d==5~0.1,SSB_d==9~NA_real_)) %>%
  dplyr::select(-SSB_d)

fr2009.dash.md <- left_join(fr2009.dash.md,ssb.2009,by="IDind")

#Serving size
dash.c.2009 <- left_join(macro_md.2009,fr2009.dash.md, by="IDind")


dash.c.2009 <-  dash.c.2009 %>%
  dplyr::mutate(dash1=(dash.c.2009$"1"/(d3kcal/2000))/28,
                dash2=(dash.c.2009$"2"/(d3kcal/2000))/80,
                dash3=(dash.c.2009$"3"/(d3kcal/2000))/80,
                dash4=(dash.c.2009$"4"/(d3kcal/2000))/245,
                dash5=(dash.c.2009$"5"/(d3kcal/2000))*7/28,
                dash6=(dash.c.2009$"6"/(d3kcal/2000))*7/50,
                dash7=(dash.c.2009$"7"/(d3kcal/2000))*7/43,
                dash8=(dash.c.2009$"8"/(d3kcal/2000))*7/28,
                dash9=(dash.c.2009$"9"/(d3kcal/2000))*7/113,
                dash10=(md_fo/(d3kcal/2000))/15,
                dash11=(dash.c.2009$"11"/(d3kcal/2000))*5/15) %>%
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
fr2009.ahei<- fr2009 %>% 
  dplyr::filter(ahei!=0) %>% 
  dplyr::group_by(IDind,VD,ahei) %>% 
  summarize(vd_amt=sum(V39))

fr2009.ahei.md <- fr2009.ahei %>%
  dplyr::group_by(IDind,ahei) %>%
  summarize(md_amt=mean(vd_amt),na.rm=T)%>%
  spread(key=ahei,value=md_amt,fill=0)

fr2009.ahei.md <- left_join(fr2009.ahei.md,fr2009.nu.md,by="IDind")

#For SSB and alcohol from questionaire
sh.2009 <- pe.all %>%
  dplyr::filter(WAVE==2009) %>%
  dplyr::select(IDind,SSB_d,alcohol_d)
fr2009.ahei.md <- left_join(fr2009.ahei.md,sh.2009,by="IDind")

#Gender and macro
fr2009.ahei.md <- left_join(fr2009.ahei.md,gender.all,by="IDind")
ahei.c.2009 <- left_join(fr2009.ahei.md,macro_md.2009,by="IDind")

#Score for alcohol
ahei.c.2009 <- ahei.c.2009 %>%
  dplyr::mutate(d51=`51`/(d3kcal/2000)/(236*4),d52=`52`/(d3kcal/2000)/(236*12),
                d53=`53`/(d3kcal/2000)/(236*1.5)) %>%
  dplyr::mutate(s_alcohol=d51+d52+d53) %>%
  dplyr::mutate(score_alcohol=case_when(gender==1 & s_alcohol>=3.5~0,gender==2 & s_alcohol>=2.5~0,s_alcohol<0.5~2.5,
                                        gender==1 & s_alcohol>=0.5 & s_alcohol<=2.0~10, gender==2 & s_alcohol>=0.5 & s_alcohol<=1.5~10,
                                        gender==1 & s_alcohol>2.0 & s_alcohol<3.5~10-(s_alcohol-2.0)*(10/1.5),
                                        gender==2 & s_alcohol>1.5 & s_alcohol<2.5~10-(s_alcohol-1.5)*(10/1)),na.rm=T)

#Score for SSB
ahei.c.2009 <- ahei.c.2009 %>%
  dplyr::mutate(score_ssb=case_when(SSB_d==1~0,SSB_d==2~4,SSB_d==3~6,SSB_d==4~8,SSB_d==5|SSB_d==9 | is.na(SSB_d)~10))

#Score for pufa
ahei.c.2009 <- ahei.c.2009 %>%
  dplyr::mutate(pufa_p=md_pufa*9/d3kcal*100,na.rm=T) %>%
  dplyr::mutate(score_pufa=case_when(pufa_p>=10~10,pufa_p<=2~0,pufa_p<10 & pufa_p>2~(pufa_p-2)*(10/8)),na.rm=T)

#Score for sodium
ahei.c.2009$md_na[is.na(ahei.c.2009$md_na)] <- round(mean(ahei.c.2009$md_na,na.rm = T))
ahei.c.2009$score_na <- ntile(desc(ahei.c.2009$md_na),10)

#others

ahei.c.2009 <- ahei.c.2009 %>%
  dplyr::mutate(score_veg=case_when(`1`==0~0,`1`>=591~10,`1`>0 & `1`<591~`1`*10/591),
                score_fruit=case_when(`2`==0~0,`2`>=473~10,`2`>0 & `2`<473~`2`*10/473),
                score_fiber=case_when(md_fiber==0~0,md_fiber>=15~10,md_fiber>0 & md_fiber<15~md_fiber*10/15),
                score_nuts=case_when(`3`==0~0,`3`>=28~10,`3`>0 & `3`<28~`3`*10/28),
                score_n3=case_when(md_n3==0~0,md_n3>=250~10,md_n3>0 & md_n3<250~md_n3*10/250),
                score_meat=case_when(`4`==0~10,`4`>=170~0,`4`>0 & `4`<170~10-(`4`*10/170))) %>%
  dplyr::mutate(ahei_score=score_alcohol+score_ssb+score_na+score_pufa+score_veg+score_fruit+score_fiber+score_nuts+score_n3+score_meat)

#diet quality 2009
cmfp.c.c2009 <- dplyr::select(cmfp.c.2009,IDind,cmfp_score)
dash.c.c2009 <- dplyr::select(dash.c.2009,IDind,dash_score)
ahei.c.c2009 <- dplyr::select(ahei.c.2009,IDind,ahei_score)

dq2009 <- left_join(cmfp.c.c2009,dash.c.c2009,by="IDind")
dq2009 <- left_join(dq2009,ahei.c.c2009,by="IDind")


###########################################################################
###########################################################################
###########################################################################
### Year 2011
macro_md.2011 <- macro_md %>% dplyr::filter(wave==2011) %>% dplyr::select(IDind,hhid,commid,d3kcal,d3carbo,d3fat,d3protn)
fi2011 <- dplyr::filter(nutr1,WAVE==2011)
fr2011 <- dplyr::filter(nutr3,WAVE==2011)

fi2011 <- left_join(fi2011,fct,by="FOODCODE")
fr2011 <- left_join(fr2011,fct,by="FOODCODE")

## Eat from home
fr2011.home <- fr2011 %>% dplyr::filter(V43==1) %>% dplyr::filter(type==4|type==5|type==8|
                                                                    type==9|type==12)

## Intake of veg and meat by individual
fr2011.ind <- fr2011.home %>% dplyr::group_by(IDind,VD,hhid) %>% summarize(ind_amt=sum(V39))
## Intake of veg and meat by family
fr2011.fam <- fr2011.home %>% dplyr::group_by(hhid, VD) %>% summarize(fam_amt=sum(V39))
## Proportion
fr2011.pro <- left_join(fr2011.ind,fr2011.fam,by=c("hhid","VD"))
fr2011.pro <- dplyr::mutate(fr2011.pro,pro=ind_amt/fam_amt)

## Family food inventory file
fi2011.fats <- dplyr::filter(fi2011,dash==11)
fi2011.salt <- dplyr::filter(fi2011,cmfp==10)

fi2011.fats.amt <-fi2011.fats %>% dplyr::group_by(hhid,dash) %>% summarize(total_fats=sum(V22))
fi2011.salt.amt <-fi2011.salt %>% dplyr::group_by(hhid,dash) %>% summarize(total_salt=sum(V22))

fr2011.pro.fats <- left_join(fr2011.pro,fi2011.fats.amt,by="hhid")
fr2011.pro.fats <- dplyr::mutate(fr2011.pro.fats,fats=pro*total_fats)
fr2011.pro.salt <- left_join(fr2011.pro,fi2011.salt.amt,by="hhid")
fr2011.pro.salt <- dplyr::mutate(fr2011.pro.salt,salt=pro*total_salt)
fr2011.pro.salt2<- dplyr::select(fr2011.pro.salt,IDind,VD,salt)
fr2011.pro.fs <- left_join(fr2011.pro.fats,fr2011.pro.salt2,by=c("IDind","VD"))

fr2011.fs.md <- fr2011.pro.fs %>% dplyr::group_by(IDind) %>% summarize(md_fo=mean(fats),md_salt=mean(salt),na.rm=T)

###########################################################################

fr2011 <- fr2011 %>% dplyr::mutate(fg_fiber=(V39*edible)/100*(fiber/100),
                                   fg_na=(V39*edible)/100*(sodium/100),
                                   fg_pufa=(V39*edible)/100*(PUFA/100),
                                   fg_n3=(V39*edible)/100*((EPA+DHA)*Total/100))

##计算每个人钠、纤维素、pufa和n3的摄入量

fr2011.nu <- fr2011 %>% dplyr::group_by(IDind,VD) %>% summarize(fiber=sum(fg_fiber),
                                                                na=sum(fg_na),
                                                                pufa=sum(fg_pufa),
                                                                n3=sum(fg_n3),na.rm=T)

fr2011.nu.md <- fr2011.nu %>% dplyr::group_by(IDind) %>% summarize(md_fiber=mean(fiber),
                                                                   md_na=mean(na),
                                                                   md_pufa=mean(pufa),
                                                                   md_n3=mean(n3),na.rm=T) %>%
  dplyr::select(-na.rm)

##计算膳食质量各个组分的摄入量
#CMFP
fr2011.cmfp <- fr2011 %>% 
  dplyr::filter(cmfp!=0) %>% 
  dplyr::group_by(IDind,VD,cmfp) %>% 
  summarize(vd_amt=sum(V39))

fr2011.cmfp.md <- fr2011.cmfp %>%
  dplyr::group_by(IDind,cmfp) %>%
  summarize(md_amt=mean(vd_amt),na.rm=T)%>%
  spread(key=cmfp,value=md_amt,fill=0)

cmfp.c.2011 <- left_join(macro_md.2011,fr2011.cmfp.md,by="IDind")
cmfp.c.2011 <- left_join(cmfp.c.2011,fr2011.fs.md,by="IDind")
cmfp.c.2011 <- cmfp.c.2011 %>%
  dplyr::mutate(cmfp1=cmfp.c.2011$"1"/(d3kcal/2000),
                cmfp2=cmfp.c.2011$"2"/(d3kcal/2000),
                cmfp3=cmfp.c.2011$"3"/(d3kcal/2000),
                cmfp4=cmfp.c.2011$"4"/(d3kcal/2000),
                cmfp5=cmfp.c.2011$"5"/(d3kcal/2000),
                cmfp6=cmfp.c.2011$"6"/(d3kcal/2000),
                cmfp7=cmfp.c.2011$"7"/(d3kcal/2000),
                cmfp8=cmfp.c.2011$"8"/(d3kcal/2000),
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
fr2011.dash<- fr2011 %>% 
  dplyr::filter(dash!=0) %>% 
  dplyr::group_by(IDind,VD,dash) %>% 
  summarize(vd_amt=sum(V39))

fr2011.dash.md <- fr2011.dash %>%
  dplyr::group_by(IDind,dash) %>%
  summarize(md_amt=mean(vd_amt),na.rm=T)%>%
  spread(key=dash,value=md_amt,fill=0)

# For fats and oils
fr2011.f.md <- fr2011.fs.md %>%
  dplyr::select(IDind,md_fo)
fr2011.dash.md <- left_join(fr2011.dash.md,fr2011.f.md,by="IDind")

#For sodium
fr2011.na.md <- dplyr::select(fr2011.nu.md,IDind,md_na)
fr2011.dash.md <- left_join(fr2011.dash.md,fr2011.na.md,by="IDind")

#For SSB
ssb.2011 <- pe.all %>%
  dplyr::filter(WAVE==2011) %>%
  dplyr::select(IDind,SSB_d) %>%
  dplyr::mutate(ssb=case_when(SSB_d==1~7,SSB_d==2~4,SSB_d==3~2,SSB_d==4~0.5,SSB_d==5~0.1,SSB_d==9~NA_real_)) %>%
  dplyr::select(-SSB_d)

fr2011.dash.md <- left_join(fr2011.dash.md,ssb.2011,by="IDind")

#Serving size
dash.c.2011 <- left_join(macro_md.2011,fr2011.dash.md, by="IDind")


dash.c.2011 <-  dash.c.2011 %>%
  dplyr::mutate(dash1=(dash.c.2011$"1"/(d3kcal/2000))/28,
                dash2=(dash.c.2011$"2"/(d3kcal/2000))/80,
                dash3=(dash.c.2011$"3"/(d3kcal/2000))/80,
                dash4=(dash.c.2011$"4"/(d3kcal/2000))/245,
                dash5=(dash.c.2011$"5"/(d3kcal/2000))*7/28,
                dash6=(dash.c.2011$"6"/(d3kcal/2000))*7/50,
                dash7=(dash.c.2011$"7"/(d3kcal/2000))*7/43,
                dash8=(dash.c.2011$"8"/(d3kcal/2000))*7/28,
                dash9=(dash.c.2011$"9"/(d3kcal/2000))*7/113,
                dash10=(md_fo/(d3kcal/2000))/15,
                dash11=(dash.c.2011$"11"/(d3kcal/2000))*5/15) %>%
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
fr2011.ahei<- fr2011 %>% 
  dplyr::filter(ahei!=0) %>% 
  dplyr::group_by(IDind,VD,ahei) %>% 
  summarize(vd_amt=sum(V39))

fr2011.ahei.md <- fr2011.ahei %>%
  dplyr::group_by(IDind,ahei) %>%
  summarize(md_amt=mean(vd_amt),na.rm=T)%>%
  spread(key=ahei,value=md_amt,fill=0)

fr2011.ahei.md <- left_join(fr2011.ahei.md,fr2011.nu.md,by="IDind")

#For SSB and alcohol from questionaire
sh.2011 <- pe.all %>%
  dplyr::filter(WAVE==2011) %>%
  dplyr::select(IDind,SSB_d,alcohol_d)
fr2011.ahei.md <- left_join(fr2011.ahei.md,sh.2011,by="IDind")

#Gender and macro
fr2011.ahei.md <- left_join(fr2011.ahei.md,gender.all,by="IDind")
ahei.c.2011 <- left_join(fr2011.ahei.md,macro_md.2011,by="IDind")

#Score for alcohol
ahei.c.2011 <- ahei.c.2011 %>%
  dplyr::mutate(d51=`51`/(d3kcal/2000)/(236*4),d52=`52`/(d3kcal/2000)/(236*12),
                d53=`53`/(d3kcal/2000)/(236*1.5)) %>%
  dplyr::mutate(s_alcohol=d51+d52+d53) %>%
  dplyr::mutate(score_alcohol=case_when(gender==1 & s_alcohol>=3.5~0,gender==2 & s_alcohol>=2.5~0,s_alcohol<0.5~2.5,
                                        gender==1 & s_alcohol>=0.5 & s_alcohol<=2.0~10, gender==2 & s_alcohol>=0.5 & s_alcohol<=1.5~10,
                                        gender==1 & s_alcohol>2.0 & s_alcohol<3.5~10-(s_alcohol-2.0)*(10/1.5),
                                        gender==2 & s_alcohol>1.5 & s_alcohol<2.5~10-(s_alcohol-1.5)*(10/1)),na.rm=T)

#Score for SSB
ahei.c.2011 <- ahei.c.2011 %>%
  dplyr::mutate(score_ssb=case_when(SSB_d==1~0,SSB_d==2~4,SSB_d==3~6,SSB_d==4~8,SSB_d==5|SSB_d==9 | is.na(SSB_d)~10))

#Score for pufa
ahei.c.2011 <- ahei.c.2011 %>%
  dplyr::mutate(pufa_p=md_pufa*9/d3kcal*100,na.rm=T) %>%
  dplyr::mutate(score_pufa=case_when(pufa_p>=10~10,pufa_p<=2~0,pufa_p<10 & pufa_p>2~(pufa_p-2)*(10/8)),na.rm=T)

#Score for sodium
ahei.c.2011$md_na[is.na(ahei.c.2011$md_na)] <- round(mean(ahei.c.2011$md_na,na.rm = T))
ahei.c.2011$score_na <- ntile(desc(ahei.c.2011$md_na),10)

#others

ahei.c.2011 <- ahei.c.2011 %>%
  dplyr::mutate(score_veg=case_when(`1`==0~0,`1`>=591~10,`1`>0 & `1`<591~`1`*10/591),
                score_fruit=case_when(`2`==0~0,`2`>=473~10,`2`>0 & `2`<473~`2`*10/473),
                score_fiber=case_when(md_fiber==0~0,md_fiber>=15~10,md_fiber>0 & md_fiber<15~md_fiber*10/15),
                score_nuts=case_when(`3`==0~0,`3`>=28~10,`3`>0 & `3`<28~`3`*10/28),
                score_n3=case_when(md_n3==0~0,md_n3>=250~10,md_n3>0 & md_n3<250~md_n3*10/250),
                score_meat=case_when(`4`==0~10,`4`>=170~0,`4`>0 & `4`<170~10-(`4`*10/170))) %>%
  dplyr::mutate(ahei_score=score_alcohol+score_ssb+score_na+score_pufa+score_veg+score_fruit+score_fiber+score_nuts+score_n3+score_meat)

#diet quality 2011
cmfp.c.c2011 <- dplyr::select(cmfp.c.2011,IDind,cmfp_score)
dash.c.c2011 <- dplyr::select(dash.c.2011,IDind,dash_score)
ahei.c.c2011 <- dplyr::select(ahei.c.2011,IDind,ahei_score)

dq2011 <- left_join(cmfp.c.c2011,dash.c.c2011,by="IDind")
dq2011 <- left_join(dq2011,ahei.c.c2011,by="IDind")

