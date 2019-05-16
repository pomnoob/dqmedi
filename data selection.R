##Subjects in 2004 with diet data
macro_md.2004 <- macro_md %>% dplyr::filter(wave==2004) %>% dplyr::select(IDind,commid,d3kcal,d3carbo,d3fat,d3protn,t2)
pe.2004 <- pe.all %>%
  dplyr::filter(WAVE==2004)
age.2004<- age.all %>%
    dplyr::filter(wave==2004) %>%
    dplyr::select(Idind,age) %>%
    dplyr::rename(IDind=Idind)

ID2004 <- left_join(macro_md.2004,pe.2004,by="IDind")
ID2004 <- left_join(ID2004,age.2004,by="IDind")

ID2004 <- ID2004 %>%
  dplyr::filter(diabetes!=1 |is.na(diabetes)) %>%
  dplyr::filter(MI!=1|is.na(MI)) %>%
  dplyr::filter(cancer!=1|is.na(cancer)) %>%
  dplyr::filter(stroke!=1|is.na(stroke)) %>%
  dplyr::filter(pregnant!=1|is.na(pregnant)) %>%
  dplyr::filter(age>=18) %>%
  dplyr::select(IDind,WAVE,hhid,t2,commid,age,d3kcal,d3carbo,d3fat,d3protn,bmi,hwr,waist_c,diastol,systol,smoke)

##Gender
ID2004 <- left_join(ID2004,gender.all,by="IDind")

##Education
educ.2004 <- educ.all %>%
  dplyr::filter(WAVE==2004) %>%
  dplyr::select(IDind,educ)
ID2004 <- left_join(ID2004,educ.2004,by="IDind")

##Income
income.2004 <- income.all %>%
  dplyr::filter(wave==2004) %>%
  dplyr::select(IDind,indinc)
ID2004 <- left_join(ID2004,income.2004,by="IDind")

##Urbanization
urban.2004 <- urban.all %>%
  dplyr::filter(wave==2004) %>%
  dplyr::select(COMMID,index) %>%
  dplyr::rename(commid=COMMID)
ID2004 <- left_join(ID2004,urban.2004,by="commid")

##Physical activity
pa.2004 <- pa.all %>%
  dplyr::filter(WAVE==2004) %>%
  dplyr::select(IDind,met)
ID2004 <- left_join(ID2004,pa.2004,by="IDind")

#Diet quality
dq04 <- read.csv("data/dq2004.csv",stringsAsFactors = F)
ID2004 <- left_join(ID2004,dq04,by="IDind")

#DBI
dbi04 <- read.csv("chns/DBI2004_SCO.csv",stringsAsFactors = F)

dbi04 <- dbi04 %>%
  dplyr::select(IDind,HBS,LBS,DQD)

ID2004 <- left_join(ID2004,dbi04,by="IDind")
ID2004 <- dplyr::rename(ID2004,age04=age,kcal04=d3kcal,
                        carbo04=d3carbo,fat04=d3fat,
                        protn04=d3protn,bmi04=bmi,hwr04=hwr,
                        wc04=waist_c,diastol04=diastol,
                        systol04=systol,educ04=educ,
                        indinc04=indinc,index04=index,
                        met04=met,cmfp04=cmfp_score,
                        dash04=dash_score,ahei04=ahei_score,
                        smoke04=smoke,lbs04=LBS,hbs04=HBS,dqd04=DQD)

id <- dplyr::select(ID2004,IDind)

##Subjects in 2006 with diet data
macro_md.2006 <- macro_md %>% dplyr::filter(wave==2006) %>% dplyr::select(IDind,commid,d3kcal,d3carbo,d3fat,d3protn)
pe.2006 <- pe.all %>%
  dplyr::filter(WAVE==2006)
age.2006<- age.all %>%
  dplyr::filter(wave==2006) %>%
  dplyr::select(Idind,age) %>%
  dplyr::rename(IDind=Idind)

ID2006<- left_join(id, macro_md.2006,by="IDind")
ID2006 <- left_join(ID2006,pe.2006,by="IDind")
ID2006 <- left_join(ID2006,age.2006,by="IDind")

ID2006 <- ID2006 %>%
  dplyr::filter(diabetes!=1 |is.na(diabetes)) %>%
  dplyr::filter(MI!=1|is.na(MI)) %>%
  dplyr::filter(cancer!=1|is.na(cancer)) %>%
  dplyr::filter(stroke!=1|is.na(stroke)) %>%
  dplyr::filter(pregnant!=1|is.na(pregnant)) %>%
  dplyr::filter(age>=18) %>%
  dplyr::select(IDind,WAVE,hhid,commid,age,d3kcal,d3carbo,d3fat,d3protn,bmi,hwr,waist_c,diastol,systol,smoke)

##Education
educ.2006 <- educ.all %>%
  dplyr::filter(WAVE==2006) %>%
  dplyr::select(IDind,educ)
ID2006 <- left_join(ID2006,educ.2006,by="IDind")

##Income
income.2006 <- income.all %>%
  dplyr::filter(wave==2006) %>%
  dplyr::select(IDind,indinc)
ID2006 <- left_join(ID2006,income.2006,by="IDind")

##Urbanization
urban.2006 <- urban.all %>%
  dplyr::filter(wave==2006) %>%
  dplyr::select(COMMID,index) %>%
  dplyr::rename(commid=COMMID)
ID2006 <- left_join(ID2006,urban.2006,by="commid")

##Physical activity
pa.2006 <- pa.all %>%
  dplyr::filter(WAVE==2006) %>%
  dplyr::select(IDind,met)
ID2006 <- left_join(ID2006,pa.2006,by="IDind")
#Diet quality
dq06 <- read.csv("data/dq2006.csv",stringsAsFactors = F)
ID2006 <- left_join(ID2006,dq06,by="IDind")

#DBI
dbi06 <- read.csv("chns/DBI2006_SCO.csv",stringsAsFactors = F)

dbi06 <- dbi06 %>%
  dplyr::select(IDind,HBS,LBS,DQD)
ID2006 <- left_join(ID2006,dbi06,by="IDind")
ID2006 <- dplyr::rename(ID2006,age06=age,kcal06=d3kcal,
                        carbo06=d3carbo,fat06=d3fat,
                        protn06=d3protn,bmi06=bmi,hwr06=hwr,
                        wc06=waist_c,diastol06=diastol,
                        systol06=systol,educ06=educ,
                        indinc06=indinc,index06=index,
                        met06=met,cmfp06=cmfp_score,
                        dash06=dash_score,ahei06=ahei_score,
                        smoke06=smoke,lbs06=LBS,hbs06=HBS,dqd06=DQD) %>%
  dplyr::select(-WAVE,-commid,-hhid)

##Subjects in 2009 with diet data
macro_md.2009 <- macro_md %>% dplyr::filter(wave==2009) %>% dplyr::select(IDind,commid,d3kcal,d3carbo,d3fat,d3protn)
pe.2009 <- pe.all %>%
  dplyr::filter(WAVE==2009)
age.2009<- age.all %>%
  dplyr::filter(wave==2009) %>%
  dplyr::select(Idind,age) %>%
  dplyr::rename(IDind=Idind)

ID2009<- left_join(id, macro_md.2009,by="IDind")
ID2009 <- left_join(ID2009,pe.2009,by="IDind")
ID2009 <- left_join(ID2009,age.2009,by="IDind")

ID2009 <- ID2009 %>%
  dplyr::filter(diabetes!=1 |is.na(diabetes)) %>%
  dplyr::filter(MI!=1|is.na(MI)) %>%
  dplyr::filter(cancer!=1|is.na(cancer)) %>%
  dplyr::filter(stroke!=1|is.na(stroke)) %>%
  dplyr::filter(pregnant!=1|is.na(pregnant)) %>%
  dplyr::filter(age>=18) %>%
  dplyr::select(IDind,WAVE,hhid,commid,age,d3kcal,d3carbo,d3fat,d3protn,bmi,hwr,waist_c,diastol,systol,smoke)

##Education
educ.2009 <- educ.all %>%
  dplyr::filter(WAVE==2009) %>%
  dplyr::select(IDind,educ)
ID2009 <- left_join(ID2009,educ.2009,by="IDind")

##Income
income.2009 <- income.all %>%
  dplyr::filter(wave==2009) %>%
  dplyr::select(IDind,indinc)
ID2009 <- left_join(ID2009,income.2009,by="IDind")

##Urbanization
urban.2009 <- urban.all %>%
  dplyr::filter(wave==2009) %>%
  dplyr::select(COMMID,index) %>%
  dplyr::rename(commid=COMMID)
ID2009 <- left_join(ID2009,urban.2009,by="commid")

##Physical activity
pa.2009 <- pa.all %>%
  dplyr::filter(WAVE==2009) %>%
  dplyr::select(IDind,met)
ID2009 <- left_join(ID2009,pa.2009,by="IDind")

#Diet quality
dq09 <- read.csv("data/dq2009.csv",stringsAsFactors = F)
ID2009 <- left_join(ID2009,dq09,by="IDind")

#DBI
dbi09 <- read.csv("chns/DBI2009_SCO.csv",stringsAsFactors = F)

dbi09 <- dbi09 %>%
  dplyr::select(IDind,HBS,LBS,DQD)
ID2009 <- left_join(ID2009,dbi09,by="IDind")
ID2009 <- dplyr::rename(ID2009,age09=age,kcal09=d3kcal,
                        carbo09=d3carbo,fat09=d3fat,
                        protn09=d3protn,bmi09=bmi,hwr09=hwr,
                        wc09=waist_c,diastol09=diastol,
                        systol09=systol,educ09=educ,
                        indinc09=indinc,index09=index,
                        met09=met,cmfp09=cmfp_score,
                        dash09=dash_score,ahei09=ahei_score,
                        smoke09=smoke,lbs09=LBS,hbs09=HBS,dqd09=DQD)%>%
  dplyr::select(-WAVE,-commid,-hhid)


##Subjects in 2011 with diet data
macro_md.2011 <- macro_md %>% dplyr::filter(wave==2011) %>% dplyr::select(IDind,commid,d3kcal,d3carbo,d3fat,d3protn)
pe.2011 <- pe.all %>%
  dplyr::filter(WAVE==2011)
age.2011<- age.all %>%
  dplyr::filter(wave==2011) %>%
  dplyr::select(Idind,age) %>%
  dplyr::rename(IDind=Idind)

ID2011<- left_join(id, macro_md.2011,by="IDind")
ID2011 <- left_join(ID2011,pe.2011,by="IDind")
ID2011 <- left_join(ID2011,age.2011,by="IDind")

ID2011 <- ID2011 %>%
  dplyr::filter(diabetes!=1 |is.na(diabetes)) %>%
  dplyr::filter(MI!=1|is.na(MI)) %>%
  dplyr::filter(cancer!=1|is.na(cancer)) %>%
  dplyr::filter(stroke!=1|is.na(stroke)) %>%
  dplyr::filter(pregnant!=1|is.na(pregnant)) %>%
  dplyr::filter(age>=18) %>%
  dplyr::select(IDind,WAVE,hhid,commid,age,d3kcal,d3carbo,d3fat,d3protn,bmi,hwr,waist_c,diastol,systol,smoke)

##Education
educ.2011 <- educ.all %>%
  dplyr::filter(WAVE==2011) %>%
  dplyr::select(IDind,educ)
ID2011 <- left_join(ID2011,educ.2011,by="IDind")

##Income
income.2011 <- income.all %>%
  dplyr::filter(wave==2011) %>%
  dplyr::select(IDind,indinc)
ID2011 <- left_join(ID2011,income.2011,by="IDind")

##Urbanization
urban.2011 <- urban.all %>%
  dplyr::filter(wave==2011) %>%
  dplyr::select(COMMID,index) %>%
  dplyr::rename(commid=COMMID)
ID2011 <- left_join(ID2011,urban.2011,by="commid")

##Physical activity
pa.2011 <- pa.all %>%
  dplyr::filter(WAVE==2011) %>%
  dplyr::select(IDind,met)
ID2011 <- left_join(ID2011,pa.2011,by="IDind")

#Diet quality
dq11 <- read.csv("data/dq2011.csv",stringsAsFactors = F)
ID2011 <- left_join(ID2011,dq11,by="IDind")

#DBI
dbi11 <- read.csv("chns/DBI2011_SCO.csv",stringsAsFactors = F)
dbi11 <- dbi11 %>%
  dplyr::select(IDind,HBS,LBS,DQD)
ID2011 <- left_join(ID2011,dbi11,by="IDind")
ID2011 <- dplyr::rename(ID2011,age11=age,kcal11=d3kcal,
                        carbo11=d3carbo,fat11=d3fat,
                        protn11=d3protn,bmi11=bmi,hwr11=hwr,
                        wc11=waist_c,diastol11=diastol,
                        systol11=systol,educ11=educ,
                        indinc11=indinc,index11=index,
                        met11=met,cmfp11=cmfp_score,
                        dash11=dash_score,ahei11=ahei_score,
                        smoke11=smoke,lbs11=LBS,hbs11=HBS,dqd11=DQD)%>%
  dplyr::select(-WAVE,-commid,-hhid)


id0411 <- left_join(ID2004,ID2006,by="IDind")
id0411 <- left_join(id0411,ID2009,by="IDind")
id0411 <- left_join(id0411,ID2011,by="IDind")

id0411$q_index <- ntile(id0411$index04,4)
id0411 <- dplyr::mutate(id0411,q1=case_when(q_index==2~1,q_index!=2~0),
                        q2=case_when(q_index==3~1,q_index!=3~0),
                        q3=case_when(q_index==4~1,q_index!=4~0))
id0411 <- id0411 %>% 
  dplyr::mutate(carbo_p=(carbo04*4/kcal04)*100,
                protn_p=(protn04*4/kcal04)*100,
                fat_p=(fat04*9/kcal04)*100)

id0411.mplus <- id0411
id0411.mplus[is.na(id0411.mplus)] <- 999


write.csv(id0411.mplus,"data/id0411 mplus.csv",row.names = F)
write.csv(id0411,"data/id0411.csv",row.names = F)


                

#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
id0411 <- read.csv("data/id0411.csv",stringsAsFactors = F)

id0411$q_index <- factor(id0411$q_index)
#Mean and SD of all
sapply(id0411, mean,na.rm=T)
sapply(id0411, sd,na.rm=T)


contrasts(id0411$q_index) <- contr.poly
#Age
tapply(id0411$age04,id0411$q_index,mean)
tapply(id0411$age04,id0411$q_index,sd)

l.age <- lm(age04~q_index+gender,data = id0411)
summary(l.age)

#BMI
tapply(id0411$bmi04,id0411$q_index,mean,na.rm=T)
tapply(id0411$bmi04,id0411$q_index,sd,na.rm=T)
l.bmi <- lm(bmi04~q_index+gender+age04,data = id0411)
summary(l.bmi)

#MET
tapply(id0411$met04,id0411$q_index,mean,na.rm=T)
tapply(id0411$met04,id0411$q_index,sd,na.rm=T)
l.met <- lm(met04~q_index+gender+age04,data = id0411)
summary(l.met)

#Systolic
tapply(id0411$systol04,id0411$q_index,mean,na.rm=T)
tapply(id0411$systol04,id0411$q_index,sd,na.rm=T)
l.systo <- lm(systol04~q_index+gender+age04,data = id0411)
summary(l.systo)

#Diastolic
tapply(id0411$diastol04,id0411$q_index,mean,na.rm=T)
tapply(id0411$diastol04,id0411$q_index,sd,na.rm=T)
l.diasto <- lm(diastol04~q_index+gender+age04,data = id0411)
summary(l.diasto)

#energy intake
tapply(id0411$kcal04,id0411$q_index,mean,na.rm=T)
tapply(id0411$kcal04,id0411$q_index,sd,na.rm=T)
l.kcal <- lm(kcal04~q_index+gender+age04,data = id0411)
summary(l.kcal)

#carbohydrate intake
tapply(id0411$carbo_p,id0411$q_index,mean,na.rm=T)
tapply(id0411$carbo_p,id0411$q_index,sd,na.rm=T)
l.carbo_p <- lm(carbo_p~q_index+gender+age04,data = id0411)
summary(l.carbo_p)

#fat intake
tapply(id0411$fat_p,id0411$q_index,mean,na.rm=T)
tapply(id0411$fat_p,id0411$q_index,sd,na.rm=T)
l.fat_p <- lm(fat_p~q_index+gender+age04,data = id0411)
summary(l.fat_p)

#protein intake
tapply(id0411$protn_p,id0411$q_index,mean,na.rm=T)
tapply(id0411$protn_p,id0411$q_index,sd,na.rm=T)
l.protn_p <- lm(protn_p~q_index+gender+age04,data = id0411)
summary(l.protn_p)

#DQD
tapply(id0411$dqd04,id0411$q_index,mean,na.rm=T)
tapply(id0411$dqd04,id0411$q_index,sd,na.rm=T)
l.dqd <- lm(dqd04~q_index+gender+age04,data = id0411)
summary(l.dqd)

#HBS
tapply(id0411$hbs04,id0411$q_index,mean,na.rm=T)
tapply(id0411$hbs04,id0411$q_index,sd,na.rm=T)
l.hbs <- lm(hbs04~q_index+gender+age04,data = id0411)
summary(l.hbs)

#HBS
tapply(id0411$lbs04,id0411$q_index,mean,na.rm=T)
tapply(id0411$lbs04,id0411$q_index,sd,na.rm=T)
l.lbs <- lm(lbs04~q_index+gender+age04,data = id0411)
summary(l.lbs)

#CHFP
tapply(id0411$cmfp04,id0411$q_index,mean,na.rm=T)
tapply(id0411$cmfp04,id0411$q_index,sd,na.rm=T)
l.cmfp <- lm(cmfp04~q_index+gender+age04,data = id0411)
summary(l.cmfp)

#DASH
tapply(id0411$dash04,id0411$q_index,mean,na.rm=T)
tapply(id0411$dash04,id0411$q_index,sd,na.rm=T)
l.dash <- lm(dash04~q_index+gender+age04,data = id0411)
summary(l.dash)

#AHEI
tapply(id0411$ahei04,id0411$q_index,mean,na.rm=T)
tapply(id0411$ahei04,id0411$q_index,sd,na.rm=T)
l.ahei <- lm(ahei04~q_index+gender+age04,data = id0411)
summary(l.ahei)

#Urbanization index
tapply(id0411$index04,id0411$q_index,mean,na.rm=T)
tapply(id0411$index04,id0411$q_index,sd,na.rm=T)
summary(id0411$q_index)

# Cochran-Armitage trend test for binary
library(coin)
independence_test(gender~q_index,data = id0411,teststat = "quad")
independence_test(smoke04~q_index,data = id0411,teststat = "quad")
independence_test(t2~q_index,data = id0411,teststat = "quad")

#Percentage
id0411$q_index <- fill_gaps(id0411$q_index)
id0411_smoke <- id0411 %>%
  dplyr::select(smoke04,q_index) %>%
  dplyr::filter(!is.na(q_index))
perc <- prop.table(table(id0411_smoke))
print(perc
      )

id0411_t2 <- id0411 %>%
  dplyr::select(t2,q_index) %>%
  dplyr::filter(!is.na(q_index))
perct2 <- prop.table(table(id0411_t2),2)
print(perct2
)

id0411_gender <- id0411 %>%
  dplyr::select(gender,q_index) %>%
  dplyr::filter(!is.na(q_index))
perctg <- prop.table(table(id0411_gender),2)
print(perctg)
