macro_md.2004 <- macro_md %>% dplyr::filter(wave==2004) %>% dplyr::select(IDind,commid,d3kcal,d3carbo,d3fat,d3protn)

pe.2004 <- pe.all %>%
  dplyr::filter(WAVE==2004)
age.2004<- age.all %>%
  dplyr::filter(wave==2004) %>%
  dplyr::select(Idind,age) %>%
  dplyr::rename(IDind=Idind)

me2004 <- left_join(macro_md.2004,pe.2004,by="IDind")
me2004 <- left_join(me2004,age.2004,by="IDind")


me2004 <- me2004 %>%
  dplyr::filter(diabetes!=1 |is.na(diabetes)) %>%
  dplyr::filter(MI!=1|is.na(MI)) %>%
  dplyr::filter(cancer!=1|is.na(cancer)) %>%
  dplyr::filter(stroke!=1|is.na(stroke)) %>%
  dplyr::filter(pregnant!=1|is.na(pregnant)) %>%
  dplyr::filter(age>=18)%>%
  dplyr::select(-c(9:16),-c(19:27))

##Gender
me2004 <- left_join(me2004,gender.all,by="IDind")

##Education
educ.2004 <- educ.all %>%
  dplyr::filter(WAVE==2004) %>%
  dplyr::select(IDind,educ)
me2004 <- left_join(me2004,educ.2004,by="IDind")

##Income
income.2004 <- income.all %>%
  dplyr::filter(wave==2004) %>%
  dplyr::select(IDind,indinc)
me2004 <- left_join(me2004,income.2004,by="IDind")

##Urbanization
urban.2004 <- urban.all %>%
  dplyr::filter(wave==2004) %>%
  dplyr::select(COMMID,index) %>%
  dplyr::rename(commid=COMMID)
me2004 <- left_join(me2004,urban.2004,by="commid")

##Physical activity
pa.2004 <- pa.all %>%
  dplyr::filter(WAVE==2004) %>%
  dplyr::select(IDind,met)
me2004 <- left_join(me2004,pa.2004,by="IDind")
me2004 <- left_join(me2004,dq2004,by="IDind")

me2004 <- me2004 %>%
  dplyr::mutate(cmfp=cmfp_score/sd(cmfp_score,na.rm = T),
                dash=dash_score/sd(dash_score,na.rm = T),
                ahei=ahei_score/sd(ahei_score,na.rm = T))

dbi04 <- read.csv("chns/DBI2004_SCO.csv",stringsAsFactors = F)

dbi04 <- dbi04 %>%
  dplyr::select(IDind,HBS,LBS,DQD)

me2004 <- left_join(me2004,dbi04,by="IDind")

pe.2006 <- pe.all %>%
  dplyr::filter(WAVE==2006) %>%
  dplyr::select(IDind,bmi) %>%
  dplyr::rename(bmi06=bmi)

me200406 <- left_join(me2004,pe.2006,by="IDind")
me0409 <- left_join(me200406,bio2009,by="IDind")

source("http://goo.gl/UUyEzD")
outlierKD(me0409, homa_ir)
outlierKD(me0409, HbA1c)
outlierKD(me0409, quicki)


me0409.mplus <- me0409
me0409.mplus[is.na(me0409.mplus)] <- 999
write.csv(me0409.mplus,"data/me0409 outlier.csv",row.names = F)

