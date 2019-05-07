bio2009 <- biom2009 %>%
  dplyr::filter(!is.na(HbA1c) & !is.na(homa_ir)& !is.na(quicki))

macro_md.2009 <- macro_md %>% dplyr::filter(wave==2009) %>% dplyr::select(IDind,commid,d3kcal,d3carbo,d3fat,d3protn)

pe.2009 <- pe.all %>%
  dplyr::filter(WAVE==2009)
age.2009<- age.all %>%
  dplyr::filter(wave==2009) %>%
  dplyr::select(Idind,age) %>%
  dplyr::rename(IDind=Idind)

me2009 <- left_join(bio2009,macro_md.2009,by="IDind")
me2009 <- left_join(me2009,pe.2009,by="IDind")
me2009 <- left_join(me2009,age.2009,by="IDind")


me2009 <- me2009 %>%
  dplyr::filter(diabetes!=1 |is.na(diabetes)) %>%
  dplyr::filter(MI!=1|is.na(MI)) %>%
  dplyr::filter(cancer!=1|is.na(cancer)) %>%
  dplyr::filter(stroke!=1|is.na(stroke)) %>%
  dplyr::filter(pregnant!=1|is.na(pregnant)) %>%
  dplyr::filter(age>=18)%>%
  dplyr::select(-c(19:26),-c(29:36))

##Gender
me2009 <- left_join(me2009,gender.all,by="IDind")

##Education
educ.2009 <- educ.all %>%
  dplyr::filter(WAVE==2009) %>%
  dplyr::select(IDind,educ)
me2009 <- left_join(me2009,educ.2009,by="IDind")

##Income
income.2009 <- income.all %>%
  dplyr::filter(wave==2009) %>%
  dplyr::select(IDind,indinc)
me2009 <- left_join(me2009,income.2009,by="IDind")

##Urbanization
urban.2009 <- urban.all %>%
  dplyr::filter(wave==2009) %>%
  dplyr::select(COMMID,index) %>%
  dplyr::rename(commid=COMMID)
me2009 <- left_join(me2009,urban.2009,by="commid")

##Physical activity
pa.2009 <- pa.all %>%
  dplyr::filter(WAVE==2009) %>%
  dplyr::select(IDind,met)
me2009 <- left_join(me2009,pa.2009,by="IDind")
me2009 <- left_join(me2009,dq2009,by="IDind")

me2009 <- me2009 %>%
  dplyr::mutate(cmfp=cmfp_score/sd(cmfp_score,na.rm = T),
                dash=dash_score/sd(dash_score,na.rm = T),
                ahei=ahei_score/sd(ahei_score,na.rm = T))

dbi09 <- read.csv("chns/DBI2009_SCO.csv",stringsAsFactors = F)

dbi09 <- dbi09 %>%
  dplyr::select(IDind,HBS,LBS,DQD)

me2009 <- left_join(me2009,dbi09,by="IDind")

source("http://goo.gl/UUyEzD")
outlierKD(me2009, homa_ir)
outlierKD(me2009, HbA1c)
outlierKD(me2009, quicki)

me2009.mplus <- me2009
me2009.mplus[is.na(me2009.mplus)] <- 999
write.csv(me2009.mplus,"data/me2009 outlier.csv",row.names = F)
