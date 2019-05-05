##Subjects in 2009 with diet data
macro_md.2009 <- macro_md %>% dplyr::filter(wave==2009) %>% dplyr::select(IDind,commid,d3kcal,d3carbo,d3fat,d3protn)
pe.2009 <- pe.all %>%
  dplyr::filter(WAVE==2009)
age.2009<- age.all %>%
  dplyr::filter(wave==2009) %>%
  dplyr::select(Idind,age) %>%
  dplyr::rename(IDind=Idind)

ID2009 <- left_join(macro_md.2009,pe.2009,by="IDind")
ID2009 <- left_join(ID2009,age.2009,by="IDind")

ID2009 <- ID2009 %>%
  dplyr::filter(diabetes!=1 |is.na(diabetes)) %>%
  dplyr::filter(MI!=1|is.na(MI)) %>%
  dplyr::filter(cancer!=1|is.na(cancer)) %>%
  dplyr::filter(stroke!=1|is.na(stroke)) %>%
  dplyr::filter(pregnant!=1|is.na(pregnant)) %>%
  dplyr::filter(age>=18) %>%
  dplyr::select(IDind,WAVE,hhid,age,commid,d3kcal,d3carbo,d3fat,d3protn,bmi,hwr,waist_c,diastol,systol)

##Gender
ID2009 <- left_join(ID2009,gender.all,by="IDind")

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
ID2009 <- left_join(ID2009,dq2009,by="IDind")

##Biomarker
ID2009 <- left_join(ID2009,biom2009,by="IDind")
ID2009 <- ID2009 %>%
  dplyr::mutate(cmfp=cmfp_score/sd(cmfp_score,na.rm = F),
                dash=dash_score/sd(dash_score,na.rm = F),
                ahei=ahei_score/sd(ahei_score,na.rm = T))
ID2009.mplus <- ID2009
ID2009.mplus[is.na(ID2009.mplus)] <- 999
write.csv(ID2009.mplus,"data/id2009.csv",row.names = F)