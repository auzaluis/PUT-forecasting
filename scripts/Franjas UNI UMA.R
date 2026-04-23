ruta<-c("C:/Users/monica.olivares/OneDrive - Corporación Televisa, S.A. de C.V/Cris/US/Estimaciones 25/")
require(tidyverse)
require(readxl)
US<-readRDS("data/US_20260417.rds") %>% mutate(yearly=year(daily),trimestre=quarter(daily),
            hora=paste0(ifelse(nchar(hours)==2,hours,paste0("0",hours)),":",ifelse(nchar(quarter_hour)==2,quarter_hour,paste0("0",quarter_hour))),
            daily=case_when(hours%in%0:1~daily-1,TRUE~daily))

s<-US %>% filter(network_name=="Univision",data_type=="Panel",target=="P2+",hours%in%19:23,weekdays(daily)%in%("domingo")) 


canal<-"Univision"
Target<-"P18+"
franja<-c(7:7)
NFranja<-"DESPIERTA AMERICA (SUS) 7AM-8AM"
period<-"LV"
LV<-c("lunes"  ,   "martes"  ,  "miércoles" ,"jueves" ,   "viernes")
LD<-c("lunes"  ,   "martes"  ,  "miércoles" ,"jueves" ,   "viernes", "sábado"   , "domingo")
S<-"sábado"
D<-"domingo"
SD<-c("sábado"   , "domingo")
LVD<-c("lunes"  ,   "martes"  ,  "miércoles" ,"jueves" ,   "viernes"  , "domingo")
dias<-if(period=="LV"){
  LV
}else{
    if(period=="LD"){
      LD
    }else{
        if(period=="S"){
          S
        }else{if(period=="LVD"){
            LVD
        }else{if(period=="SD"){
            SD
          }else{D}}}}}
 
unique(weekdays(US$daily))

Shares<-function(canal,Target,franja,NFranja,period){
  LV<-c("lunes"  ,   "martes"  ,  "miércoles" ,"jueves" ,   "viernes")
  LD<-c("lunes"  ,   "martes"  ,  "miércoles" ,"jueves" ,   "viernes", "sábado"   , "domingo")
  S<-"sábado"
  D<-"domingo"
  SD<-c("sábado"   , "domingo")
  LVD<-c("lunes"  ,   "martes"  ,  "miércoles" ,"jueves" ,   "viernes"  , "domingo")
  dias<-if(period=="LV"){
    LV
  }else{
    if(period=="LD"){
      LD
    }else{
      if(period=="S"){
        S
      }else{if(period=="LVD"){
        LVD
      }else{if(period=="SD"){
        SD
      }else{D}}}}}
  
  
  filtro<-US %>% filter(network_name==canal,data_type=="Panel",target%in%Target,hours%in%franja,weekdays(daily)%in%dias) %>% 
    group_by(network_name,target,daily,yearly,trimestre) %>% 
    summarise(Share=mean(quarter_hour_share)) %>% mutate(quarter=paste0(yearly,"-Q",trimestre)) %>% 
    filter(quarter!="2026-Q2")
  
  trim<-filtro %>% group_by(network_name,target,yearly,trimestre,quarter) %>% 
    summarise(Share_quarter=mean(Share)) %>% mutate(Programa=NFranja)
  return(trim)
}

Shares("UniMas","P18+",c(8:10),"Despierta America 08-11","LV")

### Qs por canal y franja

### Daytime
Daytime <- data.frame(Dias=c(rep("LV",5)),
                      Programa=c("Despierta America 8-9","Despierta America","El Dicho","1pm Novela","Sientese Quien Pueda"),
                      Inic=c(8,8,11,13,14),
                      Fin=c(8,10,11,13,14))

ParrillaUni<-read_excel("Parrilla 2027 estimaciones - F.xlsx",sheet=1) %>% select(1,3:5) %>% 
  na.omit()
canal<-"Univision"
Target<-c("P2+","P18+","P18-49")
UNIvision<-data.frame()
for(i in 1:nrow(ParrillaUni)){
  #i<-63
  aux<-ParrillaUni[i,]
  Franja<-if(aux$Inicio<aux$Fin){seq(aux$Inicio,aux$Fin)}else{c(seq(aux$Inicio,23),seq(0,aux$Fin))}#aqui
  
  sh<-Shares("Univision",Target,Franja,aux$Programa,aux$DIASEMANA)
  UNIvision<-rbind(UNIvision,sh)
}


Shr_UNI_27_n<-data.frame()

table(UNIvision$Programa)
for(i in unique(UNIvision$Programa)){
  for(k in unique(UNIvision$target)){
    for(p in unique(UNIvision$trimestre)){
      filtro<-UNIvision %>% filter(Programa==i,target==k,trimestre==p)
      A<-filtro[nrow(filtro),c(1,2,4,5,7)]
      A[1,6]<-nrow(filtro)
      Shr_UNI_27_n<-rbind(A,Shr_UNI_27_n)
    }
  }
}

Shr_UNI_27<-data.frame()
for(i in unique(UNIvision$Programa)){
    for(k in unique(UNIvision$target)){
      for(p in unique(UNIvision$trimestre)){
        # i<-"ENT SUN 7PM-8PM"
        # k<-"P18+"
        # p<-1
        filtro<-UNIvision %>% filter(Programa==i,target==k,trimestre==p)
        indice=nrow(filtro)
      if(indice==1){
   
        filtro[indice+1,1:2]<-filtro[indice,1:2]
        filtro[indice+1,3]<-2027
        filtro[indice+1,4]<-filtro[indice,4]
        filtro[indice+1,5]<-paste0("2027-Q",filtro[indice,4])
        filtro[indice+1,6]<-filtro[indice,6]
        filtro[indice+1,7]<-filtro[indice,7]
      ### cambiar aqui los casos
      }else{
        
        filtro[(indice+1),1:2]<-filtro[indice,1:2]
        filtro[(indice+1),3]<-2027
        filtro[(indice+1),4]<-filtro[indice,4]
        filtro[(indice+1),5]<-paste0("2027-Q",filtro[indice,4])
        filtro[(indice+1),6]<-predict(lm(Share_quarter~yearly,filtro),filtro[(indice+1),3])
        filtro[(indice+1),7]<-filtro[indice,7]
        }
      
      Shr_UNI_27<-rbind(filtro,Shr_UNI_27)
      }
    }
}

#### hasta aqui
forcst_best<-readRDS("Final_best_tunningxhora_Panel.rds")


PUTs_est<-data.frame()
for(i in 1:nrow(ParrillaUni)){
  #i<-27
  aux<-ParrillaUni[i,]
  filtroPutsP2<-forcst_best %>% filter(age_range=="P2+",hour%in%seq(aux$Inicio,aux$Fin)) %>% 
    group_by(fecha,age_range) %>% summarise(pred=mean(prediccion),Li_80=mean(intervalo_inf_80),
                                            Ls_80=mean(intervalo_sup_80),Li_90=mean(intervalo_inf_90),
                                            Ls_90=mean(intervalo_sup_90),Li_95=mean(intervalo_inf_95),
                                            Ls_95=mean(intervalo_sup_95)) %>% mutate(Quarter=quarter(fecha)) %>% 
    group_by(age_range,Quarter)%>% 
    summarise(Pred=mean(pred),LI_80=mean(Li_80),
              LS_80=mean(Ls_80),LI_90=mean(Li_90),
              LS_90=mean(Ls_90),LI_95=mean(Li_95),
              LS_95=mean(Ls_95))
  filtroPutsP2$Programa<-aux$Programa
  filtroPutsP2$Dias<-aux$DIASEMANA
  filtroPutsP2$Inicio<-aux$Inicio
  filtroPutsP2$Fin<-aux$Fin
  filtroPuts18<-forcst_best %>% filter(age_range=="P18+",hour%in%seq(aux$Inicio,aux$Fin)) %>% 
    group_by(fecha,age_range) %>% summarise(pred=mean(prediccion),Li_80=mean(intervalo_inf_80),
                                            Ls_80=mean(intervalo_sup_80),Li_90=mean(intervalo_inf_90),
                                            Ls_90=mean(intervalo_sup_90),Li_95=mean(intervalo_inf_95),
                                            Ls_95=mean(intervalo_sup_95)) %>% mutate(Quarter=quarter(fecha)) %>% 
                                                                                       group_by(age_range,Quarter)%>% 
    summarise(Pred=mean(pred),LI_80=mean(Li_80),
              LS_80=mean(Ls_80),LI_90=mean(Li_90),
              LS_90=mean(Ls_90),LI_95=mean(Li_95),
              LS_95=mean(Ls_95))
  filtroPuts18$Programa<-aux$Programa
  filtroPuts18$Dias<-aux$DIASEMANA
  filtroPuts18$Inicio<-aux$Inicio
  filtroPuts18$Fin<-aux$Fin
  filtroPuts1849<-forcst_best %>% filter(age_range=="P18-49",hour%in%seq(aux$Inicio,aux$Fin)) %>% 
    group_by(fecha,age_range) %>% summarise(pred=mean(prediccion),Li_80=mean(intervalo_inf_80),
                                            Ls_80=mean(intervalo_sup_80),Li_90=mean(intervalo_inf_90),
                                            Ls_90=mean(intervalo_sup_90),Li_95=mean(intervalo_inf_95),
                                            Ls_95=mean(intervalo_sup_95)) %>% mutate(Quarter=quarter(fecha)) %>% 
    group_by(age_range,Quarter)%>% 
    summarise(Pred=mean(pred),LI_80=mean(Li_80),
              LS_80=mean(Ls_80),LI_90=mean(Li_90),
              LS_90=mean(Ls_90),LI_95=mean(Li_95),
              LS_95=mean(Ls_95))
  filtroPuts1849$Programa<-aux$Programa
  filtroPuts1849$Dias<-aux$DIASEMANA
  filtroPuts1849$Inicio<-aux$Inicio
  filtroPuts1849$Fin<-aux$Fin
  PUTs_est<-rbind(PUTs_est,filtroPutsP2,filtroPuts18,filtroPuts1849)
}

names(PUTs_est)[2]<-"trimestre"
B1<-Shr_UNI_27 %>% filter(yearly==2027) %>% mutate(age_range=target) %>% 
  select(c(1,8,4,6,7))%>% spread(key=trimestre,value=Share_quarter)
names(B1)[6:9]<-c("Shr_Q1","Shr_Q2","Shr_Q3","Shr_Q4")
B2<-PUTs_est %>% select(c(1,2,5,10:13))%>% spread(key=trimestre,value=LS_80)
names(B2)[6:9]<-c("PUTs_Q1","PUTs_Q2","PUTs_Q3","PUTs_Q4")

B<-left_join(B1,B2) %>% mutate(Q1=PUTs_Q1*(Shr_Q1/100),Q2=PUTs_Q2*(Shr_Q2/100),Q3=PUTs_Q3*(Shr_Q3/100),
                               Q4=PUTs_Q1*(Shr_Q1/100))

writexl::write_xlsx(B,"UNI_Panel_pred_2027_V2.xlsx")

## establece franjas Uma ====

ParrillaUma<-read_excel("Parrilla 2027 estimaciones - F.xlsx",sheet=2) %>% select(1,3:5) %>% 
  na.omit()

canal<-"UniMas"
Target<-c("P2+","P18+","P18-49")
UNImas<-data.frame()
for(i in 1:nrow(ParrillaUma)){
  #i<-63
  aux<-ParrillaUma[i,]
  Franja<-if(aux$Inicio<aux$Fin){seq(aux$Inicio,aux$Fin)}else{c(seq(aux$Inicio,23),seq(0,aux$Fin))}#aqui
  
  sh<-Shares("UniMas",Target,Franja,aux$Programa,aux$DIASEMANA)
  UNImas<-rbind(UNImas,sh)
}


Shr_UMA_27_n<-data.frame()

table(UNImas$Programa)
for(i in unique(UNImas$Programa)){
  for(k in unique(UNImas$target)){
    for(p in unique(UNImas$trimestre)){
      filtro<-UNImas %>% filter(Programa==i,target==k,trimestre==p)
      A<-filtro[nrow(filtro),c(1,2,4,5,7)]
      A[1,6]<-nrow(filtro)
      Shr_UMA_27_n<-rbind(A,Shr_UMA_27_n)
    }
  }
}

Shr_UMA_27<-data.frame()
for(i in unique(UNImas$Programa)){
  for(k in unique(UNImas$target)){
    for(p in unique(UNImas$trimestre)){
      # i<-"ENT SUN 7PM-8PM"
      # k<-"P18+"
      # p<-1
      filtro<-UNImas %>% filter(Programa==i,target==k,trimestre==p)
      indice=nrow(filtro)
      if(indice==1){
        
        filtro[indice+1,1:2]<-filtro[indice,1:2]
        filtro[indice+1,3]<-2027
        filtro[indice+1,4]<-filtro[indice,4]
        filtro[indice+1,5]<-paste0("2027-Q",filtro[indice,4])
        filtro[indice+1,6]<-filtro[indice,6]
        filtro[indice+1,7]<-filtro[indice,7]
        ### cambiar aqui los casos
      }else{
        
        filtro[(indice+1),1:2]<-filtro[indice,1:2]
        filtro[(indice+1),3]<-2027
        filtro[(indice+1),4]<-filtro[indice,4]
        filtro[(indice+1),5]<-paste0("2027-Q",filtro[indice,4])
        filtro[(indice+1),6]<-predict(lm(Share_quarter~yearly,filtro),filtro[(indice+1),3])
        filtro[(indice+1),7]<-filtro[indice,7]
      }
      
      Shr_UMA_27<-rbind(filtro,Shr_UMA_27)
    }
  }
}

#### hasta aqui

PUTs_est<-data.frame()
for(i in 1:nrow(ParrillaUma)){
  #i<-27
  aux<-ParrillaUma[i,]
  filtroPutsP2<-forcst_best %>% filter(age_range=="P2+",hour%in%seq(aux$Inicio,aux$Fin)) %>% 
    group_by(fecha,age_range) %>% summarise(pred=mean(prediccion),Li_80=mean(intervalo_inf_80),
                                            Ls_80=mean(intervalo_sup_80),Li_90=mean(intervalo_inf_90),
                                            Ls_90=mean(intervalo_sup_90),Li_95=mean(intervalo_inf_95),
                                            Ls_95=mean(intervalo_sup_95)) %>% mutate(Quarter=quarter(fecha)) %>% 
    group_by(age_range,Quarter)%>% 
    summarise(Pred=mean(pred),LI_80=mean(Li_80),
              LS_80=mean(Ls_80),LI_90=mean(Li_90),
              LS_90=mean(Ls_90),LI_95=mean(Li_95),
              LS_95=mean(Ls_95))
  filtroPutsP2$Programa<-aux$Programa
  filtroPutsP2$Dias<-aux$DIASEMANA
  filtroPutsP2$Inicio<-aux$Inicio
  filtroPutsP2$Fin<-aux$Fin
  filtroPuts18<-forcst_best %>% filter(age_range=="P18+",hour%in%seq(aux$Inicio,aux$Fin)) %>% 
    group_by(fecha,age_range) %>% summarise(pred=mean(prediccion),Li_80=mean(intervalo_inf_80),
                                            Ls_80=mean(intervalo_sup_80),Li_90=mean(intervalo_inf_90),
                                            Ls_90=mean(intervalo_sup_90),Li_95=mean(intervalo_inf_95),
                                            Ls_95=mean(intervalo_sup_95)) %>% mutate(Quarter=quarter(fecha)) %>% 
    group_by(age_range,Quarter)%>% 
    summarise(Pred=mean(pred),LI_80=mean(Li_80),
              LS_80=mean(Ls_80),LI_90=mean(Li_90),
              LS_90=mean(Ls_90),LI_95=mean(Li_95),
              LS_95=mean(Ls_95))
  filtroPuts18$Programa<-aux$Programa
  filtroPuts18$Dias<-aux$DIASEMANA
  filtroPuts18$Inicio<-aux$Inicio
  filtroPuts18$Fin<-aux$Fin
  filtroPuts1849<-forcst_best %>% filter(age_range=="P18-49",hour%in%seq(aux$Inicio,aux$Fin)) %>% 
    group_by(fecha,age_range) %>% summarise(pred=mean(prediccion),Li_80=mean(intervalo_inf_80),
                                            Ls_80=mean(intervalo_sup_80),Li_90=mean(intervalo_inf_90),
                                            Ls_90=mean(intervalo_sup_90),Li_95=mean(intervalo_inf_95),
                                            Ls_95=mean(intervalo_sup_95)) %>% mutate(Quarter=quarter(fecha)) %>% 
    group_by(age_range,Quarter)%>% 
    summarise(Pred=mean(pred),LI_80=mean(Li_80),
              LS_80=mean(Ls_80),LI_90=mean(Li_90),
              LS_90=mean(Ls_90),LI_95=mean(Li_95),
              LS_95=mean(Ls_95))
  filtroPuts1849$Programa<-aux$Programa
  filtroPuts1849$Dias<-aux$DIASEMANA
  filtroPuts1849$Inicio<-aux$Inicio
  filtroPuts1849$Fin<-aux$Fin
  PUTs_est<-rbind(PUTs_est,filtroPutsP2,filtroPuts18,filtroPuts1849)
}

names(PUTs_est)[2]<-"trimestre"
B1<-Shr_UMA_27 %>% filter(yearly==2027) %>% mutate(age_range=target) %>% 
  select(c(1,8,4,6,7))%>% spread(key=trimestre,value=Share_quarter)
names(B1)[6:9]<-c("Shr_Q1","Shr_Q2","Shr_Q3","Shr_Q4")
B2<-PUTs_est %>% select(c(1,2,5,10:13))%>% spread(key=trimestre,value=LS_80)
names(B2)[6:9]<-c("PUTs_Q1","PUTs_Q2","PUTs_Q3","PUTs_Q4")

B<-left_join(B1,B2) %>% mutate(Q1=PUTs_Q1*(Shr_Q1/100),Q2=PUTs_Q2*(Shr_Q2/100),Q3=PUTs_Q3*(Shr_Q3/100),
                               Q4=PUTs_Q1*(Shr_Q1/100))

writexl::write_xlsx(B,"UMA_Panel_pred_2027_V2.xlsx")


##### premios ####
Lonuestro<-as.Date(c("2022-02-24","2023-02-23","2024-02-22","2025-02-20","2026-02-19"))
Juventud<-as.Date(c("2022-07-21","2023-07-20","2024-07-25","2025-09-25"))
LGrammy<-as.Date("2022-11-17","2023-11-16","2024-11-14","2025-11-13")


US_lonuestro<-US %>% filter(network_name=="Univision",data_type=="Panel",hours%in%c(20:22),
                            daily%in%Lonuestro) %>% group_by(network_name,target,yearly) %>% 
  summarise(rat=mean(quarter_hour_program_audiences))

PJuventud<-US %>% filter(network_name=="Univision",data_type=="Panel",hours%in%c(20:22),
                         daily%in%Juventud) %>% group_by(network_name,target,yearly) %>% 
  summarise(rat=mean(quarter_hour_program_audiences))
