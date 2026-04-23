
forecast<-read.csv(paste0("forecast_longterm_completo_panel.csv")) %>% mutate(modelo=ifelse(nchar(modelo)>15,"ARIMAX",modelo))

edad<-"P18+"
hora<-12
conf<-80
mod<-"GLMNET"
date<-"2023-01-01"

filtro<-forecast %>% mutate(fecha=as.Date(fecha))%>% filter(age_range==edad,hour==hora,conf_level==conf,
                            modelo==mod,fecha_base==date)
filtro2<-forecast %>% mutate(fecha=as.Date(fecha))%>% filter(age_range==edad,hour==hora,conf_level==95,
                                                            modelo==mod,fecha_base==date)

#edad<-c("P18+","P18-49")
edad<-c("P2+","P18+","P18-49")
hora<-c(7:23,0:6)
modelos<-c("ARIMAX","GLMNET")
date<-c("2022-01-01","2023-01-01")

Final<-data.frame()
for(i in edad){
  for(j in hora){
    for(k in modelos){
      for(d in date){
        # i<-"P18-49"
        # j<-20
        # k<-"GLMNET"
        # d<-"2022-01-01"
        filtro80<-forecast %>% mutate(fecha=as.Date(fecha))%>% filter(age_range==i,hour==j,conf_level==80,
                                                                    modelo==k,fecha_base==d,year(fecha)==2027) 
        names(filtro80)[10:11]<-paste0(names(filtro80)[10:11],"_80")
        filtro90<-forecast %>% mutate(fecha=as.Date(fecha))%>% filter(age_range==i,hour==j,conf_level==90,
                                                                      modelo==k,fecha_base==d,year(fecha)==2027)
        names(filtro90)[10:11]<-paste0(names(filtro90)[10:11],"_90")
        filtro95<-forecast %>% mutate(fecha=as.Date(fecha))%>% filter(age_range==i,hour==j,conf_level==95,
                                                                      modelo==k,fecha_base==d,year(fecha)==2027)
        names(filtro95)[10:11]<-paste0(names(filtro95)[10:11],"_95")
        Aux<-cbind(filtro80,filtro90) %>% select(-c(12:20)) %>% cbind(filtro95) %>% select(-c(14:22))
      Final<-rbind(Final,Aux)
        }
    }
  }
}
  

saveRDS(Final,paste0("Forecast_x_hora_Panel.rds"))

forcst<-readRDS(paste0("Forecast_x_hora_Panel.rds"))

filtro<-forcst %>% filter(age_range=="P18+",hour%in%c(8:11),fecha_base==date)


metricas<-read_xlsx(paste0("metricas_modelos_panel_2026-04-10.xlsx"))

#edad<-c("P18","P18_49")
edad<-c("P18","P18_49","P2")
hora<-c(7:23,0:6)
modelos<-c("arimax","glmnet")

date<-c("2022-01-01","2023-01-01")

FinalMetricas<-data.frame()
for(i in edad){
  for(j in hora){
        #i<-"P18"
        #j<-1
        filtro<-metricas %>% filter(age_range==i,hour==j) %>% 
        arrange(mape)
      FinalMetricas<-rbind(FinalMetricas,filtro[1,])
      
  
  }
}

FinalMetricas %>% ggplot(aes(x=hour,y=mape,group=age_range,color=age_range))+geom_line()
# Crear un vector con nombres
diccionario <- c("arimax" = "ARIMAX", "glmnet" = "GLMNET","P18"="P18+",
                 "P18_49"="P18-49","P2"="P2+","2022_01_01"="2022-01-01",
                 "2023_01_01"="2023-01-01")

# Acceder por nombre
diccionario["arimax"]  # Devuelve 2

aux<-FinalMetricas[11,]
aux$model
aux$date
aux$age_range
aux$hour

EstFinal<-data.frame()
for (i in 1:nrow(FinalMetricas)){
  #i<-1
  aux<-FinalMetricas[i,]
  filtro<-forcst %>% filter(modelo==diccionario[aux$model],
                            fecha_base==diccionario[aux$date],
                            age_range==diccionario[aux$age_range],
                            hour==aux$hour)
  EstFinal<-rbind(EstFinal,filtro)
}

saveRDS(EstFinal,paste0("Final_best_tunningxhora_Panel.rds"))

finP2<-readRDS(paste0("Final_best_tunningxhora_Panel.rds"))
