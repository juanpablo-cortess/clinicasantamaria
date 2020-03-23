library(httr)
library(httpuv)
library(googleAnalyticsR)
library(dplyr)
library(stringr)
library(googleAuthR)
library(googlesheets)
library(ggplot2)
library(ggrepel)
library(googlesheets4)
library(reshape2)
library(openxlsx)
library(stringr)
library(stringi)
ga_auth()

clinica_id<-167637274
start_date<-"2020-01-01"
end_date<-"2020-02-29"
necesary_metrics<-c("uniquePurchases")
necesary_dimensions<-c("productName",'campaign')
segments<-ga_segment_list() %>% filter(str_detect(segments$name,regex('hernia',ignore_case = TRUE)))
seg_obj1 <- segment_ga4("vista de noticia de hernia",segment_id = "gaid::PHx_GDFITbyQiBok4RwQSA")
seg_obj2 <- segment_ga4("Banner Hernia Abdominal",segment_id = "gaid::8RVNUJPRTZafc9Fo-7rbpw")

#parte 1 email

#reserva doctores canal email
Reservas_4_doctores_email<-google_analytics(clinica_id,
                                            date_range = c(start_date,end_date),
                                            metrics = necesary_metrics,
                                            dimensions =necesary_dimensions,
                                            anti_sample = TRUE,
                                            filtersExpression ='ga:productName=~(agustin.+alvarez|max.+buchheister|carlos.+diaz|gabriel .+garcia);ga:uniquePurchases>0;ga:sourceMedium=~eloqua')
#reserva doctores email hernia
Reservas_4_doctores_email_hernia<-filter(Reservas_4_doctores_hernia,str_detect(Reservas_4_doctores_hernia$campaign,regex('hernia',ignore_case = TRUE)))

#sesiones email
sesiones_email<-google_analytics(clinica_id,
                                 date_range = c(start_date,end_date),
                                 metrics = c('sessions'),
                                 dimensions =c('campaign'),
                                 anti_sample = TRUE,
                                 filtersExpression ='ga:sourceMedium=~eloqua')
#sesiones email hernia
sesiones_email_hernias<-sesiones_email %>% filter(str_detect(campaign,regex('hernia',ignore_case = TRUE)))

#parte 2 google ads

#reserva doctores canal paidsearch
Reservas_doctores_paidsearch_hernia<-google_analytics(clinica_id,
                                                      date_range = c(start_date,end_date),
                                                      metrics = necesary_metrics,
                                                      dimensions =necesary_dimensions,
                                                      anti_sample = TRUE,
                                                      filtersExpression ='ga:campaign=~CLINICA SANTA MARIA_TRAFICO_HERNIA PARED ABDOMINAL_SEM_AO;ga:uniquePurchases>0;ga:sourceMedium=~cpc')
#reserva doctores paidsearch hernia
Reservas_4_doctores_paidsearch_hernia<-filter(Reservas_doctores_paidsearch_hernia,str_detect(productName,regex('(agustin.+alvarez|max.+buchheister|carlos.+diaz|gabriel .+garcia)',ignore_case = TRUE)))

#sesiones campa??a hernia
sesiones_cpc_hernia<-google_analytics(clinica_id,
                                      date_range = c(start_date,end_date),
                                      metrics = c('sessions'),
                                      dimensions =c('campaign'),
                                      anti_sample = TRUE,
                                      filtersExpression ='ga:campaign=~CLINICA SANTA MARIA_TRAFICO_HERNIA PARED ABDOMINAL_SEM_AO')

#parte 3
#reserva doctores paidsearch Campa??a Reserva Hora
Reservas_doctores_paidsearch_Reserva_hora<-google_analytics(clinica_id,
                                                            date_range = c(start_date,end_date),
                                                            metrics = necesary_metrics,
                                                            dimensions =necesary_dimensions,
                                                            anti_sample = TRUE,
                                                            filtersExpression ='ga:campaign=~CLINICA SANTA MARIA_TRAFICO_RESERVA DE HORA_SEM_AO;ga:uniquePurchases>0;ga:sourceMedium=~cpc')
#reserva 4 doctores paidsearch Campa??a reserva de hora
Reservas_4_doctores_paidsearch_Reserva_hora<-filter(Reservas_doctores_paidsearch_Reserva_hora,str_detect(productName,regex('(agustin.+alvarez|max.+buchheister|carlos.+diaz|gabriel .+garcia)',ignore_case = TRUE)))

#sesiones campa??a reserva hora
sesiones_cpc_hernia<-google_analytics(clinica_id,
                                      date_range = c(start_date,end_date),
                                      metrics = c('sessions'),
                                      dimensions =c('campaign'),
                                      anti_sample = TRUE,
                                      filtersExpression ='ga:campaign=~CLINICA SANTA MARIA_TRAFICO_RESERVA DE HORA_SEM_AO')

