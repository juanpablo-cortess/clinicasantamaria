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
start_date<-"2019-02-01"
end_date<-"2020-02-29"
necesary_dimensions<-c('yearMonth','deviceCategory')
necesary_metrics<-c("users")

#usarios 
usuarios<-google_analytics(clinica_id,
                          date_range = c(start_date,end_date),
                          metrics = necesary_metrics,
                          dimensions =necesary_dimensions,
                          anti_sample = TRUE)


#gráficos
ggplot(usuarios,aes(yearMonth,users,fill=factor(deviceCategory)))+geom_bar(stat="identity")+
  theme(legend.position = 'bottom')+
  labs(subtitle="desde 01/02/2019 al 29/02/2020",
       y="total usuarios", 
       x="fecha", 
       title="Evolucion de usuarios", 
       caption="Source: Google Analytics API")+scale_fill_discrete(name = "Categoria de dispositivo")

ggsave('mantencion_cro/figs/usuarios_022019_022020.png')
