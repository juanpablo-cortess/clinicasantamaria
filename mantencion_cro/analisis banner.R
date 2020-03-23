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
start_date<-"2020-02-01"
end_date<-"2020-03-21"
necesary_dimensions<-c("eventLabel",'date')
necesary_metrics<-c("totalEvents",'uniqueEvents')

#reserva doctores canal email
banners<-google_analytics(clinica_id,
                                            date_range = c(start_date,end_date),
                                            metrics = necesary_metrics,
                                            dimensions =necesary_dimensions,
                                            anti_sample = TRUE,
                                            filtersExpression ='ga:eventCategory==slideHome') %>%
  mutate(mes=case_when(
    date>='2020-03-01'~'marzo',
    TRUE ~ 'febrero'
  ))

#corregir codificación
banners$eventLabel<-str_replace_all(banners$eventLabel,'ñ','n')
banners$eventLabel<-str_replace_all(banners$eventLabel,'á','a')
banners$eventLabel<-str_replace_all(banners$eventLabel,'ó','o')


#click totales por día
clicks_totales_diarios<-banners %>% group_by(date) %>% summarize(clicks_totales = sum(totalEvents,na.rm = TRUE))
  ggplot(clicks_totales_diarios,aes(date,clicks_totales))+ geom_line(colour='black')+labs(subtitle="desde 01/02 al 21/03", 
                                                                                          y="clicks totales banners", 
                                                                                          x="fecha", 
                                                                                          title="Clicks totales Banner por dia", 
                                                                                          caption="Source: Google Analytics API")
  ggsave('mantencion_cro/figs/clicks_totales_mes.png')
  

#gráficos
banners_marzo<-filter(banners,date>='2020-03-01' & totalEvents>50)

#banners marzo
ggplot(banners_marzo,aes(date,totalEvents,))+ geom_line(aes(colour=factor(eventLabel)))+theme(legend.position = 'bottom')+labs(subtitle="Banners con mas de 50 clicks desde 01/02 al 21/03", 
                                                                                                                               y="clicks totales banners", 
                                                                                                                               x="fecha", 
                                                                                                                               title="Clicks totales Banner por dia Marzo", 
                                                                                                                               caption="Source: Google Analytics API")
ggsave('mantencion_cro/figs/clicks_banner_marzo.png')
#banners total
banners %>% filter(totalEvents>50) %>% 
  ggplot(aes(date,totalEvents,))+ 
  geom_line(aes(colour=factor(eventLabel)))+
  theme(legend.position = 'bottom')+
  labs(subtitle="Banners con mas de 50 clicks desde 01/02 al 21/03",
       y="clicks totales banners", 
       x="fecha", 
       title="Clicks totales Banner por dia", 
       caption="Source: Google Analytics API")
#Gráfico clicks banner
ggsave('mantencion_cro/figs/clicks_banner.png')
