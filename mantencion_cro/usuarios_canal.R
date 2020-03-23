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
necesary_dimensions<-c("channelGrouping",'date','')
necesary_metrics<-c("sessions",'users')

#reserva doctores canal email
comportamiento_canal<-google_analytics(clinica_id,
                          date_range = c(start_date,end_date),
                          metrics = necesary_metrics,
                          dimensions =necesary_dimensions,
                          anti_sample = TRUE) %>%
  mutate(mes=case_when(
    date>='2020-03-01'~'marzo',
    TRUE ~ 'febrero'
  ))

#corregir codificación
banners$eventLabel<-str_replace_all(banners$eventLabel,'ñ','n')
banners$eventLabel<-str_replace_all(banners$eventLabel,'á','a')
banners$eventLabel<-str_replace_all(banners$eventLabel,'ó','o')

comportamiento_canal %>% 
  group_by(channelGrouping) %>%
  summarise(users=sum(users)) %>%
  ggplot(aes(x=channelGrouping, y=users)) +
  geom_bar(stat="identity",aes(fill=factor(channelGrouping)))+
  labs(subtitle="desde 01/02 al 21/03",
       y="Usuarios", 
       x="Canal", 
       title="Usuarios del sitio web", 
       caption="Source: Google Analytics API")+
  theme(legend.position = "none")

ggsave('mantencion_cro/figs/usuarios_canal.png')
