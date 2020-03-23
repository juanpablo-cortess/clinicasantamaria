library(searchConsoleR)
library(tm)
library(wordcloud)
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
#se instala para hacer peek que es un head de matrices
library(futile.matrix)
#libreria de paleta de colores
library(RColorBrewer)
#libreria de text mining
library(tm)
#libreria de nude de palabras
library(wordcloud)
scr_auth()
#para revisar que urls estan en la cuenta
list_websites()

#parametros
start_date<-"2020-02-01" 
end_date<-Sys.Date()-3 #la data de search console es confiable desde 3 d??as atras
website<-'https://www.clinicasantamaria.cl/ '
download_dimensions<-c('query')
type<-c('web','video','image')
nbrow<-5000 #el m??ximo es 5000

#quitar notición cientifica
options(scipen = 999)

#queries desde 90 d??as atras
queries_page <- search_analytics(siteURL=website,
                                 startDate = start_date, #Start of analysis period (90 days before the end date)
                                 endDate =end_date , #data given in search console is from 3 days ago, so we collect data from -3 days
                                 dimensions = download_dimensions, #There are 6 dimensions you can dowload: date, searchAppearance, query, page, device, country
                                 #searchType = c('image'), #Optional, default is "web"
                                 rowLimit = nbrow,
                                 dimensionFilterExp = c('country==CHL'),#dimensionFilterExp = c("device==DESKTOP","country==GBR"), #optional
                                 walk_data = "byBatch")

#correccion terminos
queries_page$query<-str_replace_all(queries_page$query,'ñ','n')
queries_page$query<-str_replace_all(queries_page$query,'í','i')
queries_page$query<-str_replace_all(queries_page$query,'é','e')
queries_page$query<-str_replace_all(queries_page$query,'á','a')
queries_page$query<-str_replace_all(queries_page$query,'ó','o')
queries_page$query<-str_replace_all(queries_page$query,'ú','u')

#terminos de busqueda con marca palabras de marca
queries_page_branded<-filter(queries_page,str_detect(queries_page$query,regex('clinica santa maria|santa maria|csm',ignore_case = TRUE)))


#terminos de busqueda SIN marca palabras de marca, se niega el vector str_detec obteniendose el complemento
queries_page_non_branded<-filter(queries_page,!str_detect(queries_page$query,regex('clinica santa maria|santa maria|csm',ignore_case = TRUE)))

#WordCloud queries generales
corpus<-Corpus(VectorSource(queries_page$query)) #%>% tm_map(removeNumbers) %>% tm_map(removePunctuation) %>% tm_map(stripWhitespace) %>% tm_map(stopwords="spanish")
tdm<-TermDocumentMatrix(corpus)
m<-as.matrix(tdm)

#WordCloud queries branded
corpus_branded<-Corpus(VectorSource(queries_page_branded$query)) #%>% tm_map(removeNumbers) %>% tm_map(removePunctuation) %>% tm_map(stripWhitespace) %>% tm_map(stopwords="spanish")
tdm_branded<-TermDocumentMatrix(corpus_branded)
m_branded<-as.matrix(tdm_branded)

#WordCloud queries non branded
corpus_non_branded<-Corpus(VectorSource(queries_page_non_branded$query)) #%>% tm_map(removeNumbers) %>% tm_map(removePunctuation) %>% tm_map(stripWhitespace) %>% tm_map(stopwords="spanish")
tdm_non_branded<-TermDocumentMatrix(corpus_non_branded)
m_non_branded<-as.matrix(tdm_non_branded)


#se crea matriz diagonal con factores para ponderar cada busqueda por las impresiones
diag_query<-diag(queries_page$impressions)
m_impressions<-m%*%diag_query

#se crea matriz diagonal con factores para ponderar cada busqueda BRANDED por las impresiones
diag_query_branded<-diag(queries_page_branded$impressions)
m_impressions_branded<-m_branded%*%diag_query_branded

#se crea matriz diagonal con factores para ponderar cada busqueda NON BRANDED por las impresiones
diag_query_non_branded<-diag(queries_page_non_branded$impressions)
m_impressions_non_branded<-m_non_branded%*%diag_query_non_branded

#suma lateral de los las palabras
v<-sort(rowSums(m),decreasing=TRUE)
v_imp<-sort(rowSums(m_impressions),decreasing = TRUE)

#suma lateral de los las palabras BRANDED
v_imp_branded<-sort(rowSums(m_impressions_branded),decreasing = TRUE)

#suma lateral de los las palabras NON BRANDED
v_imp_non_branded<-sort(rowSums(m_impressions_non_branded),decreasing = TRUE)

#creaci??n de data frame de palabras vs freq
d_imp<-data.frame(terminos=names(v_imp),freq=v_imp)
d<-data.frame(word=names(v),freq=v)

#creaci??n de data frame de palabras vs freq BRANDED
d_imp_branded<-data.frame(terminos=names(v_imp_branded),freq=v_imp_branded)

#creaci??n de data frame de palabras vs freq NON BRANDED
d_imp_non_branded<-data.frame(terminos=names(v_imp_non_branded),freq=v_imp_non_branded)

#funci??n para graficar la nube de palabras
word_cloud<-wordcloud(d$word,d$freq,random.order = FALSE,max.words = 100, rot.per = 0.3, scale=c(6,.8), colors = brewer.pal(7,"Dark2"))
word_cloud_imp<-wordcloud(d_imp$terminos,d_imp$freq,random.order = FALSE,max.words = 100, rot.per = 0.3, scale=c(6,.8),colors = brewer.pal(8,"Dark2"))

#funci??n para graficar la nube de palabras BRANDED y barplot
word_cloud_imp_branded<-wordcloud(d_imp_branded$terminos,d_imp_branded$freq,random.order = FALSE,max.words = 100, rot.per = 0.3, scale=c(6,.8),colors = brewer.pal(8,"Dark2"))
barplot_branded<-barplot(d_imp_branded$freq[1:10],names.arg = d_imp_branded$terminos[1:10],las=2,col="lightblue")

#funci??n para graficar la nube de palabras NON BRANDED y barplot
word_cloud_imp_non_branded<-wordcloud(d_imp_non_branded$terminos,d_imp_non_branded$freq,random.order = FALSE,max.words = 100, rot.per = 0.3, scale=c(6,.8),colors = brewer.pal(8,"Dark2"))
barplot_non_branded<-barplot(d_imp_non_branded$freq[1:10],names.arg = d_imp_non_branded$terminos[1:10],las=2,col="lightblue")
