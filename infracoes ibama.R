library(tidyverse)
infracoes = read.csv("C:\\Users\\rafae\\Downloads\\auto_infracao.csv",sep = ';')
biomas = read.csv("C:\\Users\\rafae\\Downloads\\bioma.csv",sep = ';')

infracoes = infracoes %>%  
  #select(DAT_HORA_AUTO_INFRACAO) %>% 
  mutate(data = strptime(substring(DAT_HORA_AUTO_INFRACAO,1,10),"%d/%m/%Y"),
         ano=lubridate::year(data),
         mes=lubridate::month(data)) %>% 
  left_join(biomas %>% mutate(SEQ_AUTO_INFRACAO=as.character(SEQ_AUTO_INFRACAO)))

infracoes = infracoes%>% 
  filter(between(ano,1981,2022))

table(infracoes$ano)
# infracoes%>% 
#   group_by(ano) %>% dplyr::sample_n(size = 10) %>% View()
#   count(ano)
#   count(MUNICIPIO,UF,sort=T)
infracoes %>% head(20) %>% View()

infracoes %>% filter(MUNICIPIO == "DOIS VIZINHOS")
write.csv(infracoes,"infracoesIbama.csv",row.names = F)

infracoes %>% left_join(biomas %>% mutate(SEQ_AUTO_INFRACAO=as.character(SEQ_AUTO_INFRACAO))) %>% 
  filter(MUNICIPIO == "PORTO VELHO" & is.na(BIOMA)) %>% count(ano)
  filter(BIOMA == "Amazonia") %>% 
  filter(ano == 2022) %>% 
  count(mes)
  write.csv()
