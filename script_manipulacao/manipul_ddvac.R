pacman::p_load(dplyr, lubridate, data.table, janitor, here, readr)

ddvac <- fread(here('dados','vacivida.csv'), encoding = 'UTF-8') %>% clean_names()

ddvac <- ddvac %>% clean_names() %>%
  filter(    municipio_residencia_paciente=='AGUAI'|
             municipio_residencia_paciente=='AGUAS DA PRATA'|
             municipio_residencia_paciente=='CACONDE'|
             municipio_residencia_paciente=='CASA BRANCA'|
             municipio_residencia_paciente=='DIVINOLANDIA'|
             municipio_residencia_paciente=='ESPIRITO SANTO DO PINHAL'|
             municipio_residencia_paciente=='ESTIVA GERBI'|
             municipio_residencia_paciente=='ITAPIRA'|
             municipio_residencia_paciente=='ITOBI'|
             municipio_residencia_paciente=='MOCOCA'|
             municipio_residencia_paciente=='MOGI MIRIM'|
             municipio_residencia_paciente=='MOGI GUACU'|
             municipio_residencia_paciente=='SANTA CRUZ DAS PALMEIRAS'|
             municipio_residencia_paciente=='SANTO ANTONIO DO JARDIM'|
             municipio_residencia_paciente=='SAO JOAO DA BOA VISTA'|
             municipio_residencia_paciente=='SAO JOSE DO RIO PARDO'|
             municipio_residencia_paciente=='SAO SEBASTIAO DA GRAMA'|
             municipio_residencia_paciente=='TAMBAU'|
             municipio_residencia_paciente=='TAPIRATIBA'|
             municipio_residencia_paciente=='VARGEM GRANDE DO SUL')  %>%
  mutate(v1=NULL,
  data_nascimento_paciente=NULL,
  sexo_paciente=NULL,
  gve=NULL) %>% 
  setNames(c("idade","gestante","puerpera","grupo_at","muni","imunobiologico",
             "dose","data","dose_adicional")) %>% 
  mutate( muni = case_when(muni=='AGUAI'~'Aguaí',
                           muni=='AGUAS DA PRATA'~'Águas da Prata',
                           muni=='CACONDE'~'Caconde',
                           muni=='CASA BRANCA'~'Casa Branca',
                           muni=='DIVINOLANDIA'~'Divinolândia',
                           muni=='ESPIRITO SANTO DO PINHAL'~'Espírito Santo do Pinhal',
                           muni=='ESTIVA GERBI'~'Estiva Gerbi',
                           muni=='ITAPIRA'~'Itapira',
                           muni=='ITOBI'~'Itobi',
                           muni=='MOCOCA'~'Mococa',
                           muni=='MOGI GUACU'~'Mogi Guaçu',
                           muni=='MOGI MIRIM'~'Mogi Mirim',
                           muni=='SANTA CRUZ DAS PALMEIRAS'~'Santa Cruz das Palmeiras',
                           muni=='SANTO ANTONIO DO JARDIM'~'Santo Antônio do Jardim',
                           muni=='SAO JOAO DA BOA VISTA'~'São João da Boa Vista',
                           muni=='SAO JOSE DO RIO PARDO'~'São José do Rio Pardo',
                           muni=='SAO SEBASTIAO DA GRAMA'~'São Sebastião da Grama',
                           muni=='TAMBAU'~'Tambaú',
                           muni=='TAPIRATIBA'~'Tapiratiba',
                           muni=='VARGEM GRANDE DO SUL'~'Vargem Grande do Sul'),
          idade=as.numeric(idade),
          data = as.Date(dmy(data)),
          semana = lubridate::week(data),
          mes = lubridate::month(data, label=T),
          cat.pop.etaria=case_when(idade<5 ~ "00a04",
                                   idade>4 & idade<=9 ~ "05 a 09",
                                   idade>9 & idade<=14 ~ "10 a 14",
                                   idade>14 & idade<=19 ~ "15 a 19",
                                   idade>19 & idade<=29 ~ "20 a 29",
                                   idade>29 & idade<=39 ~ "30 a 39",
                                   idade>39 & idade<=49 ~ "40 a 49",
                                   idade>49 & idade<=59 ~ "50 a 59",
                                   idade>59 & idade<=69 ~ "60 a 69",
                                   idade>69 ~ "70 ou mais"),
          dose=recode(dose, '1'='d1', '2'='d2', '3'='d3'))
  

x <- ddvac[ddvac$imunobiologico=='JANSSEN',]
ddvac[ddvac$imunobiologico == 'JANSSEN', "dose"] <- 'd2'
ddvac <- rbind(ddvac, x)
rm(x)

write_csv2(x = ddvac, file = here('dados','ddvac.csv'))
