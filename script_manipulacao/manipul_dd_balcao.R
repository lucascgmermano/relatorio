pacman::p_load(data.table, dplyr, here, lubridate, janitor)

dd_comp <- fread('dados/dados_balcao.csv', encoding = "UTF-8") %>% clean_names()

dd_comp[is.na(dt_sin_pri), "dt_sin_pri"] <- dd_comp[is.na(dt_sin_pri), "dt_notific"]
dd_comp[is.na(dt_notific), "dt_notific"] <- dd_comp[is.na(dt_notific), "dt_sin_pri"]

########## Dados 2022 ############
pop <- read.csv2(here('dados', 'pop.csv'), encoding = "UTF-8")

dd22 <- dd_comp %>% 
  filter(lubridate::year(dt_sin_pri)==2022) %>% 
  select("sistema_origem","dt_sin_pri","cs_sexo",      
         "dt_nasc_informada","id_mn_resi", "evolucao","dt_notific") %>%
  setNames(c('internou', 'data', 'sexo', 'dt_nasc', 'muni', 'obito',"dt_notific")) %>% 
  mutate(internou =
           recode(internou,'1_SIVEP'=1, '2_ESUS'=0, .default = 0),                            
         obito = if_else(obito=='OBITO', 1, 0, missing = 0),
         ano = format(data, '%Y'),                                              
         mes = format(data, '%b'),                                              
         semana = lubridate::week(data),
         idade =  round(time_length(difftime(time1 = dt_notific, time2 = dt_nasc), 
                                    unit = "years"), digits = 0),
         faixa = cut(idade, breaks = c(-Inf,4,9,14,19,29,39,49,59,69,79,Inf),
                     labels = c("0 a 4","5 a 9","10 a 14","15 a 19","20 a 29",
                                "30 a 39","40 a 49","50 a 59","60 a 69",
                                "70 a 79","80 ou mais")),
         muni = case_when(muni=='AGUAI'~'Aguaí',
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
                          muni=='VARGEM GRANDE DO SUL'~'Vargem Grande do Sul')) %>% 
  na.omit() %>% 
  left_join(pop[,-3])

colSums(is.na(dd22))

write.csv2(x = dd22, file = here('dados', 'dd22.csv'))


########## Dados 2021 ############
dd <- dd_comp %>% 
  filter(lubridate::year(dt_sin_pri)==2021) %>% 
  select("sistema_origem","dt_sin_pri","cs_sexo",      
                         "dt_nasc_informada","id_mn_resi", "evolucao","dt_notific") %>%
  setNames(c('internou', 'data', 'sexo', 'dt_nasc', 'muni', 'obito',"dt_notific")) %>% 
  mutate(internou =
           recode(internou,'1_SIVEP'=1, '2_ESUS'=0, .default = 0),                            
         obito = if_else(obito=='OBITO', 1, 0, missing = 0),
         ano = format(data, '%Y'),                                              
         mes = format(data, '%b'),                                              
         semana = lubridate::week(data),
         idade =  round(time_length(difftime(time1 = dt_notific, time2 = dt_nasc), 
                                    unit = "years"), digits = 0),
         faixa = cut(idade, breaks = c(-Inf,4,9,14,19,29,39,49,59,69,79,Inf),
                     labels = c("0 a 4","5 a 9","10 a 14","15 a 19","20 a 29",
                                "30 a 39","40 a 49","50 a 59","60 a 69",
                                "70 a 79","80 ou mais")),
         muni = case_when(muni=='AGUAI'~'Aguaí',
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
                          muni=='SANTA CRUZ DAS PALMEIRAS'~'Santo Antônio do Jardim',
                          muni=='SANTO ANTONIO DO JARDIM'~'Santa Cruz das Palmeiras',
                          muni=='SAO JOAO DA BOA VISTA'~'São João da Boa Vista',
                          muni=='SAO JOSE DO RIO PARDO'~'São José do Rio Pardo',
                          muni=='SAO SEBASTIAO DA GRAMA'~'São Sebastião da Grama',
                          muni=='TAMBAU'~'Tambaú',
                          muni=='TAPIRATIBA'~'Tapiratiba',
                          muni=='VARGEM GRANDE DO SUL'~'Vargem Grande do Sul')) %>% 
  na.omit() %>% 
  left_join(pop[,-3])

colSums(is.na(dd))

write.csv2(x = dd, file = 'dados/dd.csv')

########## Dados 2020 ############
pop <- read.csv2(here('dados', 'pop.csv'), encoding = "UTF-8")

dd20 <- dd_comp %>% 
  filter(lubridate::year(dt_sin_pri)==2020) %>% 
  select("sistema_origem","dt_sin_pri","cs_sexo",      
         "dt_nasc_informada","id_mn_resi", "evolucao","dt_notific") %>%
  setNames(c('internou', 'data', 'sexo', 'dt_nasc', 'muni', 'obito',"dt_notific")) %>% 
  mutate(internou =
           recode(internou,'1_SIVEP'=1, '2_ESUS'=0, .default = 0),                            
         obito = if_else(obito=='OBITO', 1, 0, missing = 0),
         ano = format(data, '%Y'),                                              
         mes = format(data, '%b'),                                              
         semana = lubridate::week(data),
         idade =  round(time_length(difftime(time1 = dt_notific, time2 = dt_nasc), 
                                    unit = "years"), digits = 0),
         faixa = cut(idade, breaks = c(-Inf,4,9,14,19,29,39,49,59,69,79,Inf),
                     labels = c("0 a 4","5 a 9","10 a 14","15 a 19","20 a 29",
                                "30 a 39","40 a 49","50 a 59","60 a 69",
                                "70 a 79","80 ou mais")),
         muni = case_when(muni=='AGUAI'~'Aguaí',
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
                          muni=='SANTA CRUZ DAS PALMEIRAS'~'Santo Antônio do Jardim',
                          muni=='SANTO ANTONIO DO JARDIM'~'Santa Cruz das Palmeiras',
                          muni=='SAO JOAO DA BOA VISTA'~'São João da Boa Vista',
                          muni=='SAO JOSE DO RIO PARDO'~'São José do Rio Pardo',
                          muni=='SAO SEBASTIAO DA GRAMA'~'São Sebastião da Grama',
                          muni=='TAMBAU'~'Tambaú',
                          muni=='TAPIRATIBA'~'Tapiratiba',
                          muni=='VARGEM GRANDE DO SUL'~'Vargem Grande do Sul')) %>% 
  na.omit() %>% 
  left_join(pop[,-3])

colSums(is.na(dd20))

write.csv2(x = dd20, file = here('dados', 'dd20.csv'))


