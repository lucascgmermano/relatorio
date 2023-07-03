################################# Pacotes ######################################
pacman::p_load(dplyr,tidyr,readr,ggplot2,data.table,lubridate,here)

setwd("~/Documentos/ministerio")

################## Compilacao dos bancos SIVEP 2020 a 2022 #####################

lista <- list.files(path = here('bancos_brutos','sivep_bruto_2020a2022'),
                    pattern = '.csv')


# Leitura dos arquivos da lista

  ms <- list(rep(NA, length(lista)))
  y <- 1

  for (i in lista) {
    ms[[y]] <- fread(here('bancos_brutos','sivep_bruto_2020a2022',i), 
                     encoding = "UTF-8") %>% 
      filter(CLASSI_FIN == 5 &
               ID_RG_RESI=="GVE XXVI SAO JOAO DA BOA VISTA") %>% 
      select(DT_SIN_PRI,CO_MUN_RES,ID_MN_RESI,
             NU_IDADE_N,CS_SEXO,EVOLUCAO) %>% 
      setNames(c("data","code_mn","muni","idade","sexo","evolucao")) %>%
      mutate(faixa = cut(idade, breaks = c(-Inf,4,9,14,19,29,39,49,59,69,79,Inf),
                         labels = c("0 a 4","5 a 9","10 a 14","15 a 19","20 a 29",
                                    "30 a 39","40 a 49","50 a 59","60 a 69",
                                    "70 a 79","80 ou mais")),
             evolucao = if_else(condition = evolucao == 2, 
                                true = 1, false = 0, missing = 0),
             data = dmy(data),
             muni = case_when(muni == "AGUAI" ~ "Aguaí",                   
                              muni == "AGUAS DA PRATA" ~ "Águas da Prata",          
                              muni == "CACONDE" ~ "Caconde",
                              muni == "CASA BRANCA" ~ "Casa Branca",
                              muni == "DIVINOLANDIA" ~ "Divinolândia",             
                              muni == "ESPIRITO SANTO DO PINHAL" ~ "Espírito Santo do Pinhal", 
                              muni == "ESTIVA GERBI" ~ "Estiva Gerbi",
                              muni == "ITAPIRA" ~ "Itapira",
                              muni == "ITOBI" ~ "Itobi", 
                              muni == "MOCOCA" ~ "Mococa",         
                              muni == "MOGI GUACU" ~ "Mogi Guaçu",      
                              muni == "MOJI MIRIM" ~ "Mogi Mirim",             
                              muni == "SANTA CRUZ DAS PALMEIRAS" ~ "Santa Cruz das Palmeiras",
                              muni == "SANTO ANTONIO DO JARDIM" ~ "Santo Antônio do Jardim",
                              muni == "SAO JOAO DA BOA VISTA" ~ "São João da Boa Vista",   
                              muni == "SAO JOSE DO RIO PARDO" ~ "São José do Rio Pardo",  
                              muni == "SAO SEBASTIAO DA GRAMA" ~ "São Sebastião da Grama",  
                              muni == "TAMBAU" ~ "Tambaú",                   
                              muni == "TAPIRATIBA" ~ "Tapiratiba",               
                              muni == "VARGEM GRANDE DO SUL" ~ "Vargem Grande do Sul"),
             sexo = case_when(sexo == "M" ~ "Masculino",
                              sexo == "F" ~ "Feminino"))
    y <- y+1
    }


# Merge compilando os anos do sivep
  sivep2020_2022 <- rbindlist(ms)                                               # Merge

######################### Salvando bancos SIVEP compilados #####################

# Compilados SIVEP 2020
write.csv2(x = sivep2020_2022[year(sivep2020_2022$data)==2020,], 
            file = here('bancos_lotes_compi',
                         'sivep_2020_compi.csv'), 
            fileEncoding = "UTF-8")                                             

# Compilados SIVEP 2021
write.csv2(x = sivep2020_2022[year(sivep2020_2022$data)==2021,], 
             file = here('bancos_lotes_compi',
                         'sivep_2021_compi.csv'), 
             fileEncoding = "UTF-8")                                            
  
# Compilados SIVEP 2022
write.csv2(x = sivep2020_2022[year(sivep2020_2022$data)==2022,], 
           file = here('bancos_lotes_compi',
                       'sivep_2022_compi.csv'), 
           fileEncoding = "UTF-8")                                             

beepr::beep(sound = 8)
rm(list = ls())
