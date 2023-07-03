################################# Pacotes ######################################
pacman::p_load(dplyr,tidyr,data.table,lubridate,here)

setwd("~/Documentos/ministerio")

######################### Definicao de bancos por ano ##########################

# Dados 2020
  dados2020 <- merge.data.table(x = fread('bancos_lotes_compi/esus_2020_compi.csv'),
                                y = fread('bancos_lotes_compi/sivep_2020_compi.csv'),
                   all = TRUE) %>%
    pivot_wider(names_from = 'sexo',
                values_from = 'sexo',
                values_fn = list('sexo' = length)) %>%
    group_by(data,code_mn,muni,faixa) %>%
    summarise(casos = n(),
              obitos = sum(evolucao, na.rm = TRUE),
              masc = sum(Masculino, na.rm = TRUE),
              fem = sum(Feminino, na.rm = TRUE)) %>%
    mutate(ano = year(data),
           mes = month(data),
           semana = week(data)) %>%
    filter(ano == 2020) %>%
    left_join(fread(here('dados_supl','pop.csv')))


  write.csv2(dados2020,
             file = here('bancos_finais_por_ano','dados2020.csv'),
             fileEncoding = "UTF-8")


# Dados 2021

  dados2021 <- merge.data.table(x = fread('bancos_lotes_compi/esus_2021_compi.csv'),
                                y = fread('bancos_lotes_compi/sivep_2021_compi.csv'),
                                all = TRUE) %>%
    pivot_wider(names_from = 'sexo',
                values_from = 'sexo',
                values_fn = list('sexo' = length)) %>%
    group_by(data,code_mn,muni,faixa) %>%
    summarise(casos = n(),
              obitos = sum(evolucao, na.rm = TRUE),
              masc = sum(Masculino, na.rm = TRUE),
              fem = sum(Feminino, na.rm = TRUE)) %>%
    mutate(ano = year(data),
           mes = month(data),
           semana = week(data)) %>%
    filter(ano == 2021) %>%
    left_join(fread(here('dados_supl','pop.csv')))

  write.csv2(dados2021,
             file = here('bancos_finais_por_ano','dados2021.csv'),
             fileEncoding = "UTF-8")

# Dados 2022
dados2022 <- merge.data.table(x = fread('bancos_lotes_compi/esus_2022_compi.csv'),
                              y = fread('bancos_lotes_compi/sivep_2022_compi.csv'),
                              all = TRUE) %>%
  pivot_wider(names_from = 'sexo',
              values_from = 'sexo',
              values_fn = list('sexo' = length)) %>% 
  group_by(data,code_mn,muni,faixa) %>% 
  summarise(casos = n(),
            obitos = sum(evolucao, na.rm = TRUE),
            masc = sum(Masculino, na.rm = TRUE),
            fem = sum(Feminino, na.rm = TRUE)) %>% 
  mutate(ano = year(data),
         mes = month(data),
         semana = week(data)) %>% 
  filter(ano == 2022) %>% 
  left_join(fread(here('dados_supl','pop.csv')))

write.csv2(dados2022,
           file = here('bancos_finais_por_ano','dados2022.csv'))


# Dados 2023
dados2023 <- fread(file = file.path("bancos_finais_por_ano","dados2023.csv"))

##################### Salvando no projeto Relatorio ############################

write.csv2(dados2020,
           file = '~/Documentos/relatorio/dados/dados2020.csv')

write.csv2(dados2021,
           file = '~/Documentos/relatorio/dados/dados2021.csv')

write.csv2(dados2022,
           file = '~/Documentos/relatorio/dados/dados2022.csv')

rbind(dados2020,dados2021,dados2022,dados2023) %>%
  write.csv2(file = '~/Documentos/relatorio/dados/todos_anos.csv')

beepr::beep(sound = 8)
rm(list = ls())

