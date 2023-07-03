################################# Pacotes ######################################
pacman::p_load(dplyr,tidyr,readr,ggplot2,data.table,lubridate,here,janitor)

setwd("~/Documentos/ministerio")

# ############################ Compilacao Esus 2020 ##############################
# 
# lista <- list.files(path = here('bancos_brutos','esus_bruto_2020'),
#                       pattern = '.csv')
# 
# # Leitura dos arquivos da lista
# 
#   ms <- list(rep(NA, length(lista)))
#   y <- 1
# 
#   for (i in lista) {
#     ms[[y]] <- fread(file = here('bancos_brutos','esus_bruto_2020',i),
#     encoding = "UTF-8") %>% 
#       filter(classificacaoFinal %in% 
#  c("Confirmado Clínico-Epidemiológico","Confirmado Laboratorial",
#    "Confirmado por Critério Clínico","Confirmado Clínico-Imagem") &
#      municipio %in% 
#  c("Aguaí","Águas da Prata","Caconde","Casa Branca","Divinolândia",
#    "Espírito Santo do Pinhal","Estiva Gerbi","Itapira","Itobi",
#    "Mococa","Mogi Guaçu","Mogi Mirim","Santa Cruz das Palmeiras",
#    "Santo Antônio do Jardim","São João da Boa Vista","São José do Rio Pardo",
#    "São Sebastião da Grama","Tambaú","Tapiratiba","Vargem Grande do Sul")) %>%
#       select(dataInicioSintomas,municipio,idade,sexo,evolucaoCaso,dataNotificacao) %>% 
#       setNames(c("data","muni","idade","sexo","evolucao","dataNotificacao")) %>%
#       mutate(faixa = cut(idade, breaks = c(-Inf,4,9,14,19,29,39,49,59,69,79,Inf),
#                      labels = c("0 a 4","5 a 9","10 a 14","15 a 19","20 a 29",
#                                 "30 a 39","40 a 49","50 a 59","60 a 69",
#                                 "70 a 79","80 ou mais")),
#              evolucao = ifelse(evolucao != "Óbito", 0, 1),
#              code_mn = case_when( muni =='Aguaí'~'350030',
#                                   muni=='Águas da Prata'~'350040',
#                                   muni=='Caconde'~'350870',
#                                   muni=='Casa Branca'~'351080',
#                                   muni=='Divinolândia'~'351390',
#                                   muni=='Espírito Santo do Pinhal'~'351518',
#                                   muni=='Estiva Gerbi'~'355730',
#                                   muni=='Itapira'~'352260',
#                                   muni=='Itobi'~'352380',
#                                   muni=='Mococa'~'353050',
#                                   muni=='Mogi Guaçu'~'353070',
#                                   muni=='Mogi Mirim'~'353080',
#                                   muni=='Santa Cruz das Palmeiras'~'354630',
#                                   muni=='Santo Antônio do Jardim'~'354810',
#                                   muni=='São João da Boa Vista'~'354910',
#                                   muni=='São José do Rio Pardo'~'354970',
#                                   muni=='São Sebastião da Grama'~'355080',
#                                   muni=='Tambaú'~'355330',
#                                   muni=='Tapiratiba'~'355360',
#                                   muni=='Vargem Grande do Sul'~'355640'))
#     y <- y+1
#     }
# 
# 
# # Merge e tabela por municipio MS
#   dados2020 <- rbindlist(ms)                                                    # Merge
#   
#   dados2020[is.na(data),"data"] <- dados2020[is.na(data),"dataNotificacao"]     # Eliminando NA em data
#   dados2020$dataNotificacao <- NULL
#   dados2020 <- dados2020 %>% 
#     filter(data %in% as.Date('2020-01-01'):as.Date(Sys.Date()))                 # Eliminando datas erradas
# 
#   write.csv2(x = dados2020, 
#             file = here('bancos_lotes_compi',
#                         'esus_2020_compi.csv'), fileEncoding = "UTF-8")         # Salvar dados
# 
# rm(list = ls())  
#   
# ############################# Compilacao Esus 2021 #############################
#   
#   lista <- list.files(path = here('bancos_brutos','esus_bruto_2021'), 
#                       pattern = '.csv')
#   
#   # Leitura dos arquivos da lista
#   
#   ms <- list(rep(NA, length(lista)))
#   y <- 1
#   
#   for (i in lista) {
#     ms[[y]] <- fread(file = here('bancos_brutos','esus_bruto_2021',i),
#                      encoding = "UTF-8") %>% 
#       filter(classificacaoFinal %in% 
#                c("Confirmado Clínico-Epidemiológico","Confirmado Laboratorial",
#                  "Confirmado por Critério Clínico","Confirmado Clínico-Imagem") &
#                municipio %in% 
#                c("Aguaí","Águas da Prata","Caconde","Casa Branca","Divinolândia",
#                  "Espírito Santo do Pinhal","Estiva Gerbi","Itapira","Itobi",
#                  "Mococa","Mogi Guaçu","Mogi Mirim","Santa Cruz das Palmeiras",
#                  "Santo Antônio do Jardim","São João da Boa Vista","São José do Rio Pardo",
#                  "São Sebastião da Grama","Tambaú","Tapiratiba","Vargem Grande do Sul")) %>%
#       select(dataInicioSintomas,municipio,idade,sexo,evolucaoCaso,dataNotificacao) %>% 
#       setNames(c("data","muni","idade","sexo","evolucao","dataNotificacao")) %>%
#       mutate(faixa = cut(idade, breaks = c(-Inf,4,9,14,19,29,39,49,59,69,79,Inf),
#                          labels = c("0 a 4","5 a 9","10 a 14","15 a 19","20 a 29",
#                                     "30 a 39","40 a 49","50 a 59","60 a 69",
#                                     "70 a 79","80 ou mais")),
#              evolucao = ifelse(evolucao != "Óbito", 0, 1),
#              code_mn = case_when( muni =='Aguaí'~'350030',
#                                   muni=='Águas da Prata'~'350040',
#                                   muni=='Caconde'~'350870',
#                                   muni=='Casa Branca'~'351080',
#                                   muni=='Divinolândia'~'351390',
#                                   muni=='Espírito Santo do Pinhal'~'351518',
#                                   muni=='Estiva Gerbi'~'355730',
#                                   muni=='Itapira'~'352260',
#                                   muni=='Itobi'~'352380',
#                                   muni=='Mococa'~'353050',
#                                   muni=='Mogi Guaçu'~'353070',
#                                   muni=='Mogi Mirim'~'353080',
#                                   muni=='Santa Cruz das Palmeiras'~'354630',
#                                   muni=='Santo Antônio do Jardim'~'354810',
#                                   muni=='São João da Boa Vista'~'354910',
#                                   muni=='São José do Rio Pardo'~'354970',
#                                   muni=='São Sebastião da Grama'~'355080',
#                                   muni=='Tambaú'~'355330',
#                                   muni=='Tapiratiba'~'355360',
#                                   muni=='Vargem Grande do Sul'~'355640'))
#     y <- y+1
#   }
#   
#   
#   # Merge e tabela por municipio MS
#   
#   dados2021 <- rbindlist(ms)                                                    # Merge
# 
#   dados2021[is.na(data),"data"] <- dados2021[is.na(data),"dataNotificacao"]     # Eliminando NA em data
#   dados2021$dataNotificacao <- NULL
#   dados2021 <- dados2021 %>% 
#     filter(data %in% as.Date('2020-01-01'):as.Date(Sys.Date()))                 # Eliminando datas erradas
#   
#   write.csv2(x = dados2021, 
#             file = here('bancos_lotes_compi',
#                         'esus_2021_compi.csv'),
#             fileEncoding = "UTF-8")                                             # Salvar dados
#   
# 
#   rm(list = ls())
  
############################ Compilacao Esus 2022 ##############################
# Vou deixar comentado porque estamos usando agora somente 2023

# lista <- list.files(path = here('bancos_brutos','esus_bruto_2022'),
                        # pattern = '.csv')
  
    # Leitura dos arquivos da lista
# funcao.filtrar <- function(x){
#   fread(file = here("bancos_brutos","esus_bruto_2022",x), colClasses = c(municipio = "character",
#                                  municipioIBGE = "character",
#                                  evolucaoCaso = "character",
#                                  classificacaoFinal = "character",
#                                  dataNotificacao = "Date",
#                                  dataInicioSintomas = "Date")) %>% 
#   select("sexo","dataInicioSintomas","municipio","municipioIBGE","idade",
#          "evolucaoCaso","classificacaoFinal","dataNotificacao") %>%
#   filter(municipioIBGE %in% c("3500303","3500402","3508702","3510807",
#                               "3513900","3515186","3557303","3522604",
#                               "3523800","3530508",'3530706',"3530805",
#                               "3546306",'3548104',"3549102","3549706",
#                               "3550803","3553302","3553609","3556404") 
#          &
#            classificacaoFinal %in% c("Confirmado Laboratorial",
#                                      "Confirmado Clínico-Epidemiológico",
#                                      "Confirmado por Critério Clínico",
#                                      "Confirmado Clínico-Imagem")
#          &
#            !is.na(dataInicioSintomas)) %>% 
#   mutate(dataInicioSintomas = as.Date(dataInicioSintomas, "%Y-%m-%d"),
#          faixa = cut(idade, breaks = c(-Inf,4,9,14,19,29,39,49,59,69,79,Inf),
#                      labels = c("0 a 4","5 a 9","10 a 14","15 a 19","20 a 29",
#                                 "30 a 39","40 a 49","50 a 59","60 a 69",
#                                 "70 a 79","80 ou mais")),
#          evolucaoCaso = ifelse(evolucaoCaso != "Óbito", yes = 0, no = 1)) %>%
#     setNames(c("sexo","data","muni","code_mn","idade","evolucao","classifin",
#                "dataNotificacao","faixa"))
# }

    # Leitura com a funcao e fixacao em uma lista
# arquivos <- lapply(lista, funcao.filtrar); arquivos

    # Cria arquivo final com todos os dados
# dados2022 <- rbindlist(arquivos); dados2022
#   dados <- dados2022
  # dados2022 <- dados
  
  # class(dados2022$data)
  # sum(is.na(dados2022$data))
  # teste <- fread(input = "lote10.csv")
  # colnames(teste)
  
  # dados2022[is.na(data),"data"] <- dados2022[is.na(data),"dataNotificacao"]     # Eliminando NA em data
  # dados2022$dataNotificacao <- NULL
  
  # dados2022 <- dados2022 %>% 
  #   filter(lubridate::year(data) == 2022)  %>%             
  #   mutate(data = as.Date(data, "%Y-%m-%d"),                                    # Eliminando datas erradas
  #          code_mn = substr(code_mn, start = 1, stop = 6),
  #          dataNotificacao = NULL) 
  #   
  #   
  # 
  # write.csv2(x = dados2022,
  #            file = here("bancos_lotes_compi","esus_2022_compi.csv"),
  #            fileEncoding = "UTF-8")

  # write.csv2(x = dados2022,
  #            file = "/home/costela/Documentos/relatorio/dados/dados2022.csv",
  #            fileEncoding = "UTF-8")
  

  # beepr::beep(sound = 8)  
  # rm(list = ls())
  

  
  ######################## Compilacao Esus 2023 ########################
# Esta comentado porque o OpenDatasus nao atualizou ainda para 2023
  
 #  lista <- list.files(path = here('bancos_brutos','esus_bruto_2022'),
 #                      pattern = '.csv')
 #  
 #  # Leitura dos arquivos da lista
 #  funcao.filtrar <- function(x){
 #    fread(file = here("bancos_brutos","esus_bruto_2022",x), colClasses = c(municipio = "character",
 #                                                                           municipioIBGE = "character",
 #                                                                           evolucaoCaso = "character",
 #                                                                           classificacaoFinal = "character",
 #                                                                           dataNotificacao = "Date",
 #                                                                           dataInicioSintomas = "Date")) %>% 
 #      select("sexo","dataInicioSintomas","municipio","municipioIBGE","idade",
 #             "evolucaoCaso","classificacaoFinal","dataNotificacao") %>%
 #      filter(municipioIBGE %in% c("3500303","3500402","3508702","3510807",
 #                                  "3513900","3515186","3557303","3522604",
 #                                  "3523800","3530508",'3530706',"3530805",
 #                                  "3546306",'3548104',"3549102","3549706",
 #                                  "3550803","3553302","3553609","3556404") 
 #             &
 #               classificacaoFinal %in% c("Confirmado Laboratorial",
 #                                         "Confirmado Clínico-Epidemiológico",
 #                                         "Confirmado por Critério Clínico",
 #                                         "Confirmado Clínico-Imagem")
 #             &
 #               !is.na(dataInicioSintomas)) %>% 
 #      mutate(dataInicioSintomas = as.Date(dataInicioSintomas, "%Y-%m-%d"),
 #             faixa = cut(idade, breaks = c(-Inf,4,9,14,19,29,39,49,59,69,79,Inf),
 #                         labels = c("0 a 4","5 a 9","10 a 14","15 a 19","20 a 29",
 #                                    "30 a 39","40 a 49","50 a 59","60 a 69",
 #                                    "70 a 79","80 ou mais")),
 #             evolucaoCaso = ifelse(evolucaoCaso != "Óbito", yes = 0, no = 1),
 #             municipioIBGE = substr(municipioIBGE, start = 1, stop = 6)) %>%
 #      setNames(c("sexo","data","muni","code_mn","idade","evolucao","classifin",
 #                 "dataNotificacao","faixa")) %>% 
 #      filter(year(data)==2023)
 #  }
 #  
 #  # Leitura com a funcao e fixacao em uma lista
 #  arquivos <- lapply(lista, funcao.filtrar); arquivos
 #  
 #  # Cria arquivo final com todos os dados
 #  dados2023 <- rbindlist(arquivos); dados2023
 #  
 #  
 # 
 #  dados2023[is.na(data),"data"] <- dados2023[is.na(data),"dataNotificacao"]     # Eliminando NA em data
 # 
 # 
 #  
 #  
 #  write.csv2(x = dados2023,
 #             file = here("bancos_lotes_compi","esus_2023_compi.csv"),
 #             fileEncoding = "UTF-8")
 #  
 # beepr::beep(sound = 8)  
 #  rm(list = ls())
  
  
  ######################## 2023 Balcao ########################
  dados <- fread(file = file.path("balcao_banco","dados.csv"), encoding = "Latin-1") %>% 
  select("DT_SIN_PRI","ID_MN_RESI","CO_MUN_RES","NU_IDADE_N",
         "EVOLUCAO","CLASSI_FIN","DT_NOTIFIC") %>% 
  filter(year(DT_SIN_PRI)==2023)

dados$evolucao <- ifelse(test = dados$EVOLUCAO=="OBITO", yes = 1, no = 0)
dados$EVOLUCAO <- NULL

dd23 <-dados %>%
    mutate(faixa = cut(NU_IDADE_N, breaks = c(-Inf,4,9,14,19,29,39,49,59,69,79,Inf),
                          labels = c("0 a 4","5 a 9","10 a 14","15 a 19","20 a 29",
                                     "30 a 39","40 a 49","50 a 59","60 a 69",
                                     "70 a 79","80 ou mais"))) %>% 
       setNames(c("data","muni","code_mn","idade","classifin",
                  "dataNotificacao","evolucao","faixa"))


  

# Remover os NA em data
# dd23[is.na(data),"data"] <- dd23[is.na(data),"dataNotificacao"]     # Eliminando NA em data


# Agrupando os dados (casos e obitos)
dd23 <- dd23 %>% 
  group_by(data,code_mn,muni,faixa) %>% 
  summarise(casos = n(),
            obitos = sum(evolucao, na.rm = TRUE)) %>% 
  mutate(ano = year(data),
         mes = month(data),
         semana = week(data)) %>% 
  left_join(fread(here('dados_supl','pop.csv')), by = "code_mn") %>% 
  mutate(muni.x = NULL) %>% 
  rename(muni = muni.y)


write.csv2(x = dd23,
              file = file.path("bancos_finais_por_ano","dados2023.csv"),
              fileEncoding = "UTF-8")

setwd("~/Documentos")

write.csv2(x = dd23,
           file = file.path("relatorio","dados","dados2023.csv"),
           fileEncoding = "UTF-8")

  beepr::beep(sound = 8)
   rm(list = ls())
