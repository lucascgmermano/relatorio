library(readxl)
library(data.table)
library(here)
library(openxlsx)

setwd("~/Documentos/ministerio/arquivos_vacivida")

lista <- list.files(path = here('arquivos_vacivida'),  # Caminho 
                    pattern = '.xlsx',                 # Padrão
                    recursive = TRUE)                  # Na pasta atual e subpastas

arquivos <- sapply(X = lista, FUN = openxlsx::read.xlsx)

ddvac_comp <- rbindlist(l = arquivos, fill = T)

data.table::fwrite(x = ddvac_comp[,c("DATA_NASCIMENTO_PACIENTE","IDADE","SEXO_PACIENTE",
                    "PACIENTE_GESTANTE","PACIENTE_PUERPERE","GRUPO_ATENDIMENTO",
                    "MUNICIPIO_RESIDENCIA_PACIENTE","GVE","IMUNOBIOLOGICO",
                    "DOSE","DATA_APLICACAO_VACINA","DOSE_ADICIONAL")], sep = ";",
           file = here("vacivida","vacivida.csv"))

# file.copy(from = here('vacivida','vacivida.csv'), 
#           to = '/home/costela/Documentos/relatorio/dados/vacivida.csv', 
#           overwrite = TRUE)

rm(lista, arquivos, ddvac_comp)

