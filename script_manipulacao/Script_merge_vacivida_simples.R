library(openxlsx)
library(readxl)
library(data.table)
library(here)



lista <- list.files(path = here('arquivos_vacivida'),
                    pattern = '.xlsx',
                    recursive = TRUE)


setwd('arquivos_vacivida/')     # Precisa estar no mesmo setwd dos arquivos que vão ser lidos

arquivos <- sapply(lista, readxl::read_xlsx, simplify = F)
ddvac_comp <- rbindlist(arquivos)

write.csv2(ddvac_comp[,c("DATA_NASCIMENTO_PACIENTE","IDADE","SEXO_PACIENTE",
                    "PACIENTE_GESTANTE","PACIENTE_PUERPERE","GRUPO_ATENDIMENTO",
                    "MUNICIPIO_RESIDENCIA_PACIENTE","GVE","IMUNOBIOLOGICO",
                    "DOSE","DATA_APLICACAO_VACINA","DOSE_ADICIONAL")], 
           file = here("dados","vacivida.csv"))

rm(lista, arquivos)


