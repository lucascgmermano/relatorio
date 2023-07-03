source(file = 'compila_bancos_esus.R')
source(file = 'compila_bancos_sivep.R')
source(file = 'merge_final_por_ano.R')
source(file = 'Script_merge_vacivida_simples.R')
source(file = 'manipul_ddvac.R')



# Criar copia dos scripts de ajustes dos bancos ---------------------------

setwd("~/Documentos/ministerio/")

arquivos <- list("compila_bancos_esus.R", "compila_bancos_sivep.R",
                 "merge_final_por_ano.R","Script_merge_vacivida_simples.R",
                 "manipul_ddvac.R", "executa_atualizacao_bancos.R")

destino <- "../relatorio/ajustes_bancos/"

lapply(arquivos, function(arquivos){file.copy(from = arquivos,
                                             to = destino,
                                             overwrite = TRUE)})


beepr::beep(sound = 8)

