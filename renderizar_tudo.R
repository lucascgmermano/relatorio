# Definindo o diretório que contém os arquivos .rmd
diretorio <- here::here()



# Listando todos os arquivos .rmd no diretório
arquivos_rmd <- list.files(here::here(), pattern = "\\.Rmd$", full.names = TRUE)

# Loop para renderizar cada arquivo
for(arquivo in arquivos_rmd) {
  rmarkdown::render(arquivo)
}


todosAnos <- read.csv2(file = "dados/todos_anos.csv")

quadro <- 
  todosAnos %>% 
  group_by(ano, mesL) %>% 
  summarise(casos = sum(casos)) %>% 
  mutate(mesL = factor(mesL, levels = c("jan","fev","mar",
                                         "abr","mai","jun",
                                         "jul","ago","set",
                                         "out","nov","dez")))
quadro[quadro$mesL %in% c("jan","fev") & quadro$ano == 2020,'casos'] <- 0

# Criando o heatmap
ggplot(quadro, aes(x = mesL, y = ano, fill = casos)) +
  geom_tile() + # Usar geom_tile para criar o heatmap
  geom_text(aes(label= casos), color="red3", check_overlap = TRUE) +
  scale_fill_gradient(low = "#D1E5F0", high = "blue2") + # Definir cores do gradiente
  theme_minimal() + # Tema minimalista
  labs(fill = "Número de Casos", x = NULL, y = NULL, title = "Heatmap de Casos de COVID-19") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Ajustar texto do eixo x



resumoCasos <- reshape2::dcast(data = todosAnos,
                formula = "n" ~ ano,
                value.var = "casos",
                fun.aggregate = sum,
                fill = 0) 

resumoObitos <- reshape2::dcast(data = todosAnos,
                               formula = "n" ~ ano,
                               value.var = "obitos",
                               fun.aggregate = sum,
                               fill = 0) 

acumulado <- rbind(resumoCasos,resumoObitos)

acumulado[,1] <- c("Casos", "Óbitos")
Total <- rowSums(acumulado[,2:6])

cbind(acumulado, total)

heat.colors(5)
barplot(5:1, col=heat.colors(5))

