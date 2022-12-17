doses.trabsaude <- tabyl(filter(ddvac, grupo_at == "TRABALHADOR DE SAUDE"), 
                         muni, dose, show_na = F)

tab <- cbind.data.frame(doses.trabsaude[1],pop.trabsaude,doses.trabsaude[,2:6]) # monta a tabela c/ pop
tab <- tab %>% rbind.data.frame(c("Total", as.numeric(colSums(tab[2:7])))) # adiciona totais
tab <- cbind.data.frame(tab[1], sapply(X = tab[,2:7], FUN = as.numeric)) # transforma numerico

cbind.data.frame(tab,
                 cob.d1=round(tab[3]/tab[2]*100,1),
                 cob.d2=round(tab[4]/tab[2]*100,1),
                 cob.ref1=round(tab[5]/tab[2]*100,1),
                 cob.ref2=round(tab[6]/tab[2]*100,1),
                 cob.ref3=round(tab[7]/tab[2]*100,1)) %>% 
  kable(col.names = c("Município", "Pop.trab.saude", "D1","D2","Ref1", 
                      "Ref2", "Ref3","D1%","D2%","Ref1%","Ref2%","Ref3%")) %>%                  
  kable_styling(full_width = F, 
                bootstrap_options = c("striped", "hover", 
                                      "condensed", "responsive"))

###################### USAR ESTE - FALTA RESOLVER O PROBLEMA DOS DADOS AUSENTES DE 2022

#Casos
dd <- fread(here('dados','todos_anos.csv'))
casos <- dd %>%
  group_by(data) %>%
  summarise(casos = sum(casos, na.rm=T)) %>% 
  mutate(data = as.Date(data),
         mmovel7 = round(rollmean(casos, k = 7, fill = NA, align = 'left'),0))

#Doses
doses.data <- tabyl(ddvac, data, dose, show_na = F) %>%
  mutate(data = as.Date(data)) %>% 
  setNames(c('data','d1','d2','ref1','ref2','ref3','ref4'))

#Juntando casos, doses e limpando NA
casos.doses <- left_join(x = casos, y = doses.data, by="data")
casos.doses[is.na(casos.doses)] <- 0

#Gerando arquivo para o gráfico
graf <- cbind(casos.doses,
              round(cumsum(x = casos.doses$d1)/766509*100, 1),
              round(cumsum(x = casos.doses$d2)/766509*100, 1),
              round(cumsum(x = casos.doses$ref1)/766509*100, 1),
              round(cumsum(x = casos.doses$ref2)/766509*100, 1),
              round(cumsum(x = casos.doses$ref3)/766509*100, 1),
              round(cumsum(x = casos.doses$ref4)/766509*100, 1)) %>%
  setNames(c("data","casos","mmovel","d1","d2","ref1","ref2","ref3","ref4",
             'cob.d1','cob.d2','cob.ref1','cob.ref2','cob.ref3','cob.ref4'))

#Gráfico
ggplot(graf[year(graf$data)!='2020',])+
  geom_col(aes(x = data, y = mmovel), alpha = .25, width = 1, fill='#00598C')+  
  geom_line(aes(x = data, y = cob.d1*20, col='Cob.d1'))+
  geom_line(aes(x = data, y = cob.d2*20, col='Cob.d2'))+
  geom_line(aes(x = data, y = cob.ref1*20, col='Cob.ref1'))+
  geom_line(aes(x = data, y = cob.ref2*20, col='Cob.ref2'))+
  geom_line(aes(x = data, y = cob.ref3*20, col='Cob.ref3'))+
  geom_line(aes(x = data, y = cob.ref4*20, col='Cob.ref4'))+
  scale_colour_manual(values = RColorBrewer::brewer.pal(n = 6, name = 'Paired'), 
                      labels = c("D1",'D2','Reforço 1', 'Reforço 2', 'Reforço 3', 'Reforço'))+
  # scale_fill_manual(breaks = c('Cobertura de D1','Cobertura de D2',"Dose adicional"))+
  scale_x_date(date_breaks = "2 month", date_labels = "%b/%y")+
  scale_y_continuous(sec.axis = sec_axis(~./20, name = "Cobertura (%)", breaks = seq(0,120,5)))+
  labs(x=NULL, y='Casos', colour=NULL, title = "Casos e coberturas vacinais por dia.")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5, face = "bold", colour = alpha('black', .7)),
        legend.position = 'top', axis.text.x = element_text(angle = 30),
        legend.key = element_rect(fill = 'transparent', 
                                  colour = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent', 
                                             colour = 'transparent'))

##################
ddvac[ddvac$data %in% as.Date("2021-01-01"):as.Date(Sys.Date()),] %>% 
  group_by(data, dose) %>% 
  summarise(num = n()) %>% 
  
  ggplot(aes(group=dose))+
  geom_col(aes(x = format(data, '%b/%y'), y = num, fill=dose), width = .95)+
  scale_x_discrete(limits = format(seq.Date(from = as.Date("2021-01-01"), 
                                            to = as.Date(Sys.Date()), by = "month"),format="%b/%y"))+
  scale_fill_manual(values = RColorBrewer::brewer.pal(n = 6, name = 'Paired'), 
                    labels = c("D1","D2","Ref1","Ref2","Ref3","Ref4"))+
  labs(x=NULL, y="Doses", col=NULL, fill=NULL)+
  theme_minimal()+
  theme(legend.position = 'top', 
        axis.text.x = element_text(angle = 45, hjust = 1))


c('jan/21','fev/21','mar/21','abr/21','mai/21',
  'jun/21','jul/21','ago/21','set/21','out/21',
  'nov/21','dez/21','jan/22','fev/22','mar/22',
  'abr/22', 'mai/22', 'jun/22', 'jul/22','ago/22',
  'set/22','out/22')

format(seq.Date(from = as.Date("2021-01-01"), 
         to = as.Date(Sys.Date()), by = "month"),format="%b/%y")


format(Sys.Date(), format="%b/%y")

