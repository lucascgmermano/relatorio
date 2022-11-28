################################## Dia #########################################
#Casos
casos <- dd22 %>%
  group_by(data) %>%
  summarise(casos = sum(casos, na.rm=T)) %>% 
  mutate(data = as.Date(data),
         mmovel7 = round(rollmean(casos, k = 7, fill = NA, align = 'left'),0))

#Doses
doses.data <- ddvac[,c("data","dose")] %>% 
  pivot_wider(names_from = dose, values_from = dose, values_fn = length)
doses.data <- doses.data[,1:4]
doses.data <- doses.data %>% 
  setNames(c('data','d1','d2','dadic'))

#Juntando casos, doses e limpando NA
casos.doses <- left_join(x = casos, y = doses.data, by="data")
casos.doses[is.na(casos.doses)] <- 0

#Gerando arquivo para o gráfico
graf <- cbind(casos.doses,
              round(cumsum(x = casos.doses$d1)/718970*100, 1),
              round(cumsum(x = casos.doses$d2)/718970*100, 1),
              round(cumsum(x = casos.doses$dadic)/718970*100, 1)) %>%
  setNames(c('data', 'casos', 'mmovel', 'd1', 'd2',
             'ref', 'cob.d1', 'cob.d2','cob.d.adic'))

#Gráfico
filter(graf, lubridate::year(data)==2022) %>% 
  ggplot()+
  geom_col(aes(x = data, y = casos), width = .9, fill='#00598C')+  
  geom_text(hjust=-.5, aes(x = data, y = casos, label = casos, angle=90), size=3, check_overlap = TRUE)+
  scale_x_date(date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(n.breaks = 10, expand = expansion(mult = c(0,.25)))+
  labs(x=NULL, y='Casos', colour=NULL)+
  ggforce::facet_zoom(xy =  data >= as.Date(Sys.Date()-20) &
                        data <= as.Date(Sys.Date()), horizontal = F, shrink = F)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = .5, face = "bold", colour = alpha('black', .7)),
        legend.position = 'top', axis.text.x = element_text(angle = 30),
        legend.key = element_rect(fill = 'transparent', 
                                  colour = 'transparent'),
        legend.box.background = element_rect(fill = 'transparent', 
                                             colour = 'transparent'),
        strip.background = element_rect(fill= '#deeaee',
                                        color='tomato',
                                        linetype = 'dotted'))


### Casos e coberturas
```{r efeito vacina, results='asis', out.width= "100%"}
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
  setNames(c('data','d1','d2','dadic'))

#Juntando casos, doses e limpando NA
casos.doses <- left_join(x = casos, y = doses.data, by="data")
casos.doses[is.na(casos.doses)] <- 0

#Gerando arquivo para o gráfico
graf <- cbind(casos.doses,
              round(cumsum(x = casos.doses$d1)/766509*100, 1),
              round(cumsum(x = casos.doses$d2)/766509*100, 1),
              round(cumsum(x = casos.doses$dadic)/766509*100, 1)) %>%
  setNames(c('data', 'casos', 'mmovel', 'd1', 'd2',
             'ref', 'cob.d1', 'cob.d2','cob.d.adic'))

#Gráfico
ggplot(graf[year(graf$data)!='2020',])+
  geom_col(aes(x = data, y = mmovel), alpha = .25, width = 1, fill='#00598C')+  
  geom_line(aes(x = data, y = cob.d1*20, col='Cob.d1'))+
  geom_line(aes(x = data, y = cob.d2*20, col='Cob.d2'))+
  geom_line(aes(x = data, y = cob.d.adic*20, col='Cob.D.Adic'))+
  scale_colour_manual(values = c('tomato','#90C134','orange'), 
                      labels = c("Dose adicional",'Cobertura de D1','Cobertura de D2'))+
  scale_fill_manual(breaks = c('Cobertura de D1','Cobertura de D2',"Dose adicional"))+
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
cat("<Br><Br><Br><Br><Br><Br>")
