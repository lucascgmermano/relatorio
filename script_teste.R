left_join(tabyl(subset(ddvac, muni=="Caconde"), cat.pop.etaria, dose),
          subset(pop.et, muni=="Caconde"), by = 'cat.pop.etaria') %>% 
  mutate(cob.d1=round(d1/pop.etaria*100, 1),
         cob.d2=round(d2/pop.etaria*100, 1),
         cob.dadic1=round(dadic1/pop.etaria*100, 1),
         cob.dadic2=round(dadic2/pop.etaria*100, 1),
         cob.dadic3=round(dadic3/pop.etaria*100, 1),
         muni = NULL) %>%
  select(cat.pop.etaria, pop.etaria, 
         d1, d2, dadic1, dadic2, dadic3,
         cob.d1, cob.d2, cob.dadic1, cob.dadic2, cob.dadic3) %>%
  kable(col.names = c("Faixa etária", "População", 
                      "D1", "D2", "Da1","Da2","Da3",
                      "Cob.D1", "Cob.D2", "Cob.Da1","Cob.Da2","Cob.Da3"),
        caption = i) %>%
  kable_styling(full_width = F,
                bootstrap_options = c("striped", "hover",
                                      "condensed", "responsive")) 

table(ddvac[ddvac$muni=="Caconde",]$dose)
unique(ddvac$d)




doses.geralidade <- tabyl(ddvac, cat.pop.etaria, dose, show_na = T)

cbind.data.frame(doses.geralidade[,1], 
                 pop.geraletaria[1:10,2],
                 doses.geralidade[2:4],
                 cobd1=round(doses.geralidade[1:8,2]/pop.geraletaria[1:10,2]*100,1),
                 cobd2=round(doses.geralidade[3]/pop.geraletaria[1:10,2]*100,1),
                 cobad=round(doses.geralidade[4]/pop.geraletaria[1:10,2]*100,1)) %>% 
  
  kable(col.names = c("Faixa etária", "População", "D1","D2","D.Adic.", 
                      "Cob.D1", "Cob.D2", "Cob.D.Adic.")) %>%                  
  kable_styling(full_width = F, 
                bootstrap_options = c("striped", "hover", 
                                      "condensed", "responsive"))

as.character(format(seq(from = as.Date("2021-01-01"), 
           to = Sys.Date(), 
           by = "month"), "%b-%Y"))

