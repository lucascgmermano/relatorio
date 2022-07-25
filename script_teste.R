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
