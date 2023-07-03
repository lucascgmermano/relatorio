x <-  xtabs(casos ~ muni + faixa, 
            data = dd23) %>% 
  addmargins(margin = c(1,2))

row.names(x)[21] <- "Total"
colnames(x)[12] <- "Total"

x %>% kableExtra::kable() %>% 
  kable_styling(full_width = F,
                bootstrap_options = c("striped", "hover",
                                      "condensed", "responsive"))
