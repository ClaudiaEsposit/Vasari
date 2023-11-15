cat('\014')
rm(list=ls())
setwd("~/Vasari")
source("Info_providing.R")

#--------------------b)Agreement_summary---------------------------####
Agreement <- read_excel("~/Vasari/Data/Vasari_LDT.xlsx", sheet = "PDR", range = "A1:S109",col_names = TRUE)
Agreement <- Agreement %>%
  `colnames<-`(tolower(colnames(.)))%>%
  rename_all(~ gsub(" ", "", .))%>%
  mutate_all(~ gsub(" {2,}", " ", trimws(.x)))%>%
  mutate_all(tolower)%>%
  rename(id.bor=ndg)%>%
  mutate(importodovuto=as.numeric(importodovuto))
Agre <- Agreement %>%
  rename(
    date.agreement = datapdr,
    amount.agreement = importodovuto,
    date.start = datainiziopagamento,
    status = stato,
    cf.piva=codicefiscale)%>%
  select(-azione, -metodo) %>%
  distinct() %>%
  mutate(
    year.agreement = lubridate::year(date.agreement),
    gbv.agreement = "-",
    importosingolerate = as.numeric(importosingolerate),
    amount.agreement = as.numeric(amount.agreement)
  ) %>%
  group_by(id.bor,idccpdr) %>%distinct()%>%
  mutate(
    residual = amount.agreement - sum(importosingolerate),
    paid=sum(importosingolerate)
  ) %>%
  ungroup()%>%
  mutate(check_sum = residual + paid == amount.agreement)%>% #check if the sum has been made right
  select(-check_sum)%>%
  left_join(Entities, by = "cf.piva")

#left_join(Counterparties, by = "id.bor", relationship = "many-to-many")%>%
#select(-name,-role,-n.entities,-flag.imputed)%>%

  
Agre$id.entity <- paste0("a_", seq_len(nrow(Agre))) 

#check if date.cutoff=date.agreement, if not than we do not have gbv.agreement
is_present <- any(Agre$date.agreement == as.Date("2023-04-29")) #FALSE



