#------------------a)Intro-----------------------------------------####
# source("Info_providing.R")
Agreement <- read_excel("~/Vasari/Data/Vasari_LDT.xlsx", sheet = "PDR", range = "A1:S109",col_names = TRUE)
Agreement <- Agreement %>%
  `colnames<-`(tolower(colnames(.)))%>%
  rename_all(~ gsub(" ", "", .))%>%
  mutate_all(~ gsub(" {2,}", " ", trimws(.x)))%>%
  mutate_all(tolower)%>%
  rename(id.bor=ndg,status=stato)%>%
  mutate(importodovuto=as.numeric(importodovuto),
         pagamento=as.numeric(pagamento),
         importosaldo=as.numeric(importosaldo),
         sumattivo=as.numeric(sumattivo))
#--------------------b)Agreement_summary---------------------------####
date.cutoff<-"2023-04-29"
Agre <- Agreement %>%
  rename(
    date.agreement = datapdr,
    gbv.agreement = importodovuto,n.instalment=qtarate,
    id.agreement=idccpdr,date.last.payment=dtfatturazione,
    cf.piva=codicefiscale)%>%
  select(-azione, -metodo) %>%
  distinct() %>%
  mutate(
    year.agreement = lubridate::year(date.agreement),
    gbv.agreement = as.numeric(gbv.agreement),
    importosingolerate = as.numeric(importosingolerate)
  ) %>%
  group_by(id.bor,id.agreement) %>%distinct()%>%
  mutate(
    amount.agreement=sum(importosingolerate),
    paid=sum(pagamento),date.start = min(as.Date(dtpagamento)),
    residual = amount.agreement - paid,
    check=importosaldo-sumattivo,
    date.end = max(as.Date(dtpagamento)),
    length = ifelse(date.start == date.end, 1, round(as.numeric(difftime(date.end, date.start, units = "days")) / 30))) %>%
  ungroup()%>%
  mutate(check_amount = importosaldo == amount.agreement,
         check_paid=sumattivo==paid)%>%
  select(-check_amount,-check,-check_paid,-tiporata,-dtpagamento,-importosingolerate,
         -pagamento,-ultima_rata_pagamento,-importosaldo,-sumattivo,
         -qtapagamentipresenti,-datainiziopagamento)%>%
  distinct(id.bor,id.agreement,.keep_all = TRUE)

Agre <- Agre %>% mutate(status = case_when( 
  date.start > date.cutoff ~ 'Proposal',
  (paid == 0) &  as.numeric(difftime(date.cutoff, date.start, units = "days")) / 30.44 > 6 ~ 'Failed',
  (paid == 0) &  as.numeric(difftime(date.cutoff, date.start, units = "days")) / 30.44 < 6 ~ 'Active',
  (paid != amount.agreement) &  as.numeric(difftime(date.cutoff, date.last.payment, units = "days")) / 30.44 > 6 ~ 'Failed',
  (paid != amount.agreement) &  as.numeric(difftime(date.cutoff, date.last.payment, units = "days")) / 30.44 < 6 ~ 'Active',
  residual == 0 ~ 'Closed'
))
#---####
Agre<-Agre%>%
  left_join(entities%>% select(cf.piva,id.entity,name), by = "cf.piva")%>%
  left_join(link.counterparties.entities, by = "id.entity", relationship = "many-to-many")%>%
  left_join(Counterparties%>%select(id.bor,name,id.counterparty,id.group),by="name",relationship="many-to-many")%>%
  distinct()%>%
  mutate(id.counterparty = coalesce(id.counterparty.y, id.counterparty.x)) %>%
  select(-id.counterparty.x, -id.counterparty.y)%>%
  group_by(id.bor.x, id.entity) %>%
  arrange(id.bor.x, id.entity, desc(id.bor.y == id.bor.x)) %>%
  slice_head() %>%
  ungroup()%>%
  select(-name,-id.bor.y)%>%
  rename(id.bor=id.bor.x)%>%
  select(id.agreement,id.counterparty,id.bor,id.group,year.agreement,
         date.agreement,gbv.agreement,amount.agreement,date.start,date.end,
         n.instalment,length,status,paid,residual,date.last.payment)


#check if date.cutoff=date.agreement, if not than we do not have gbv.agreement
#is_present <- any(Agre$date.agreement == as.Date("2023-04-29")) #FALSE

Agre_proj<- Agreement%>%
  select(idccpdr,dtpagamento,importosingolerate,dtfatturazione,pagamento)%>%
  rename(id.agreement=idccpdr,date.due=dtpagamento,amount.due=importosingolerate,
         date.paid=dtfatturazione,amount.paid=pagamento)%>%
  mutate(date.due=as.Date(date.due),amount.due=as.numeric(amount.due),
         date.paid=as.Date(date.paid),amount.paid=as.numeric(amount.paid))


