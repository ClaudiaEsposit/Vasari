# #---------------------------- a)Intro --------------------------------------####
# cat('\014')
# rm(list=ls())
# source("Library.R")
# source("Functions.R")
# source("Vanilla.R")
#----------------------------- b)Check -------------------------------------####
Loans_check<- Loans%>%
  select(id.loan,id.bor,gbv.original,principal,interest,expenses,date.status)%>%
  mutate(GBV_Check=expenses+principal+interest,
         check_gbv = ifelse(abs(GBV_Check - gbv.original) <= 0.01, "YES", "NO"))
#It gives true if the number of yes is equal to the number of valid rows, so it is always confirmed
Loans_check %>%
  summarise(
    count_yes_check_gbv = sum(!is.na(check_gbv) & check_gbv == "YES"),
    non_na_rows_check_gbv = sum(!is.na(check_gbv))
  ) %>%
  summarise(
    check_gbv_match = count_yes_check_gbv == non_na_rows_check_gbv
  )
#-------------------------- c)Normalization -------------------------------#####
prim_ndg<-primary_key(NDG)
prim_loans<-primary_key(Loans)
# Functional dependence
#dependence_function(NDG, "NDG")
#dependence_function(Loans, "Loans")

#-------------------------- d)Loans_Table ---------------------------------#####
Loans_table <- Loans%>%
  mutate(originator=NA,ptf=NA,cluster.ptf=NA,status=NA,penalties=NA,date.last.act=NA,
         flag.imputed=NA,id.group=id.bor,gbv.residual=NA,date.origination=NA)%>%
  select(id.loan,id.bor,id.group,originator,ptf,cluster.ptf,type,status,
         gbv.original,gbv.residual,principal,interest,penalties,expenses,
         date.origination,date.status,date.last.act,flag.imputed)%>%
  mutate(id.loan=as.character(id.loan),id.bor=as.character(id.bor),id.group=as.character(id.group),
         originator=as.character(originator),ptf=as.character(ptf),cluster.ptf=as.character(cluster.ptf),
         type=as.factor(type),status=as.factor(status),gbv.original=as.numeric(gbv.original),
         gbv.residual=as.numeric(gbv.residual),principal=as.numeric(principal),interest=as.numeric(interest),
         penalties=as.numeric(penalties),expenses=as.numeric(expenses),
         date.status=as.Date(date.status),date.last.act=as.Date(date.last.act),flag.imputed = as.integer(flag.imputed))%>%
  distinct()
#------------------------ e)Counterparties_Table ---------------------------####
NDG <- NDG %>%
  mutate(across(c(starts_with("nome_"), "name"), ~ gsub("[,.*]+$", "", .))) #Deletes the *.* from the names
NDG <- NDG %>% mutate_all(str_trim)
NDG$nome_1 <- gsub("\\s+", " ", NDG$nome_1)
NDG <- NDG %>%
  mutate(
    name = ifelse(
      is.na(nome_1),
      name,
      paste0(
        ifelse(!is.na(nome_1), nome_1, ""),
        ifelse(!is.na(nome_2), paste0(", ", nome_2), ""),
        ifelse(!is.na(nome_3), paste0(", ", nome_3), ""),
        ifelse(!is.na(nome_4), paste0(", ", nome_4), ""),
        ifelse(!is.na(nome_5), paste0(", ", nome_5), "")
      )
    )
  )
NDG <- NDG %>%
  mutate(
    cf.piva = ifelse(
      !is.na(pf) & is.na(cf), pf,
      ifelse(!is.na(cf) & is.na(pf), cf,
             ifelse(is.na(cf) & is.na(pf),
                    apply(select(., starts_with("codice_fiscale")), 1, function(row) {
                      non_na_values <- row[!is.na(row)]
                      if (length(non_na_values) > 0) {
                        return(paste(non_na_values, collapse = ","))
                      } else {
                        return(NA)
                      }
                    }),
                    ifelse(cf != pf, paste0(cf, ",", pf), pf)
             )
      )
    )
  )

NDG_count <- NDG %>%
  select(id.bor,name,cf.piva,role)%>%
  mutate (id.group = NA,role= case_when(role == "d" ~ "borrower",
                                        role == "g" ~  "guarantor",
                                        role == "dg" ~  "both"),
          flag.imputed=NA)

both <- NDG_count %>% filter(role == "both") %>% select(id.bor) 
g <- Guarantors %>% filter(id.gar %in% both$id.bor & id.gar != id.bor) %>% #This filters the rows of Guarantors where the "id.gar" is in the list of "id.bor" from the previous step (both$id.bor) and where "id.gar" is not equal to "id.bor".
  select(id.bor, name,id.gar) %>% distinct() %>% 
  mutate(role = 'guarantor')  

counterparties <- NDG_count %>% mutate(role= case_when(role == "both"  ~ "borrower",
                                                       role == "borrower"  ~ "borrower",
                                                       role == "guarantor" ~  "guarantor"))
count_link<- counterparties

counterparties <- counterparties %>% bind_rows(g)
counterparties$id.counterparty <- paste0("c_", seq_len(nrow(counterparties)))
Counterparties <- counterparties%>%
  mutate(n.entities=NA)%>%
  select(id.counterparty,id.bor,id.group,role,name,n.entities,flag.imputed)%>%
  mutate(role = as.factor(role),n.entities = as.integer(n.entities),
         flag.imputed = as.integer(flag.imputed))

Counterparties$id.group <- ifelse(is.na(Counterparties$id.group),"-", Counterparties$id.group)
#-------------------------- f)Entities_Table -------------------------------####

entities <- NDG %>% select(id.bor, name, cf.piva,type.subject=type.pg, city, province)
entities <- divide_column_by_character_piva(entities, c("name", "cf.piva"), ",")
entities <- entities %>% distinct()

entities <- entities %>%
  group_by(name, cf.piva) %>%
  summarise(
    id.bor = paste(id.bor, collapse = ","),
    type.subject = first(type.subject),
    city = first(city),
    province = first(province)
  )%>%
  ungroup()
#id
entities$id.entity <- paste0("e_", seq_len(nrow(entities))) 

entities <- entities %>% mutate(dummy.info=NA,
                                solvency.pf=NA,
                                income.pf=NA,
                                date.cessation=NA,
                                status.pg=NA,
                                flag.imputed=NA)
entities<-entities%>%
  add_age_column() %>%
  add_age_range_column() %>%
  add_type_column() %>%
  add_type_subject_column()%>%
  add_sex_column()


entities$cf.piva <- clean_cf.piva(entities$cf.piva)
entities$name <- gsub(' time house','',entities$name)

#--------------------------- g)Link_c_e_Table ------------------------------####

row_to_split <- 245

count <- Counterparties %>% slice(1:(row_to_split - 1))
gar <- Counterparties %>% slice(row_to_split:nrow(Counterparties))%>%
  left_join(g,by="name",relationship="many-to-many")%>%
  select(id.counterparty,id.bor=id.gar)%>%distinct()%>%
  left_join(entities%>%select(id.bor,id.entity)%>% divide_column_by_character(., id.bor, ","),
            by= "id.bor", relationship = "many-to-many")%>% select(-id.bor)%>%distinct()
cntprova <- count %>% select (id.counterparty, id.bor)
cntprova <- cntprova %>%
  left_join(entities %>% select(id.bor, id.entity) %>% divide_column_by_character(., id.bor, ","),
            by= "id.bor", relationship = "many-to-many")  %>% select(-id.bor)%>%distinct()
link.counterparties.entities<-cntprova%>%bind_rows(gar)


# count_link<-count_link%>%
#   select(id.bor)%>%
# g_link<-g%>%
#   rename(id.bor=id.gar,id.borbor=id.bor)%>%
#   select(id.bor,id.borbor)%>%
# prova<-count_link%>%bind_rows(g_link)%>%
#   mutate(id.borbor = coalesce(id.borbor, id.bor))%>%
#   rename(id.gar=id.bor,id.bor=id.borbor)%>%
#   left_join(Counterparties,by="id.bor",relationship="many-to-many")%>%
#   select(id.gar,id.counterparty)%>%distinct()
#   rename(id.bor=id.gar)%>%
#   left_join(entities %>% select(id.bor, id.entity) %>% divide_column_by_character(., id.bor, ","),
#             by= "id.bor", relationship = "many-to-many")  %>% select(-id.bor)
# 
# count_prova<-Counterparties

# link.counterparties.entities <- Counterparties %>% select (id.counterparty, id.bor)
# link.counterparties.entities <- link.counterparties.entities %>%
#   left_join(entities %>% select(id.bor, id.entity) %>% divide_column_by_character(., id.bor, ","),
#             by= "id.bor", relationship = "many-to-many")  %>% select(-id.bor)

entities <- entities %>% select(-id.bor)

Counterparties <- Counterparties %>%  left_join(    link.counterparties.entities %>%      
                                                      group_by(id.counterparty) %>%      
                                                      summarise(n.entities = n()),    by = "id.counterparty"  )%>%
  mutate(n.entities=n.entities.y)%>%
  select(-n.entities.x,-n.entities.y)

Entities<-entities%>%
  mutate(area=NA,region=NA)%>%
  select(id.entity,name,cf.piva,type.subject,dummy.info,sex,range.age,age,solvency.pf,income.pf,type.pg,status.pg,date.cessation,city,
         province,region,area,flag.imputed)%>%
  mutate(type.subject=as.factor(type.subject),dummy.info = as.integer(dummy.info),
         sex = as.factor(sex),age = as.integer(age),solvency.pf = as.factor(solvency.pf),
         income.pf = as.numeric(income.pf),type.pg = as.factor(type.pg),status.pg = as.factor(status.pg),
         province = as.factor(province),region = as.factor(region),area = as.factor(area),flag.imputed = as.integer(flag.imputed))
Entities$province <- Geo$province[match(Entities$province, Geo$descr.prov)]
Entities$region <- Geo$region[match(Entities$province, Geo$province)]
Entities$area <- Geo$area[match(Entities$region, Geo$region)]


