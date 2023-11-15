cat('\014')
rm(list=ls())
setwd("~/Vasari")
source("Library.R")
source("Functions.R")
source("Vanilla.R")
source("Tables.R")
#--------------------------b)Infoproviding_pf-------------------------------####
Info_pf <- read_excel("~/Vasari/Data/DD_PDL CON RESIDENZA + IVA - 199 POS. 12072023.xlsx", sheet = "Foglio1", range = "A1:V200",col_names = TRUE)
Info_pf <- Info_pf %>%
  `colnames<-`(tolower(colnames(.)))%>%
  rename_all(~ gsub(" ", "", .))%>%
  mutate_all(~ gsub(" {2,}", " ", trimws(.x)))%>%
  mutate_all(tolower)%>%
  select(ndg,cf_piva,cognome,nome,statooccupazione,emolumentimensililordi,comune,provincia,note...17)%>%
  mutate(date.infoprov="2023-07-12",solvency.adj=NA,income.net=NA,region=NA,
         id.bor=ndg,cf.piva=cf_piva,name=paste(trimws(nome), trimws(cognome), sep=" "),
         solvency.base=statooccupazione,income.gross=emolumentimensililordi,
         city=comune,province=provincia,note=note...17)%>%
  select(id.bor,cf.piva,date.infoprov,name,solvency.base,solvency.adj,income.gross,income.net,
         city,province,region,note)%>%
  mutate(id.bor=as.character(id.bor),cf.piva=as.character(cf.piva),
         date.infoprov=as.Date(date.infoprov),name=as.character(name),
         solvency.base=as.factor(solvency.base),solvency.adj=as.factor(solvency.adj),
         income.gross=as.numeric(income.gross),income.net=as.numeric(income.net),
         city=as.character(city),province=as.factor(province),region=as.factor(region))

Info_pf <- Info_pf %>%
  mutate(solvency.base = case_when(
    str_detect(note, 'indeterminato') ~ 'employee-permanent',
    str_detect(note, 'determinato') ~ 'employee-temporary',
    str_detect(solvency.base,'pensionato') ~ 'pensioner',
    str_detect(solvency.base,'deceduto') ~ 'deceased',
    str_detect(solvency.base,'dipendente') ~ 'employee-N/A',
    str_detect(solvency.base,'disoccupato') ~ 'insolvent'
  ))


Info_pf <- apply_fct_emp_pens(Info_pf)

Info_pf <- Info_pf %>%
  mutate(
    solvency.adj = case_when(
      str_detect(solvency.base,'pensioner') & income.net > 1200 ~ "pensioner",
      str_detect(solvency.base,'employee-permanent') & income.net > 500 ~ "employee-permanent",
      str_detect(solvency.base,'employee-temporary') & income.net > 500 ~ "employee-temporary",
      str_detect(solvency.base,'employee-N/A') & income.net > 500 ~ "employee-N/A",
      TRUE ~ "Insolvent"
    )
  )%>%
  select(-note)

#--------------------------c)Infoproviding_pg-------------------------------####
Info_pg <- read_excel("~/Vasari/Data/DD_EARLY REPORT 62 POS. 13072023.xlsx", sheet = "Worksheet", range = "A1:AW63",col_names = TRUE)
Info_pg <- Info_pg %>%
  `colnames<-`(tolower(colnames(.)))%>%
  rename_all(~ gsub(" ", "", .))%>%
  mutate_all(~ gsub(" {2,}", " ", trimws(.x)))%>%
  mutate_all(tolower)%>%
  mutate(cf.piva=cf_piva,date.infoprov="2023-07-12",name= out_dati_anagrafici_denominazione,
         type=out_dati_attivita_natura_giuridica, status=out_dati_attivita_stato_attivita,
         date.cessation=out_dati_attivita_data_cessazione_attivita,city=out_dati_anagrafici_indirizzo_comune,
         province=out_dati_anagrafici_indirizzo_provincia, region=out_dati_anagrafici_indirizzo_regione)%>%
  mutate(cf.piva=as.character(cf.piva),date.infoprov=as.Date(date.infoprov),
         name=as.character(name),type=as.factor(type),status=as.factor(status),
         date.cessation=as.Date(date.cessation,"%d/%m/%Y"),city=as.character(city),
         province=as.factor(province),region=as.factor(region))%>%
  select(cf.piva,date.infoprov,name,type,status,
         date.cessation,city,province,region)
Info_pg <- Info_pg %>%
  mutate(type = case_when(
    str_detect(type, 'associazione') ~ 'other',
    str_detect(type, "societa' a responsabilita' limitata") ~ 'srl',
    str_detect(type,"societa' cooperativa") ~ 'sc',
    str_detect(type,"societa' in accomandita semplice") ~ 'sas',
    str_detect(type,"societa' in nome collettivo") ~ 'snc',
    str_detect(type,"societa' per azioni") ~ 'spa',
    str_detect(type,"consortile") ~ 'sc',
    TRUE ~ NA
  ))
Info_pg <- Info_pg %>%
  mutate(status = case_when(
    str_detect(status, "attiva") ~ 'active',
    str_detect(status, "cancellata") ~ 'canceled',
    str_detect(status,"fallita") ~ 'bankruptcy',
    str_detect(status,"in procedura concorsuale") ~ 'insolvency',
    str_detect(status,"in scioglimento / liquidazione") ~ 'liquidation',
    str_detect(status,"inattiva") ~ 'inactive',
    TRUE ~ NA
  ))
#---------------------------- b)Infoproviding_check ------------------------####
info_pf<-Info_pf%>%
  rename(solvency.pf=solvency.adj,income.pf=income.net)
info_pf$province <- as.character(info_pf$province)
Entities<-left_join(Entities,info_pf,by="cf.piva")
Entities<-Entities%>%
  mutate(
    city= ifelse(!is.na(city.y), city.y, city.x),
    province= ifelse(!is.na(province.y), province.y, province.x),
    region= ifelse(!is.na(region.y), region.y, region.x),
    solvency.pf= ifelse(!is.na(solvency.pf.y), solvency.pf.y, solvency.pf.x),
    income.pf= ifelse(!is.na(income.pf.y), income.pf.y, income.pf.x),
    dummy.info = ifelse(cf.piva %in% info_pf$cf.piva, 1, 0)
  )%>%
  rename(name=name.x)%>%
  select(id.entity,name,cf.piva,type.subject,dummy.info,sex,range.age,age,solvency.pf,income.pf,type.pg,status.pg,date.cessation,city,
         province,region,area,flag.imputed)

info_pg<-Info_pg%>%
  rename(type.pg=type,status.pg=status)
info_pg$province <- as.character(info_pg$province)
info_pg$region <- as.character(info_pg$region)
info_pg$date.cessation <- as.character(info_pg$date.cessation)
info_pg$type.pg <- as.character(info_pg$type.pg)
Entities<-left_join(Entities,info_pg,by="cf.piva")
Entities$type.pg.x <- as.character(Entities$type.pg.x)


Entities<-Entities%>%
  mutate(
    city= ifelse(!is.na(city.y), city.y, city.x),
    province= ifelse(!is.na(province.y), province.y, province.x),
    region= ifelse(!is.na(region.y), region.y, region.x),
    status.pg= ifelse(!is.na(status.pg.y), status.pg.y, status.pg.x),
    type.pg= ifelse(!is.na(type.pg.y), type.pg.y, type.pg.x),
    date.cessation= ifelse(!is.na(date.cessation.y), date.cessation.y, date.cessation.x),
    dummy.info = ifelse(cf.piva %in% info_pf$cf.piva, 1, 0)
  )%>%
  rename(name=name.x)%>%
  select(id.entity,name,cf.piva,type.subject,dummy.info,sex,range.age,age,solvency.pf,income.pf,type.pg,status.pg,date.cessation,city,
         province,region,area,flag.imputed)

Entities$province <- ifelse(nchar(Entities$province) == 2,
                            Geo$province[match(Entities$province, Geo$descr.prov)],
                            Entities$province)