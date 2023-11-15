#---------------------------- a)Intro --------------------------------------####
cat('\014')
rm(list=ls())
setwd("~/Vasari")
source("Library.R")
source("Functions.R")
source("Vanilla.R")
source("Tables.R")
source("Info_providing.R")
#---------------------------- c)Tables_by_borrower ------------------------####
entities.loans <- Entities  %>% 
  left_join(link.counterparties.entities, by= 'id.entity',relationship = "many-to-many") %>% 
  left_join(Counterparties,by = 'id.counterparty') %>%
  filter(role=='borrower' ) %>%
  left_join(Loans_table,by = 'id.bor',relationship = "many-to-many")

ent_g <- entities.loans %>% 
  select(type.subject,id.bor,gbv.original,province,type,area) %>% arrange(desc(gbv.original))%>%
  distinct(id.bor,gbv.original,type.subject,.keep_all = TRUE)%>%
  group_by(id.bor) %>% 
  reframe(type.subject=type.subject)%>%distinct()
gar<-Guarantors%>%
  select(id.bor,id.gar)
solv<-Info_pf%>%
  select(id.bor,solvency.adj)%>%
  mutate(solvency.adj=case_when(
    solvency.adj=="Insolvent"~"Insolvent",
    TRUE ~ "Solvent"
  ))%>%
  rename(id.gar=id.bor)
ind_gar <- ent_g  %>% 
  left_join(gar, by= 'id.bor',relationship = "many-to-many")
idgar<-entities.loans%>%
  select(id.bor,type.subject)%>%
  rename(id.gar=id.bor,type.gaurantors=type.subject)
prova <- ind_gar  %>% 
  left_join(idgar, by= 'id.gar',relationship = "many-to-many")%>%
  left_join(solv, by= 'id.gar',relationship = "many-to-many")%>%
  distinct()

prova1 <- prova %>%
  filter(type.subject %in% c("individual", "corporate") & 
           type.gaurantors == "individual")
prova_ind<-prova1%>%filter(type.subject=="individual")
prova_corp<-prova1%>%filter(type.subject=="corporate")
solvent_count <- sum(prova_ind$solvency.adj == "Solvent", na.rm = TRUE)
insolvent_count <- sum(prova_ind$solvency.adj == "Insolvent", na.rm = TRUE)


prova2 <- prova %>%
  filter(type.subject %in% c("individual", "corporate") & 
           type.gaurantors == "corporate")
prova_ind2<-prova2%>%filter(type.subject=="individual")
prova_corp2<-prova2%>%filter(type.subject=="corporate")
solvent_count <- sum(prova_ind2$solvency.adj == "Solvent", na.rm = TRUE)
insolvent_count <- sum(prova_ind2$solvency.adj == "Insolvent", na.rm = TRUE)

updated_gaur <- data.frame(
  "Type of Borrower" = rep(c("Individual", "Individual", "Corporate", "Corporate"), each = 1),
  "Type of Gaurantor" = rep(c("Individual", "Corporate", "Individual", "Corporate"), each = 1),
  "Total" = c(49, 14, 23, 22),
  "N Solvent Guarantors" = c(16, "-", 5, "-"),
  "N Insolvent Guarantors" = c(27, "-", 7, "-")
)
colnames(updated_gaur) <- gsub(" ", ".", colnames(updated_gaur))
