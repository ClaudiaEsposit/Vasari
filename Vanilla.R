#---------------------------- a)Intro --------------------------------------####
cat('\014')
rm(list=ls())
source("Library.R")
source("Functions.R")

#---------------------------- b)NDG -------------------------------------####
NDG <- read_excel("~/Vasari/Data/Vasari_LDT.xlsx", sheet = "Anagrafiche", range = "A1:AD245",col_names = TRUE)
#Profiling NDG
Profile_ndg <- ExpData(data=NDG,type=2) %>% as.data.frame()
Profile_ndg <- Profile_ndg %>%
  filter(!grepl("numeric", Variable_Type, ignore.case = TRUE))
Profile_ndg <- Profile_ndg %>% rename("Variable" = "Variable_Name", "Type" = "Variable_Type",
                                      "#_Entries" = "Sample_n", "NAs" = "Missing_Count", 
                                      "%_NAs" ="Per_of_Missing", "#_distinct_values" ="No_of_distinct_values")
Profile_ndg$`%_NAs` <- paste0(Profile_ndg$`%_NAs` *100, "%")
Profile_ndg <- Profile_ndg %>% select(-NAs)
NDG <- NDG %>%
  `colnames<-`(tolower(colnames(.)))%>%
  rename_all(~ gsub(" ", "", .))%>%
  rename(id.bor=ndg,name=`intestaz.unica/rag.soc.`,type.pg=formagiuridica,pf=`partitaiva(aziende)`,
         cf=`codicefiscale(personafisica)`,province=provincia,city=comune,role=`tipoanagrafica(d=debitore,g=garante,dg=debitoreegarante)`)%>%
  mutate_all(~ gsub(" {2,}", " ", trimws(.x)))%>%
  mutate_all(tolower)%>%
  mutate(id.bor=as.character(id.bor,label=TRUE),cf=as.character(cf,label=TRUE),pf=as.character(pf,label=TRUE),
         city=as.character(city,label=TRUE),province=as.factor(province))


#---------------------------- c)Loans -------------------------------------####
Loans <- read_excel("~/Vasari/Data/Vasari_LDT.xlsx", sheet = "Posizioni", range = "C1:L204",col_names = TRUE)
Loans<- Loans%>%
  rename(id.loan=`Numero rapporto`,id.bor=NDG,name=Intestazione,type=`Forma tecnica rapporto`,
         gbv.original=`Gross book value Totale`,principal=`Gross book value Capitale`,
         interest=`Gross book value Interessi`,expenses=`Gross book value Spese`,
         date.status=`Data di passaggio a sofferenza`)%>%
  mutate_all(~ gsub(" {2,}", " ", trimws(.x)))%>%
  mutate_all(tolower)%>%
  `colnames<-`(tolower(colnames(.)))%>%
  rename_all(~ gsub(" ", "", .))%>%
  mutate(id.bor=as.character(id.bor,label=TRUE),
         type=as.factor(type),id.loan=as.character(id.loan,label=TRUE),
         gbv.original=as.numeric(gbv.original,label=TRUE),
         interest=as.numeric(interest,label=TRUE),
         expenses=as.numeric(expenses,label=TRUE),
         principal=as.numeric(principal,label=TRUE),
         date.status=as.Date(date.status,label=TRUE),
  )
#Profiling Loans
Profile_loans <- ExpData(data=Loans,type=2) %>% as.data.frame()
Profile_loans <- Profile_loans %>%
  filter(!grepl("numeric", Variable_Type, ignore.case = TRUE))
Profile_loans <- Profile_loans %>% rename("Variable" = "Variable_Name", "Type" = "Variable_Type",
                                          "#_Entries" = "Sample_n", "NAs" = "Missing_Count", 
                                          "%_NAs" ="Per_of_Missing", "#_distinct_values" ="No_of_distinct_values")
Profile_loans$`%_NAs` <- paste0(Profile_loans$`%_NAs` *100, "%")
Profile_loans <- Profile_loans %>% select(-NAs)
type_distinct<-Loans%>%
  distinct(type,)
Loans<-change_type_credit(Loans)
Loans <- Loans %>%
  mutate(id.bor = str_trim(id.bor)) %>%
  mutate(id.loan = str_trim(id.loan))
#--------------------------------d)Guarantors ------------------------------####
Guarantors <- read_excel("~/Vasari/Data/Vasari_LDT.xlsx", sheet = "Garanzie", range = "A1:J349",col_names = TRUE)
Guarantors <- Guarantors %>%
  rename(id.bor=NDG,id.gar=`NDG garante`,name=`Intestazione garante`,
         id.loan=`Numero rapporto garantito`)%>%
  `colnames<-`(tolower(colnames(.)))%>%
  rename_all(~ gsub(" ", "", .))%>%
  mutate_all(~ gsub(" {2,}", " ", trimws(.x)))%>%
  mutate_all(tolower)
Guarantors<-Guarantors%>% mutate(name = ifelse(name == 'san piero sas di gaini marta e c', name, gsub(' e ',', ',name)))

#---------------------------- e)Geo --------------------------------------####
Geo <- read_excel("~/Vasari/Data/Geo.xlsx",col_names = TRUE)
Geo <- Geo %>%
  mutate_all(~ gsub(" {2,}", " ", trimws(.x)))%>%
  mutate_all(tolower)%>%
  rename_all(~ gsub(" ", "", .))