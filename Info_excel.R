
source("Gaur_table.R")

#---------------a)Info_pf-------------####
Insolvent_table<-Entities%>%
  filter(type.subject=="individual")%>%
  filter(solvency.pf=="Insolvent")%>%
  group_by(province)%>%
  summarise(num=n())%>%
  top_n(3, num) %>%
  arrange(desc(num))%>%
  ungroup()%>%
  rename(Province=province)
total_ins <- data.frame(
  Province = "Total",num = sum(Insolvent_table$num))
updated_ins <- bind_rows(Insolvent_table, total_ins)
updated_ins<-updated_ins%>%
  arrange(desc(num))%>%
  rename("N Individual"=num)
updated_ins$Province <- str_to_title(updated_ins$Province)


na_individual<-Entities%>%
  filter(type.subject=="individual")%>%
  filter(is.na(solvency.pf))%>%
  group_by(province)%>%
  summarise(num=n())%>%
  top_n(3, num) %>%
  arrange(desc(num))%>%
  ungroup()%>%
  rename(Province=province)
total_na <- data.frame(
  Province = "Total",num = sum(na_individual$num))
updated_na <- bind_rows(na_individual, total_na)
updated_na<-updated_na%>%
  arrange(desc(num))%>%
  rename("N Individual"=num)
updated_na$Province <- str_to_title(updated_na$Province)
  

Solv_prov <- Entities %>%
  filter(type.subject == "individual" & solvency.pf %in% c("pensioner", "employee-temporary", "employee-permanent")) %>%
  group_by(solvency.pf, province) %>%
  summarise(num = n()) %>%
  arrange(solvency.pf, desc(num)) %>%
  group_by(solvency.pf) %>%
  top_n(3, wt = num) %>%
  ungroup() %>%
  arrange(solvency.pf, desc(num))

total_prov <- data.frame(
  solvency.pf = "Total",province = "",num=sum(Solv_prov$num))

total_owners <- Solv_prov %>%
  group_by(solvency.pf) %>%
  summarize(solvency.pf = if ("employee-permanent" %in% solvency.pf) "Employee-Permanent Tot."
            else if ("employee-temporary" %in% solvency.pf) "Employee-Temporary Tot."
            else if ("pensioner" %in% solvency.pf) "Pensioner Tot.",
            province = "",num = sum(num))
total_owners <- as.data.frame(total_owners)
updated_range <- bind_rows(Solv_prov, total_prov,total_owners)
updated_range<-updated_range%>%
  rename("Solvency Type"=solvency.pf,"Province"=province,"N Solvent"=num)%>%
  arrange(factor(`Solvency Type`, levels = c("Total","Employee-Permanent Tot.", "employee-permanent",
                                             "Employee-Temporary Tot.","employee-temporary",
                                             "Pensioner Tot.","pensioner")))
updated_range$`Solvency Type` <- str_to_title(updated_range$`Solvency Type`)
updated_range$Province <- str_to_title(updated_range$Province)


Solv_table<-Entities%>%
  filter(type.subject == "individual" & solvency.pf %in% c("pensioner", "employee-temporary", "employee-permanent")) %>%
  group_by(solvency.pf)%>%
  summarise(max=max(income.pf),min=min(income.pf))%>%
  arrange(solvency.pf) %>%
  rename("Solvency type"=solvency.pf,"Max Income Net"=max,"Min Income Net"=min)
Solv_table$`Solvency type` <- str_to_title(Solv_table$`Solvency type`)

#---------------------b)Info_pg---------------------------------------------####
Type_table<-Entities%>%
  filter(type.subject=="corporate")%>%
  group_by(type.pg)%>%
  summarise(num=n())%>%
  rename(Type=type.pg)
Type_table$Type <- ifelse(is.na(Type_table$Type), "N/A", Type_table$Type)
total_type <- data.frame(
  Type = "Total",num = sum(Type_table$num))
updated_type <- bind_rows(Type_table, total_type)
updated_type<-updated_type%>%
  arrange(factor(Type,levels = c("Total","srl", "snc","sc","sas","spa","other","N/A")))%>%
  rename("N Corporate"=num)
updated_type$Type <- str_to_title(updated_type$Type)


Status_table<-Entities%>%
  filter(type.subject=="corporate")%>%
  group_by(status.pg)%>%
  summarise(num=n())%>%
  rename(Status=status.pg)
Status_table$Status <- ifelse(is.na(Status_table$Status), "N/A", Status_table$Status)
total_status <- data.frame(
  Status = "Total",num = sum(Status_table$num))
updated_status <- bind_rows(Status_table, total_status)
updated_status<-updated_status%>%
  arrange(factor(Status,levels = c("Total","active", "bankruptcy","canceled","insolvency","liquidation","N/A")))%>%
  rename("N Corporate"=num)
updated_status$Status <- str_to_title(updated_status$Status)

#age
Entities$range.age<-as.character(Entities$range.age)
age <- Entities%>%
  filter(type.subject=="individual")%>%
  group_by(solvency.pf,range.age)%>%
  summarise(num=n_distinct(id.entity))
age_total <- data.frame(solvency.pf="Total",range.age="",num=sum(age$num))

total_age <- age %>%
  filter(!is.na(solvency.pf)) %>%
  group_by(solvency.pf) %>%
  summarise(
    solvency.pf = if ("employee-permanent" %in% solvency.pf) "Employee-Permanent Tot."
    else if ("employee-temporary" %in% solvency.pf) "Employee-Temporary Tot."
    else if ("Insolvent" %in% solvency.pf) "Insolvent Tot."
    else if ("pensioner" %in% solvency.pf) "Pensioner Tot.",
    range.age = "",
    num = sum(num)
  ) %>%
  ungroup()

total_age <- as.data.frame(total_age)
updated_age <- bind_rows(age, age_total,total_age)%>%
  arrange(factor(solvency.pf,levels=c("Total","Employee-Permanent Tot.", "employee-permanent",
                                    "Employee-Temporary Tot.","employee-temporary",
                                    "Pensioner Tot.","pensioner","Insolvent Tot.","Insolvent")))%>%
  rename("Solvency Type"=solvency.pf,"Range age"=range.age,"N Individual"=num)
updated_age$`Range age` <- ifelse(is.na(updated_age$`Range age`), "-", updated_age$`Range age`)
updated_age$`Solvency Type` <- ifelse(is.na(updated_age$`Solvency Type`), "N/A", updated_age$`Solvency Type`)
updated_age$`Solvency Type` <- str_to_title(updated_age$`Solvency Type`)

#type subject
subject<-Entities%>%
  group_by(type.subject)%>%
  summarise(num=n())
subject$type.subject<-as.character(subject$type.subject)
subject$type.subject <- ifelse(is.na(subject$type.subject), "N/A", subject$type.subject)
total_subject <- data.frame(
  type.subject= "Total",num = sum(subject$num))
updated_subject <- bind_rows(subject, total_subject)
updated_subject<-updated_subject%>%
  arrange(factor(type.subject,levels = c("Total","individual","corporate","confidi")))%>%
  rename("N Entities"=num,"Type Subject"=type.subject)
updated_subject$`Type Subject` <- str_to_title(updated_subject$`Type Subject`)
#---------------Excel------------------####

source("Excel_format.R")
wb <- loadWorkbook("Tables.xlsx")
addWorksheet(wb, "Report_InfoProviding")
showGridLines(wb, sheet = 3, showGridLines = FALSE)

#formatting subject
writeDataTable(wb, 3, updated_subject, startRow = startRow, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "number")
applyStylesToColumns(wb,3, updated_subject, column_types, startRow,startCol)
writeData(wb, 3, x = "Entities by Type of Subject", startCol =startCol, startRow = startRow-1)
mergeCells(wb, 3, startCol:(startCol + ncol(updated_subject) - 1), rows = startRow-1)
applyCustomStyles(wb, 3, updated_subject, startRow, startCol)
addStyle(wb, sheet = 3, style = highlight_value, rows = startRow+2, cols = startCol+1,stack = TRUE)

startRow_sol<-startRow+nrow(updated_subject)+6


#formatting insolvent
startColu<-startCol+5
startRow_updated <- startRow_sol
writeDataTable(wb, 3, updated_ins, startRow = startRow_updated, startCol = startColu, tableStyle = "TableStylelight9")
column_types <- c("general", "number")
applyStylesToColumns(wb,3, updated_ins, column_types, startRow_updated,startColu)
writeData(wb, 3, x = "Top 3 Province by Insolvent Individual", startCol =startColu, startRow = startRow_updated-1)
mergeCells(wb, 3, startColu:(startColu + ncol(updated_ins) - 1), rows = startRow_updated-1)
applyCustomStyles(wb, 3, updated_ins, startRow_updated, startColu)
addStyle(wb, sheet = 3, style = highlight_value, rows = startRow_updated+2, cols = startColu+1,stack = TRUE)


#formatting na
startColu<-startCol+5
startRow_na <- startRow_sol+11
writeDataTable(wb, 3, updated_na, startRow = startRow_na, startCol = startColu, tableStyle = "TableStylelight9")
column_types <- c("general", "number")
applyStylesToColumns(wb,3, updated_na, column_types, startRow_na,startColu)
writeData(wb, 3, x = "Top 3 Province by Individual N/A", startCol =startColu, startRow = startRow_na-1)
mergeCells(wb, 3, startColu:(startColu + ncol(updated_na) - 1), rows = startRow_na-1)
applyCustomStyles(wb, 3, updated_na, startRow_na, startColu)
addStyle(wb, sheet = 3, style = highlight_value, rows = startRow_na+2, cols = startColu+1,stack = TRUE)

#formatting solv income
startRow_income <- startRow_sol+nrow(updated_range)+5
writeDataTable(wb, 3, Solv_table, startRow = startRow_income, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "number","number")
applyStylesToColumns(wb,3, Solv_table, column_types, startRow_income,startCol)
writeData(wb, 3, x = "Solvent Income Net", startCol =startCol, startRow = startRow_income-1)
mergeCells(wb, 3, startCol:(startCol + ncol(Solv_table) - 1), rows = startRow_income-1)
addStyle(wb, 3, style = title_style,rows = startRow_income-1, cols = startCol:(startCol + ncol(Solv_table) - 1), gridExpand = TRUE)
addStyle(wb, 3, style = section_style,rows = startRow_income+nrow(Solv_table),
         cols = startCol:(startCol + ncol(Solv_table) - 1), gridExpand = TRUE,stack = TRUE)
#formatting pg type
startRow_corp_type <- startRow_income+nrow(Solv_table)+3
writeDataTable(wb, 3, updated_type, startRow = startRow_corp_type, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "number")
applyStylesToColumns(wb,3, updated_type, column_types, startRow_corp_type,startCol)
writeData(wb, 3, x = "Corporate Type", startCol =startCol, startRow = startRow_corp_type-1)
mergeCells(wb, 3, startCol:(startCol + ncol(updated_type) - 1), rows = startRow_corp_type-1)
applyCustomStyles(wb, 3, updated_type, startRow_corp_type, startCol)
addStyle(wb, sheet = 3, style = highlight_value, rows = startRow_corp_type+2, cols = startCol+1,stack = TRUE)

#formatting pg status
startColsta<-startCol+5
startRow_corp_status <- startRow_corp_type
writeDataTable(wb, 3, updated_status, startRow = startRow_corp_status, startCol = startColsta, tableStyle = "TableStylelight9")
column_types <- c("general", "number")
applyStylesToColumns(wb,3, updated_status, column_types, startRow_corp_status,startColsta)
writeData(wb, 3, x = "Corporate Status", startCol =startColsta, startRow = startRow_corp_status-1)
mergeCells(wb, 3, startColsta:(startColsta + ncol(updated_status) - 1), rows = startRow_corp_status-1)
applyCustomStyles(wb, 3, updated_status, startRow_corp_status, startColsta)
addStyle(wb, sheet = 3, style = highlight_value, rows = startRow_corp_status+2, cols = startColsta+1,stack = TRUE)
#formatting age
writeDataTable(wb,3, updated_age, startRow = startRow_updated, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general","general", "number")
applyStylesToColumns(wb,3, updated_age, column_types, startRow_updated, startCol)
writeData(wb, 3, x = "Range Age by Individual", startCol, startRow_updated-1)
mergeCells(wb, 3, startCol:(startCol + ncol(updated_age) - 1),startRow_updated-1)
addStyle(wb, sheet = 3, style = highlight_value, rows = startRow_updated+4, cols = startCol+2,stack = TRUE)
addStyle(wb, sheet = 3, style = highlight_value, rows = startRow_updated+7, cols = startCol+2,stack = TRUE)
addStyle(wb, sheet = 3, style = highlight_value, rows = startRow_updated+11, cols = startCol+2,stack = TRUE)
addStyle(wb, sheet = 3, style = highlight_value, rows = startRow_updated+14, cols = startCol+2,stack = TRUE)
addStyle(wb,sheet=3,style=createStyle(border = "Bottom",borderColour ="black",borderStyle= "thin"),
         rows=startRow_income-4,cols = startCol:(startCol + ncol(updated_age) - 1), gridExpand = TRUE,stack = TRUE)
applyCustomStyles(wb,3, updated_age, startRow_updated, startCol)



#Range solvent

startcoll<-startColu+4
writeDataTable(wb, 3, updated_range, startRow = startRow_sol, startCol = startcoll, tableStyle = "TableStylelight9")
column_types <- c("general","general","number")
applyStylesToColumns(wb,3, updated_range, column_types, startRow_sol, startcoll)
writeData(wb, 3, x = "Top 3 Province by Solvent Individual", startCol = startcoll, startRow = startRow_sol-1)
mergeCells(wb, 3, startcoll:(startcoll + ncol(updated_range) - 1), rows = startRow_sol-1)
applyCustomStyles(wb, 3, updated_range, startRow_sol, startcoll)
addStyle(wb, sheet = 3, style = highlight_value, rows = startRow_sol+3, cols = startcoll+2,stack = TRUE)
addStyle(wb, sheet = 3, style = highlight_value, rows = startRow_sol+8, cols = startcoll+2,stack = TRUE)
addStyle(wb, sheet = 3, style = highlight_value, rows = startRow_sol+13, cols = startcoll+2,stack = TRUE)

startRow_ga <- startRow_corp_type+nrow(updated_type)+3
writeDataTable(wb, 3, updated_gaur, startRow = startRow_ga, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general","general","number","number","number")
applyStylesToColumns(wb,3, updated_gaur, column_types, startRow_ga, startCol)
writeData(wb, 3, x = "Type of Guarantor by Borrower + Solvency Individual Guarantor", startCol = startCol, startRow = startRow_ga-1)
mergeCells(wb, 3, startCol:(startCol + ncol(updated_gaur) - 1), rows = startRow_ga-1)
applyCustomStyles(wb, 3, updated_gaur, startRow_ga, startCol)






saveWorkbook(wb,"Tables.xlsx", overwrite = TRUE)

