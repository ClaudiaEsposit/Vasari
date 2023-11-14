cat('\014')
rm(list=ls())
setwd("~/Vasari")
source("Library.R")
source("Functions.R")
source("Vanilla.R")
source("Tables.R")
source("Info_providing.R")
#---------------a)Info_pf-------------####
Insolvent_table<-Entities%>%
  filter(type.subject=="individual")%>%
  filter(solvency.pf=="Insolvent")%>%
  group_by(province)%>%
  summarise(num=n())%>%
  top_n(5, num) %>%
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


Solv_prov<-Entities%>%
  filter(type.subject == "individual" & solvency.pf %in% c("pensioner", "employee-temporary", "employee-permanent")) %>%
  group_by(solvency.pf,province)%>%
  summarise(num=n())%>%
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
  rename("Solvency type"=solvency.pf,"Max Income"=max,"Min Income"=min)
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
  arrange(factor(Status,levels = c("Total","active", "ceased","canceled","insolvency","liquidation","N/A")))%>%
  rename("N Corporate"=num)
updated_status$Status <- str_to_title(updated_status$Status)

#---------------Excel------------------####

source("Excel_format.R")
wb <- loadWorkbook("Tables.xlsx")
addWorksheet(wb, "Report_InfoProviding")
showGridLines(wb, sheet = 3, showGridLines = FALSE)
writeDataTable(wb, 3, updated_range, startRow = startRow, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general","general","number")
applyStylesToColumns(wb,3, updated_range, column_types, startRow, startCol)
writeData(wb, 3, x = "Solvent Individual", startCol = startCol, startRow = startRow-1)
mergeCells(wb, 3, startCol:(startCol + ncol(updated_range) - 1), rows = startRow-1)
applyCustomStyles(wb, 3, updated_range, startRow, startCol)
addStyle(wb, sheet = 3, style = highlight_value, rows = startRow+3, cols = startCol+2,stack = TRUE)
addStyle(wb, sheet = 3, style = highlight_value, rows = startRow+7, cols = startCol+2,stack = TRUE)
addStyle(wb, sheet = 3, style = highlight_value, rows = startRow+12, cols = startCol+2,stack = TRUE)

#formatting insolvent
startColu<-startCol+5
startRow_updated <- startRow
writeDataTable(wb, 3, updated_ins, startRow = startRow_updated, startCol = startColu, tableStyle = "TableStylelight9")
column_types <- c("general", "number")
applyStylesToColumns(wb,3, updated_ins, column_types, startRow_updated,startColu)
writeData(wb, 3, x = "Insolvent Individual", startCol =startColu, startRow = startRow_updated-1)
mergeCells(wb, 3, startColu:(startColu + ncol(updated_ins) - 1), rows = startRow_updated-1)
applyCustomStyles(wb, 3, updated_ins, startRow_updated, startColu)
addStyle(wb, sheet = 3, style = highlight_value, rows = startRow_updated+2, cols = startColu+1,stack = TRUE)

#formatting solv income
startRow_income <- startRow+nrow(updated_range)+3
writeDataTable(wb, 3, Solv_table, startRow = startRow_income, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "number","number")
applyStylesToColumns(wb,3, Solv_table, column_types, startRow_income,startCol)
writeData(wb, 3, x = "Solvent Income", startCol =startCol, startRow = startRow_income-1)
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

saveWorkbook(wb,"Tables.xlsx", overwrite = TRUE)

