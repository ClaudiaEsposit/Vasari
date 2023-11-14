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
numb.loans <- n_distinct(Loans_table$id.loan)
sum.gbv <- sum(Loans_table$gbv.original)
sum.borr <- n_distinct(Loans_table$id.bor)
average.loan <- sum.gbv/numb.loans
average.borr <- sum.gbv/sum.borr
sum.principal_tot <-sum(Loans_table$principal)

col_tot <- c("N Borrowers" ,"N Loans","GBV(€k)",  "Avg GBV per Borrower(€k)","Avg GBV per Loans(€k)")
total_table <- data.frame(
  "N Borrowers" = sum.borr,
  "N Loans" = numb.loans,
  "GBV(€k)" = sum.gbv,
  "Avg GBV per Borrower(€k)" = sum.gbv / sum.borr,
  "Avg GBV per Loans(€k)" = sum.gbv / numb.loans
)
colnames(total_table) <- col_tot
cutoff.date <- as.Date("2023-04-29")
entities.loans <- Entities  %>% 
  left_join(link.counterparties.entities, by= 'id.entity',relationship = "many-to-many") %>% 
  left_join(Counterparties,by = 'id.counterparty') %>%
  filter(role=='borrower' ) %>%
  left_join(Loans_table,by = 'id.bor',relationship = "many-to-many")


ent.by.type <- entities.loans %>% 
  select(type.subject,id.bor,gbv.original,province,type,area) %>% distinct(id.bor,gbv.original,area,.keep_all = TRUE)%>%
  group_by(id.bor) %>% 
  summarise(gbv.original=sum(gbv.original),area=first(area),province=first(province),
            type=first(type),type.subject=first(type.subject))

#gbv range column
add_gbv_range <- function(data) {
  breaks <- c(0, 50000,100000, Inf)
  labels <- c("0-50k", "50-100k", "100k+")
  result <- data %>%
    mutate(
      range.gbv = cut(gbv.original, breaks = breaks, labels = labels, right = FALSE)
    )
  return(result)
}
ent.by.type <-add_gbv_range(ent.by.type)
gbv_help <- ent.by.type %>%
  group_by(type.subject) %>%
  summarize(
    numb.borr = n_distinct(id.bor),perc.borr = sum(numb.borr) / sum(sum.borr),gbv_tot = sum(gbv.original),
    mean.gbv = gbv_tot/numb.borr,perc.gbv = sum(gbv_tot) / sum(sum.gbv)
  )

total_row <- data.frame(
  type.subject = "Total",numb.borr = sum(gbv_help$numb.borr),
  gbv_tot = sum(gbv_help$gbv_tot), perc.borr = sum(gbv_help$perc.borr),
  mean.gbv = sum(gbv_help$mean.gbv * gbv_help$numb.borr) / sum(gbv_help$numb.borr),
  perc.gbv = sum(gbv_help$perc.gbv))

updated_df <- bind_rows(gbv_help, total_row)
updated_df<-updated_df%>%
  rename("Type of subject"=type.subject,"N Bor"=numb.borr,
         "% Bor"=perc.borr,"GBV(€k)"=gbv_tot,"Mean GBV(€k)"=mean.gbv,"% GBV"=perc.gbv)%>%
  arrange(factor(`Type of subject`, levels = c("Total","individual", "co-owners","legal person")))
updated_df$`Type of subject` <- str_to_title(updated_df$`Type of subject`)



# RANGE GBV
range <- ent.by.type %>%
  group_by(type.subject,range.gbv) %>%
  summarize(
    numb.borr = n_distinct(id.bor),perc.borr = sum(numb.borr) / sum(sum.borr),gbv_tot = sum(gbv.original),
    mean.gbv = gbv_tot/numb.borr,perc.gbv = sum(gbv_tot) / sum(sum.gbv)
  )

total_row_range <- data.frame(
  type.subject = "Total",range.gbv = "",
  gbv_tot = sum(range$gbv_tot),numb.borr = sum(range$numb.borr), perc.borr = sum(range$perc.borr),
  mean.gbv = sum(range$mean.gbv * range$numb.borr) / sum(range$numb.borr),
  perc.gbv = sum(range$perc.gbv))

total_owners <- range %>%
  group_by(type.subject) %>%
  summarize(type.subject = if ("individual" %in% type.subject) "Individual Tot."
            else if ("corporate" %in% type.subject) "Corporate Tot.",
            range.gbv = "",gbv_tot = sum(gbv_tot),
            numb.borr = sum(numb.borr),perc.borr = sum(perc.borr),
            mean.gbv = gbv_tot / numb.borr,
            perc.gbv = sum(perc.gbv))
total_owners <- as.data.frame(total_owners)

updated_range <- bind_rows(range, total_row_range,total_owners)
updated_range<-updated_range%>%
  rename("Type of subject"=type.subject,"Range GBV"=range.gbv,"N Bor"=numb.borr,
         "% Bor"=perc.borr,"GBV(€k)"=gbv_tot,"Mean GBV(€k)"=mean.gbv,"% GBV"=perc.gbv)%>%
  arrange(factor(`Type of subject`, levels = c("Total","Individual Tot.", "individual","Corporate Tot.","corporate")),
          factor(`Range GBV`,levels = c("0-50k", "50-100k", "100k+")))
updated_range$`Type of subject` <- str_to_title(updated_range$`Type of subject`)
updated_range$`Range GBV` <- str_to_title(updated_range$`Range GBV`)

#loans
type_loans <- ent.by.type %>%
  group_by(type) %>%
  summarize(
    numb.borr = n_distinct(id.bor),perc.borr = sum(numb.borr) / sum(sum.borr),gbv_tot = sum(gbv.original),
    mean.gbv = gbv_tot/numb.borr,perc.gbv = sum(gbv_tot) / sum(sum.gbv)
  )

total_row_loans <- data.frame(
  type = "Total",gbv_tot = sum(type_loans$gbv_tot),numb.borr = sum(type_loans$numb.borr), perc.borr = sum(type_loans$perc.borr),
  mean.gbv = sum(type_loans$mean.gbv * type_loans$numb.borr) / sum(type_loans$numb.borr),
  perc.gbv = sum(type_loans$perc.gbv))

updated_loans <- bind_rows(type_loans, total_row_loans)
updated_loans<-updated_loans%>%
  rename("Type of Credit"=type,"N Bor"=numb.borr,
         "% Bor"=perc.borr,"GBV(€k)"=gbv_tot,"Mean GBV(€k)"=mean.gbv,"% GBV"=perc.gbv)%>%
  arrange(factor(`Type of Credit`, levels = c("Total","Bank Accounts", "Mortgages","Personal Loans")))
updated_loans$`Type of Credit` <- str_to_title(updated_loans$`Type of Credit`)


# province
province_true <- ent.by.type %>%
  group_by(province) %>%
  summarize(
    numb.borr = n_distinct(id.bor),perc.borr = sum(numb.borr) / sum(sum.borr),gbv_tot = sum(gbv.original),
    mean.gbv = gbv_tot/numb.borr,perc.gbv = sum(gbv_tot) / sum(sum.gbv)
  )
total_row_prot <- data.frame(
  province = "Total",numb.borr = sum(province_true$numb.borr), perc.borr = sum(province_true$perc.borr),
  gbv_tot = sum(province_true$gbv_tot),
  mean.gbv = sum(province_true$mean.gbv * province_true$numb.borr) / sum(province_true$numb.borr),
  perc.gbv = sum(province_true$perc.gbv))
updated_prot <- bind_rows(province_true, total_row_prot)
updated_prot<-updated_prot%>%
  rename("Province"=province,"N Bor"=numb.borr,
         "% Bor"=perc.borr,"GBV(€k)"=gbv_tot,"Mean GBV(€k)"=mean.gbv,"% GBV"=perc.gbv)%>%
  arrange(factor(`Province`, levels = c("Total")))
updated_prot$Province <- str_to_title(updated_prot$Province)
updated_prot$Province<- ifelse(is.na(updated_prot$Province),"N/A", updated_prot$Province)
updated_prot <- updated_prot[order(-updated_prot$`GBV(€k)`), ]
first_5_rowst <- updated_prot[1:6, ]
updated_pro2t <- updated_prot[order(-updated_prot$`N Bor`), ]
first_5_rows2t <- updated_pro2t[1:6, ]

# area
area <- ent.by.type %>%
  group_by(area) %>%
  summarize(
    numb.borr = n_distinct(id.bor),perc.borr = sum(numb.borr) / sum(sum.borr),gbv_tot = sum(gbv.original),
    mean.gbv = gbv_tot/numb.borr,perc.gbv = sum(gbv_tot) / sum(sum.gbv)
  )
total_row_area <- data.frame(
  area = "Total",numb.borr = sum(area$numb.borr), perc.borr = sum(area$perc.borr),
  gbv_tot = sum(area$gbv_tot),
  mean.gbv = sum(area$mean.gbv * area$numb.borr) / sum(area$numb.borr),
  perc.gbv = sum(area$perc.gbv))
updated_area <- bind_rows(area, total_row_area)
updated_area<-updated_area%>%
  rename("Area"=area,"N Bor"=numb.borr,
         "% Bor"=perc.borr,"GBV(€k)"=gbv_tot,"Mean GBV(€k)"=mean.gbv,"% GBV"=perc.gbv)%>%
  arrange(factor(`Area`, levels = c("Total")))
updated_area$Area <- str_to_title(updated_area$Area)
updated_area$Area<- ifelse(is.na(updated_area$Area),"N/A", updated_area$Area)
updated_area <- updated_area[order(-updated_area$`GBV(€k)`), ]

rows_to_merge <- updated_area %>%
  filter(str_detect(Area, "South|Islands"))

# Create a new row for "South and Islands" and calculate the summary statistics
new_row <- rows_to_merge %>%
  summarise(
    Area = "South and Islands",
    `N Bor` = sum(`N Bor`),
    `% Bor` = sum(`% Bor`),
    `GBV(€k)` = sum(`GBV(€k)`),
    `Mean GBV(€k)` = sum(`Mean GBV(€k)` * `N Bor`) / sum(`N Bor`),
    `% GBV` = sum(`% GBV`)
  )

# Filter out the rows for "South" and "Islands"
updated_area <- updated_area %>%
  filter(!str_detect(Area, "South|Islands"))

# Add the new row for "South and Islands" to the data frame
updated_area <- bind_rows(updated_area, new_row)

# Reorder the 'Area' column
updated_area$Area <- str_to_title(updated_area$Area)
updated_area$Area <- ifelse(is.na(updated_area$Area), "N/A", updated_area$Area)
updated_area <- updated_area[order(-updated_area$`GBV(€k)`), ]

#SEC/UNSEC + RANGE GBV
range_total <- ent.by.type %>%
  group_by(range.gbv) %>%
  summarize(
    numb.borr = n_distinct(id.bor),perc.borr = sum(numb.borr) / sum(sum.borr),gbv_tot = sum(gbv.original),
    mean.gbv = gbv_tot/numb.borr,perc.gbv = sum(gbv_tot) / sum(sum.gbv)
  )

total_row_range_tot <- data.frame(
  range.gbv = "Total",
  gbv_tot = sum(range$gbv_tot),numb.borr = sum(range$numb.borr), perc.borr = sum(range$perc.borr),
  mean.gbv = sum(range$mean.gbv * range$numb.borr) / sum(range$numb.borr),
  perc.gbv = sum(range$perc.gbv))



updated_range_tot <- bind_rows(range_total, total_row_range_tot)
updated_range_tot<-updated_range_tot%>%
  rename("Range GBV"=range.gbv,"N Bor"=numb.borr,
         "% Bor"=perc.borr,"GBV(€k)"=gbv_tot,"Mean GBV(€k)"=mean.gbv,"% GBV"=perc.gbv)%>%
  arrange(factor(`Range GBV`,levels = c("Total","0-50k", "50-100k","100k+")))
updated_range_tot$`Range GBV` <- str_to_title(updated_range_tot$`Range GBV`)


#------------------------------d)Excel-------------------------------------####

source("Excel_format.R")
wb <- loadWorkbook("Tables.xlsx")
addWorksheet(wb, "Report_Borrowers")
showGridLines(wb, sheet = 2, showGridLines = FALSE)
writeDataTable(wb, 2, total_table, startRow = startRow, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("number","number","currency", "currency","currency")
applyStylesToColumns(wb,2, total_table, column_types, startRow, startCol)
writeData(wb, 2, x = "Overview", startCol = startCol, startRow = startRow-1)
mergeCells(wb, 2, startCol:(startCol + ncol(total_table) - 1), rows = startRow-1)
addStyle(wb, 2, style = title_style,rows = startRow-1, cols = startCol:(startCol + ncol(total_table) - 1), gridExpand = TRUE)
addStyle(wb, 2, style = section_style,rows = startRow+nrow(total_table),
         cols = startCol:(startCol + ncol(total_table) - 1), gridExpand = TRUE,stack = TRUE)

#formatting type subject
startRow_updated <- startRow+nrow(total_table)+3
writeDataTable(wb, 2, updated_df, startRow = startRow_updated, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "number", "percentage", "currency", "currency", "percentage")
applyStylesToColumns(wb,2, updated_df, column_types, startRow_updated, startCol)
writeData(wb, 2, x = "GBV per Type of Subject", startCol = startCol, startRow = startRow_updated-1)
mergeCells(wb, 2, startCol:(startCol + ncol(updated_df) - 1), rows = startRow_updated-1)
applyCustomStyles(wb, 2, updated_df, startRow_updated, startCol)
addStyle(wb, sheet = 2, style = highlight_value, rows = startRow_updated+2, cols = startCol+1,stack = TRUE)
addStyle(wb, sheet = 2, style = highlight_value, rows = startRow_updated+3, cols = startCol+3,stack = TRUE)


#formatting range
startRow_range <- startRow_updated+nrow(updated_df)+3
writeDataTable(wb,2, updated_range, startRow = startRow_range, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "general", "number", "percentage", "currency", "currency", "percentage")
applyStylesToColumns(wb,2, updated_range, column_types, startRow_range, startCol)
writeData(wb, 2, x = "GBV per Type of Subject + Range GBV", startCol = startCol, startRow = startRow_range-1)
mergeCells(wb, 2, startCol:(startCol + ncol(updated_range) - 1), rows = startRow_range-1)
applyCustomStyles(wb,2, updated_range, startRow_range, startCol)
addStyle(wb, sheet = 2, style = highlight_value, rows = startRow_range+5, cols = startCol+2,stack = TRUE)
addStyle(wb, sheet = 2, style = highlight_value, rows = startRow_range+9, cols = startCol+2,stack = TRUE)
addStyle(wb, sheet = 2, style = highlight_value, rows = startRow_range+5, cols = startCol+4,stack = TRUE)
addStyle(wb, sheet = 2, style = highlight_value, rows = startRow_range+9, cols = startCol+4,stack = TRUE)

#formatting loans
startRow_loans <- startRow_range+nrow(updated_range)+3
writeDataTable(wb,2, updated_loans, startRow = startRow_loans, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "number", "percentage", "currency", "currency", "percentage")
applyStylesToColumns(wb,2, updated_loans, column_types, startRow_loans, startCol)
writeData(wb, 2, x = "Type Loans by Borrower", startCol, startRow_loans-1)
mergeCells(wb, 2, startCol:(startCol + ncol(updated_loans) - 1),startRow_loans-1)
applyCustomStyles(wb,2, updated_loans, startRow_loans, startCol)
addStyle(wb, sheet = 2, style = highlight_value, rows = startRow_loans+2, cols = startCol+1,stack = TRUE)
addStyle(wb, sheet = 2, style = highlight_value, rows = startRow_loans+2, cols = startCol+3,stack = TRUE)

#formatting province
startRow_prot <- startRow_loans+nrow(updated_loans)+3
writeDataTable(wb,2, first_5_rowst, startRow = startRow_prot, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "number", "percentage", "currency", "currency", "percentage")
applyStylesToColumns(wb,2, first_5_rowst, column_types, startRow_prot, startCol)
writeData(wb, 2, x = "Top 5 Province by GBV", startCol, startRow_prot-1)
mergeCells(wb, 2, startCol:(startCol + ncol(first_5_rowst) - 1),startRow_prot-1)
applyCustomStyles(wb,2, first_5_rowst, startRow_prot, startCol)

#Area
startRow_area <- startRow_prot+nrow(first_5_rowst)+3
writeDataTable(wb,2, updated_area, startRow = startRow_area, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "number", "percentage", "currency", "currency", "percentage")
applyStylesToColumns(wb,2, updated_area, column_types, startRow_area, startCol)
writeData(wb, 2, x = "GBV by Area", startCol, startRow_area-1)
mergeCells(wb, 2, startCol:(startCol + ncol(updated_area) - 1),startRow_area-1)
applyCustomStyles(wb,2, updated_area, startRow_area, startCol)

#Range tot gbv
startRow_GBV <- startRow_area+nrow(updated_area)+3
writeDataTable(wb,2, updated_range_tot, startRow = startRow_GBV, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "number", "percentage", "currency", "currency", "percentage", "currency", "currency", "percentage")
applyStylesToColumns(wb,2, updated_range_tot, column_types, startRow_GBV, startCol)
writeData(wb, 2, x = "Borrower by Range GBV", startCol, startRow_GBV-1)
mergeCells(wb, 2, startCol:(startCol + ncol(updated_range_tot) - 1),startRow_GBV-1)
applyCustomStyles(wb,2, updated_range_tot, startRow_GBV, startCol)
addStyle(wb, sheet = 2, style = highlight_value, rows = startRow_GBV+4, cols = startCol+1,stack = TRUE)
addStyle(wb, sheet = 2, style = highlight_value, rows = startRow_GBV+4, cols = startCol+3,stack = TRUE)



#------------------------------e)Plot-------------------------------------####

plot_prvince <- first_5_rowst[-1,]
plot_prvince <- plot_prvince %>%
  arrange(`GBV(€k)`)
plot_prvince$Province <- factor(plot_prvince$Province, levels = plot_prvince$Province)
plot_prvince$GBV_formatted <- scales::number(plot_prvince$`GBV(€k)`, scale = 1e-6, accuracy = 0.1)
province_plot <- ggplot(data = plot_prvince, aes(x = `GBV(€k)`, y = `Province`)) +
  geom_bar(stat = "identity", fill = "slateblue4") +
  geom_smooth() +
  labs(
    title = "Top 5 Province by GBV",
    subtitle = "Borrower level",
    x = "Total GBV(€k)",
    y = "Province"
  ) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal()
province_plot_with_labels <- province_plot +
  geom_text(aes(label = GBV_formatted), hjust = -0.1, vjust = 0.4)
print(province_plot_with_labels)

insertPlot(wb,2,width = 6,height = 4,xy = NULL,startRow = startRow_updated,
           startCol = length(first_5_rowst) + 4,fileType = "png",units = "in",dpi = 300
)


plot_area <- updated_area[-1,]
plot_area <- plot_area %>%
  arrange(`GBV(€k)`)
plot_area$Area <- factor(plot_area$Area, levels = plot_area$Area)
plot_area$GBV_formatted <- scales::number(plot_area$`GBV(€k)`, scale = 1e-6, accuracy = 0.1)
area_plot <- ggplot(data = plot_area, aes(x = `GBV(€k)`, y = `Area`)) +
  geom_bar(stat = "identity", fill = "slateblue4") +
  geom_smooth() +
  labs(
    title = "GBV by Area",
    subtitle = "Borrower level",
    x = "Total GBV(€k)",
    y = "Area"
  ) +
  scale_x_continuous(labels = scales::comma) +
  theme_minimal()
area_plot_with_labels <- area_plot +
  geom_text(aes(label = GBV_formatted), hjust = -0.1, vjust = 0.4)
print(area_plot_with_labels)

insertPlot(wb,2,width = 6,height = 4,xy = NULL,startRow = startRow_prot,
           startCol = length(first_5_rowst) + 4,fileType = "png",units = "in",dpi = 300
)
saveWorkbook(wb,"Tables.xlsx", overwrite = TRUE)

