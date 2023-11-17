#---------------------------- a)Intro --------------------------------------####
# cat('\014')
# rm(list=ls())
# setwd("~/Vasari")
# source("Library.R")
# source("Functions.R")
# source("Vanilla.R")
# source("Tables.R")
#-----------------------------c)Loans_level--------------------------------####
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


filtered_data <- Loans_table %>%
  group_by(id.loan,type) %>%
  summarize(
    gbv = sum(gbv.original),
    principal_tot = sum(principal),
    vintage = round(as.numeric(cutoff.date - date.status)/365,0)
  ) %>%
  ungroup()
Range_vintage <- c(0,3,5,Inf)
Range_vintage_labels <- c('0-3y','4-5y','5y+')
filtered_data$range.vintage <- cut(filtered_data$vintage, breaks = Range_vintage, labels = Range_vintage_labels, include.lowest = TRUE)
add_gbv_range_column <- function(data) {
  breaks <- c(0, 10000, 50000,100000, Inf)
  labels <- c("0-10k", "10-50k", "50-100k", "100k+")
  result <- data %>%
    mutate(
      range.gbv = cut(gbv, breaks = breaks, labels = labels, right = FALSE)
    )
  return(result)
}

filtered_data <-add_gbv_range_column(filtered_data)%>%select(-vintage)

#type_loans
gbv_help <- filtered_data %>%arrange(desc(gbv))%>%
  group_by(type) %>%
  summarize(
    numb.loan = n_distinct(id.loan),perc.loan = sum(numb.loan) / sum(numb.loans),gbv_tot = sum(gbv),
    mean.gbv = gbv_tot/numb.loan,perc.gbv = sum(gbv_tot) / sum(sum.gbv)
  )

total_row <- data.frame(
  type = "Total",numb.loan = sum(gbv_help$numb.loan),
  gbv_tot = sum(gbv_help$gbv_tot), perc.loan = sum(gbv_help$perc.loan),
  mean.gbv = sum(gbv_help$mean.gbv * gbv_help$numb.loan) / sum(gbv_help$numb.loan),
  perc.gbv = sum(gbv_help$perc.gbv))

updated_df <- bind_rows(gbv_help, total_row)
updated_df<-updated_df%>%
  rename("Type of Loans"=type,"N Loan"=numb.loan,
         "% Loan"=perc.loan,"GBV(€k)"=gbv_tot,"Mean GBV(€k)"=mean.gbv,"% GBV"=perc.gbv)%>%
  arrange(factor(`Type of Loans`, levels = c("Total","individual", "co-owners","legal person")))
updated_df$`Type of Loans` <- str_to_title(updated_df$`Type of Loans`)


#RANGE GBV
range <- filtered_data %>%
  group_by(type,range.gbv) %>%
  summarize(
    numb.loan = n(),perc.loan = sum(numb.loan) / sum(numb.loans),gbv_tot = sum(gbv),
    mean.gbv = gbv_tot/numb.loan,perc.gbv = sum(gbv_tot) / sum(sum.gbv)
  )
total_row_range <- data.frame(
  type = "Total",range.gbv=" ",numb.loan = sum(range$numb.loan),
  gbv_tot = sum(range$gbv_tot), perc.loan = sum(range$perc.loan),
  mean.gbv = sum(range$mean.gbv * range$numb.loan) / sum(range$numb.loan),
  perc.gbv = sum(range$perc.gbv))

total_range_col <- range %>%
  group_by(type) %>%
  summarize(type = if ("bank accounts" %in% type) "Bank Accounts Tot."
            else if ("mortgages" %in% type) "Mortgages Tot."
            else if ("personal loans" %in% type) "Personal Loans Tot.",
            range.gbv = "",gbv_tot = sum(gbv_tot),
            numb.loan = sum(numb.loan),perc.loan = sum(perc.loan),
            mean.gbv =gbv_tot / numb.loan,
            perc.gbv = sum(perc.gbv))
total_range_col <- as.data.frame(total_range_col)
updated_range <- bind_rows(range, total_row_range,total_range_col)
updated_range<-updated_range%>%
  rename("Type of Loans"=type,"Range GBV"=range.gbv,"N Loan"=numb.loan,
         "% Loan"=perc.loan,"GBV(€k)"=gbv_tot,"Mean GBV(€k)"=mean.gbv,"% GBV"=perc.gbv)%>%
  arrange(factor(`Range GBV`,levels = c("Total","0-10k", "10-50k", "50-100k", "100k+")))

custom_levels <- c("Total", "Bank Accounts Tot.","bank accounts", "Mortgages Tot.","mortgages","Personal Loans Tot.","personal loans")
updated_range$`Type of Loans` <- factor(updated_range$`Type of Loans`, levels = custom_levels)
updated_range <- updated_range %>%
  arrange(`Type of Loans`)
updated_range$`Type of Loans` <- str_to_title(updated_range$`Type of Loans`)
updated_range$`Range GBV` <- str_to_title(updated_range$`Range GBV`)

#vintage range

vintage <- filtered_data %>%
  group_by(type,range.vintage) %>%
  summarize(
    numb.loan = n(),perc.loan = sum(numb.loan) / sum(numb.loans),gbv_tot = sum(gbv),
    mean.gbv = gbv_tot/numb.loan,perc.gbv = sum(gbv_tot) / sum(sum.gbv),
  )

total_row_vintage <- data.frame(
  type = "Total",range.vintage = "",numb.loan = sum(vintage$numb.loan),
  gbv_tot = sum(vintage$gbv_tot), perc.loan = sum(vintage$perc.loan),
  mean.gbv = sum(vintage$mean.gbv * vintage$numb.loan) / sum(vintage$numb.loan),
  perc.gbv = sum(vintage$perc.gbv))

total_secured3 <- vintage %>%
  group_by(type) %>%
  summarize(type = if ("bank accounts" %in% type) "Bank Accounts Tot."
            else if ("mortgages" %in% type) "Mortgages Tot."
            else if ("personal loans" %in% type) "Personal Loans Tot.",
            range.vintage = "",gbv_tot = sum(gbv_tot),
            numb.loan = sum(numb.loan),perc.loan = sum(perc.loan),
            mean.gbv =gbv_tot / numb.loan,
            perc.gbv = sum(perc.gbv))
total_secured3 <- as.data.frame(total_secured3)
updated_vintage <- bind_rows(vintage, total_row_vintage,total_secured3)
updated_vintage$range.vintage[is.na(updated_vintage$range.vintage)] <- "utp"

updated_vintage<-updated_vintage%>%
  rename("Type of Loans"=type,"Range Vintage"=range.vintage,"N Loans"=numb.loan,
         "% Loans"=perc.loan,"GBV(€k)"=gbv_tot,"Mean GBV(€k)"=mean.gbv,"% GBV"=perc.gbv)

custom_levels <- c("Total", "Bank Accounts Tot.","bank accounts", "Mortgages Tot.","mortgages","Personal Loans Tot.","personal loans")
updated_vintage$`Type of Loans` <- factor(updated_vintage$`Type of Loans`, levels = custom_levels)
updated_vintage <- updated_vintage %>%
  arrange(`Type of Loans`, `Range Vintage`)
updated_vintage$`Type of Loans` <- str_to_title(updated_vintage$`Type of Loans`)
updated_vintage$`Range Vintage` <- str_to_title(updated_vintage$`Range Vintage`)


#RANGE GBV total
range_total <- filtered_data %>%
  group_by(range.gbv) %>%
  summarize(
    numb.loan = n(),perc.loan = sum(numb.loan) / sum(numb.loans),gbv_tot = sum(gbv),
    mean.gbv = gbv_tot/numb.loan,perc.gbv = sum(gbv_tot) / sum(sum.gbv),
  )

total_row_range_tot <- data.frame(
  range.gbv = "Total",
  gbv_tot = sum(range_total$gbv_tot),numb.loan = sum(range_total$numb.loan), perc.loan = sum(range_total$perc.loan),
  mean.gbv = sum(range_total$mean.gbv * range_total$numb.loan) / sum(range_total$numb.loan),
  perc.gbv = sum(range_total$perc.gbv))

updated_range_tot <- bind_rows(range_total, total_row_range_tot)
updated_range_tot<-updated_range_tot%>%
  rename("Range GBV"=range.gbv,"N Loans"=numb.loan,
         "% Loans"=perc.loan,"GBV(€k)"=gbv_tot,"Mean GBV(€k)"=mean.gbv,"% GBV"=perc.gbv)%>%
  arrange(factor(`Range GBV`,levels = c("Total","0-10k", "10-50k","50-100k","100k+")))
updated_range_tot$`Range GBV` <- str_to_title(updated_range_tot$`Range GBV`)


#RANGE vintage total
vintage_total <- filtered_data %>%
  group_by(range.vintage) %>%
  summarize(
    numb.loan = n(),perc.loan = sum(numb.loan) / sum(numb.loans),gbv_tot = sum(gbv),
    mean.gbv = gbv_tot/numb.loan,perc.gbv = sum(gbv_tot) / sum(sum.gbv),
  )

total_row_vintage_tot <- data.frame(
  range.vintage = "Total",
  gbv_tot = sum(vintage_total$gbv_tot),numb.loan = sum(vintage_total$numb.loan), perc.loan = sum(vintage_total$perc.loan),
  mean.gbv = sum(vintage_total$mean.gbv * vintage_total$numb.loan) / sum(vintage_total$numb.loan),
  perc.gbv = sum(vintage_total$perc.gbv))

updated_vintage_tot <- bind_rows(vintage_total, total_row_vintage_tot)
updated_vintage_tot<-updated_vintage_tot%>%
  rename("Range Vintage"=range.vintage,"N Loans"=numb.loan,
         "% Loans"=perc.loan,"GBV(€k)"=gbv_tot,"Mean GBV(€k)"=mean.gbv,"% GBV"=perc.gbv)%>%
  arrange(factor(`Range Vintage`,levels = c("Total","0-3y", "4-5y","5y+")))
updated_vintage_tot$`Range Vintage` <- str_to_title(updated_vintage_tot$`Range Vintage`)

#-----------Excel----------------------------------------------------------####
source("Excel_format.R")
wb <- createWorkbook()
addWorksheet(wb, "Report_Loans")
showGridLines(wb, sheet = 1, showGridLines = FALSE)
writeDataTable(wb, 1, total_table, startRow = startRow, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("number","number","currency", "currency","currency")
applyStylesToColumns(wb,1, total_table, column_types, startRow, startCol)
writeData(wb, 1, x = "Overview", startCol = startCol, startRow = startRow-1)
mergeCells(wb, 1, startCol:(startCol + ncol(total_table) - 1), rows = startRow-1)
addStyle(wb, 1, style = title_style,rows = startRow-1, cols = startCol:(startCol + ncol(total_table) - 1), gridExpand = TRUE)
addStyle(wb, 1, style = section_style,rows = startRow+nrow(total_table),
         cols = startCol:(startCol + ncol(total_table) - 1), gridExpand = TRUE,stack = TRUE)

#formatting type by loans
startRow_updated <- startRow+nrow(total_table)+3
writeDataTable(wb, 1, updated_df, startRow = startRow_updated, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "number", "percentage", "currency", "currency", "percentage", "currency", "currency","percentage")
applyStylesToColumns(wb,1, updated_df,column_types, startRow_updated, startCol)
writeData(wb, 1, x = "Type of Loan", startCol = startCol, startRow = startRow_updated-1)
mergeCells(wb, 1, startCol:(startCol + ncol(updated_df) - 1), rows = startRow_updated-1)
applyCustomStyles(wb,1, updated_df, startRow_updated, startCol)
highlight_row<-startRow_updated+2
highlight_col<-startCol+1
addStyle(wb, sheet = 1, style = highlight_value, rows = highlight_row, cols = highlight_col,stack = TRUE)
addStyle(wb, sheet = 1, style = highlight_value, rows = highlight_row+1, cols = highlight_col+2,stack = TRUE)


#formatting range
startRow_range <- startRow_updated+nrow(updated_df)+3
writeDataTable(wb,1, updated_range, startRow = startRow_range, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general","general", "number", "percentage", "currency", "currency", "percentage", "currency", "currency","percentage")
applyStylesToColumns(wb,1, updated_range, column_types, startRow=startRow_range, startCol=startCol)
writeData(wb, 1, x = "Range GBV by type of Loans", startCol = startCol, startRow = startRow_range-1)
mergeCells(wb, 1, startCol:(startCol + ncol(updated_range) - 1), rows = startRow_range-1)
applyCustomStyles(wb,1, updated_range, startRow_range, startCol)
addStyle(wb, sheet = 1, style = highlight_value, rows = startRow_range+3, cols = highlight_col+1,stack = TRUE)
addStyle(wb, sheet = 1, style = highlight_value, rows = startRow_range+11, cols = highlight_col+1,stack = TRUE)

#formatting VINTAGE
startRow_vintage <- startRow_range+nrow(updated_range)+3
writeDataTable(wb,1, updated_vintage, startRow = startRow_vintage, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general","general", "number", "percentage", "currency", "currency", "percentage", "currency", "currency","percentage")
applyStylesToColumns(wb,1, updated_vintage, column_types, startRow=startRow_vintage, startCol=startCol)
writeData(wb, 1, x = "Range Vintage by Loan", startCol = startCol, startRow = startRow_vintage-1)
mergeCells(wb, 1, startCol:(startCol + ncol(updated_vintage) - 1), rows = startRow_vintage-1)
applyCustomStyles(wb,1, updated_vintage, startRow_vintage, startCol)
addStyle(wb, sheet = 1, style = highlight_value, rows = startRow_vintage+3, cols = highlight_col+1,stack = TRUE)
addStyle(wb, sheet = 1, style = highlight_value, rows = startRow_vintage+7, cols = highlight_col+1,stack = TRUE)
addStyle(wb, sheet = 1, style = highlight_value, rows = startRow_vintage+12, cols = highlight_col+1,stack = TRUE)


#Range tot gbv
startRow_GBV <- startRow_vintage+nrow(updated_vintage)+3
writeDataTable(wb,1, updated_range_tot, startRow = startRow_GBV, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "number", "percentage", "currency", "currency", "percentage")
applyStylesToColumns(wb,1, updated_range_tot, column_types, startRow_GBV, startCol)
writeData(wb, 1, x = "Range GBV by Loan", startCol, startRow_GBV-1)
mergeCells(wb, 1, startCol:(startCol + ncol(updated_range_tot) - 1),startRow_GBV-1)
applyCustomStyles(wb,1, updated_range_tot, startRow_GBV, startCol)


#Range tot vintage
startRow_vt <- startRow_GBV+nrow(updated_range_tot)+3
writeDataTable(wb,1, updated_vintage_tot, startRow = startRow_vt, startCol = startCol, tableStyle = "TableStylelight9")
column_types <- c("general", "number", "percentage", "currency", "currency", "percentage")
applyStylesToColumns(wb,1, updated_vintage_tot, column_types, startRow_vt, startCol)
writeData(wb, 1, x = "Range Vintage by Loan", startCol, startRow_vt-1)
mergeCells(wb, 1, startCol:(startCol + ncol(updated_vintage_tot) - 1),startRow_vt-1)
applyCustomStyles(wb,1, updated_vintage_tot, startRow_vt, startCol)
#------------------------------e)Plot-------------------------------------####
# Range GBV plot
plot_range <- updated_range_tot[-1,]
plot_range$GBV_formatted <- scales::number(plot_range$`GBV(€k)`, scale = 1e-6, accuracy = 0.1)
plot_range$`Range GBV` <- factor(plot_range$`Range GBV`, levels = c("0-10k", "10-50k", "50-100k","100k+"))
range_plot <- ggplot(data = plot_range, aes(x = `Range GBV`, y = `GBV(€k)`)) +
  geom_bar(stat = "identity", fill = "slateblue4") +
  geom_smooth() +
  labs(
    title = "Range GBV by Loan",
    subtitle = "Loan level",
    x = "Range GBV",
    y = "Total GBV(€)"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()
range_plot_with_labels <- range_plot +
  geom_text(aes(label = GBV_formatted, group = `Range GBV`), hjust = -0.1, vjust = 0.001)
print(range_plot_with_labels)
# Insert the Range GBV plot
insertPlot(wb, 1, width = 6, height = 4, xy = NULL, startRow = startRow_vintage,
           startCol = length(updated_vintage) + 4, fileType = "png", units = "in", dpi = 300)

# Vintage plot
plot_vintage <- updated_vintage_tot[-1,]
plot_vintage$GBV_formatted <- scales::number(plot_vintage$`GBV(€k)`, scale = 1e-6, accuracy = 0.1)
vintage_plot <- ggplot(data = plot_vintage, aes(x = `Range Vintage`, y = `GBV(€k)`)) +
  geom_bar(stat = "identity", fill = "slateblue4") +
  geom_smooth() +
  labs(
    title = "Range Vintage by Loan",
    subtitle = "Loan level",
    x = "Range Vintage",
    y = "Total GBV(€)"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()
vintage_plot_with_labels <- vintage_plot +
  geom_text(aes(label = GBV_formatted), hjust = -0.1, vjust = 0.4)
print(vintage_plot)
# Insert the Vintage plot
insertPlot(wb, 1, width = 6, height = 4, xy = NULL, startRow = startRow_vt + 3,
           startCol = length(updated_vintage_tot) + 4, fileType = "png", units = "in", dpi = 300)

# Save the workbook
saveWorkbook(wb, "Tables2.xlsx", overwrite = TRUE)
