highlight_style <- createStyle(fgFill = "#8DB4E2", border = "TopBottom",
                               borderColour ="black",borderStyle= "medium",textDecoration="bold", valign = "center")
title_style <- createStyle(fgFill = "white", border = "TopBottomLeftRight",borderColour = "black",
                           borderStyle= "medium",textDecoration="bold")
section_style <- createStyle(border = "Bottom",borderColour ="black",borderStyle= "medium")
group_style <- createStyle(fgFill = "#B7DEE8",textDecoration="bold",border = "Top")
startRow <- 3
startCol <- 2

#sec/unsec table excel 

applyStylesToColumns <- function(wb, sheet, df, column_types, startRow, startCol) {
  for (col in startCol:(startCol + ncol(df) - 1)) {
    col_type <- column_types[col - startCol + 1]  # Adjust the column index to match column_types
    rows_to_style <- startRow + 1:nrow(df)  # Start at startRow + 1 and go until the end of the data frame
    
    if (col_type == "currency") {
      addStyle(wb, sheet, style = createStyle(numFmt = "[>=1000] #,##0,\"K\";[=0]\"-\";#0"), rows = rows_to_style, cols = col, gridExpand = TRUE)
    } else if (col_type == "percentage") {
      addStyle(wb, sheet, style = createStyle(numFmt = "0.0%;[=0]\"-\""), rows = rows_to_style, cols = col, gridExpand = TRUE)
    } else if (col_type == "number"){
      addStyle(wb,sheet,style=createStyle(numFmt="0;[=0]\"-\""),rows=rows_to_style,cols=col,gridExpand = TRUE,stack = TRUE)
    }
  }
}


# Replace `df` with your data frame, and `startRow` with the appropriate start row
#applyStylesToColumns(wb, df, column_types, startRow,startCol)

# sec/unsec 
applyCustomStyles <- function(wb, sheet,df, startRow, startCol) {
  addStyle(wb, sheet, style = title_style, rows = startRow - 1, cols = startCol:(startCol + ncol(df) - 1), gridExpand = TRUE)
  
  for (row in 1:(nrow(df))) {
    if (df[row, 1] == "Bank Accounts Tot." || df[row, 1] == "Individual Tot." ||df[row, 1] == "Corporate Tot." ||
        df[row, 1] == "Mortgages Tot."|| df[row, 1] == "Personal Loans Tot."|| df[row, 1] == "Employee-Permanent Tot."
        || df[row, 1] == "Employee-Temporary Tot."|| df[row, 1] == "Pensioner Tot."|| df[row, 1] == "Insolvent Tot.") {
      addStyle(wb, sheet, style = group_style, rows = startRow + row, cols = startCol:(startCol + ncol(df) - 1), gridExpand = TRUE, stack = TRUE)
    }
  }
  
  addStyle(wb, sheet, style = highlight_style, rows = startRow + 1, cols = startCol:(startCol + ncol(df) - 1), gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet, style = section_style, rows = startRow + nrow(df), cols = startCol:(startCol + ncol(df) - 1), gridExpand = TRUE, stack = TRUE)
}

# Replace `df`, `startRow`, and `startCol` with your data frame and appropriate values
#applyCustomStyles(wb, df, startRow, startCol)

highlight_value <- createStyle(
  fgFill = "#755BBF", border = "TopBottomLeftRight",
  borderColour ="black",borderStyle= "medium",textDecoration="bold"
)
