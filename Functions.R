


fct.emp <- function(x) { 
  # base irpef (RAL - 9% contributi) 
  y <- x * 12 * (1 - 0.09)  
  # aliquote progressive 
  x.1 <- 15*10^3 
  x.2 <- 28*10^3 
  x.3 <- 50*10^3 
  p.1 <- 0.23 
  p.2 <- 0.25 
  p.3 <- 0.35 
  p.4 <- 0.43 
  # calcolo tasse 
  t.1 <- pmin(y, x.1) %>% pmax(0) * p.1 
  t.2 <- pmin(y - x.1, x.2 - x.1) %>% pmax(0) * p.2 
  t.3 <- pmin(y - x.2, x.3 - x.2) %>% pmax(0) * p.3 
  t.4 <- pmin(y - x.3) %>% pmax(0) * p.4 
  # importo netto 
  z <- (y - (t.1+t.2+t.3+t.4))/12 
  return(z) 
} 


###... Pensioner
fct.pens <- function(x) { 
  y <- x * 12  
  x.1 <- 15*10^3 
  x.2 <- 28*10^3 
  x.3 <- 50*10^3 
  p.1 <- 0.23 
  p.2 <- 0.25 
  p.3 <- 0.35 
  p.4 <- 0.43 
  t.1 <- pmin(y, x.1) %>% pmax(0) * p.1 
  t.2 <- pmin(y - x.1, x.2 - x.1) %>% pmax(0) * p.2 
  t.3 <- pmin(y - x.2, x.3 - x.2) %>% pmax(0) * p.3 
  t.4 <- pmin(y - x.3) %>% pmax(0) * p.4 
  z <- (y - (t.1+t.2+t.3+t.4))/12 
  return(z) 
  
} 
apply_fct_emp_pens <- function(data) {
  data <- data %>%
    mutate(
      income.net = case_when(
        str_detect(solvency.base,'employee-permanent') ~ sapply(income.gross, fct.emp),
        str_detect(solvency.base,'employee-temporary') ~ sapply(income.gross, fct.emp),
        str_detect(solvency.base,'employee-N/A') ~ sapply(income.gross, fct.emp),
        solvency.base == "pensioner" ~ sapply(income.gross, fct.pens),
        TRUE ~ income.net
      )
    )
  
  return(data)
}




add_age_column <- function(data) {
  data <- data %>%
    mutate(
      age = ifelse(
        !is.na(cf.piva) & nchar(cf.piva) >= 16,  # Check if cf.piva is not NA and has at least 16 characters
        with(data, {
          year_of_birth <- as.numeric(stringr::str_sub(cf.piva, start = 7L, end = 8L))
          current_year <- as.numeric(format(Sys.Date(), "%Y"))
          ifelse(
            year_of_birth >= 0 & year_of_birth <= (current_year - 2018),
            current_year - (2000 + year_of_birth),
            current_year - (1900 + year_of_birth)
          )
        }),
        NA
      )
    )
  
  return(data)
}
#EXAMPLE
#Entities <- add_age_column(Entities)

# Define the age categories based on the age column
add_age_range_column <- function(data) {
  breaks <- c(0, 25, 50, 65, 75, Inf)
  labels <- c("0-25", "25-50", "50-65", "65-75", "75+")
  result <- data %>%
    mutate(
      range.age = cut(age, breaks = breaks, labels = labels, right = FALSE)
    )
  return(result)
}
# Running example:
#Entities <- add_age_range_column(Entities)

add_sex_column <- function(data) {
  result <- data %>%
    mutate(sex = case_when(
      !is.na(type.subject) & type.subject == "individual" & as.numeric(str_sub(cf.piva, start = 10L, end = 11L)) > 40 ~ "f",
      !is.na(type.subject) & type.subject == "individual" & as.numeric(str_sub(cf.piva, start = 10L, end = 11L)) <= 40 ~ "m",
      TRUE ~ NA_character_
    ))
  return(result)
}
# example
# Entities <-   add_sex_column (Entities)

add_type_column <- function(data) {
  result <- data %>%
    mutate(type.pg = case_when(
      str_detect(name, "srl|s.r.l|s.r.l.|srls")  ~ "srl",
      str_detect(name, "d.i|d.i.")  ~ "di",
      str_detect(name, " ss |s.s|s.s.|societa' semplice")  ~ "ss",
      str_detect(name, " sas |s.a.s|s.a.s.")  ~ "sas",
      str_detect(name, "snc|s.n.c|s.n.c.|sncs")  ~ "snc",
      str_detect(name, " sc |s.c|s.c.|scs")  ~ "sc",
      TRUE ~ NA_character_
    ))
} 
# example
#Entities <- add_type_column(Entities)

#Creates a type_subject_column based on the cf.piva column
add_type_subject_column <- function(data) {
  result <- data %>%
    mutate(
      type.subject = sapply(1:nrow(data), function(i) {
        x <- data$cf.piva[i]
        y <- data$name[i]
        
        if (is.na(x) && is.na(y)) {
          return(NA)
        } else if (!is.na(y) && any(stringr::str_detect(y, "confidi|fidi"))) {
          return("confidi")
        } else if (!is.na(x) && stringr::str_length(x) == 11) {
          return("corporate")
        } else {
          return("individual")
        }
      })
    )
  
  return(result)
}
# Example
# Entities <- add_type_subject_column(Entities)


# trasform the type of credit
change_type_credit <- function(data) {
  result <- data %>%
    mutate(type = case_when(
      str_detect(type, "credit cards| carta di credito")  ~ "credit cards",
      str_detect(type, "bank accounts|conto corrente|conti correnti|scopertocc")  ~ "bank accounts",
      str_detect(type, "mutuo|mutui")  ~ "mortgages",
      str_detect(type, "mutuo fondiario")  ~ "mortgages (fondiario)",
      str_detect(type, "credito di firma")  ~ "other",
      str_detect(type, "finanziamento")~"personal loans",
      TRUE ~ "N/A"
    ))
} 
#example
#Loans_new <- change_type_credit(Loans_new)

#vintage range column
add_vintage_range_column <- function(data, date_col1, date_col2) {
  result <- data %>%
    mutate(
      date_diff = ifelse(!is.na(data[[date_col1]]) & !is.na(data[[date_col2]]),
                         as.numeric(difftime(data[[date_col1]], data[[date_col2]], units = "days")),
                         NA),
      range.vintage = cut(date_diff, 
                          breaks = c(0, 365, 730, 1095, 1825, 3650, Inf), 
                          labels = c("0y", "1y", "2y", "3-5y", "6-10y", "11-20y"),
                          right = FALSE)
    ) %>%
    select(-date_diff)
  result$range.vintage[is.na(result$range.vintage)] <- "N/A"
  return(result)
}

#gbv range column
add_gbv_range_column <- function(data) {
  breaks <- c(0, 10000, 50000,100000, Inf)
  labels <- c("0-10k", "10-50k", "50-100k", "100k+")
  result <- data %>%
    mutate(
      range.gbv = cut(gbv.original, breaks = breaks, labels = labels, right = FALSE)
    )
  return(result)
}

# Function to split values, and create two rows when there's one with multiple values divided by a separator
divide_column_by_character <- function(dataframe, column_name, separator) {
  dataframe %>%
    mutate(across({{column_name}}, ~ ifelse(is.na(.), "NA", .))) %>%
    rowwise() %>%
    separate_rows({{column_name}}, sep = separator, convert = TRUE) %>%
    mutate_all(~str_trim(., 'left')) %>% mutate_all(~str_trim(., 'right'))
}

# Running example:
# NDG_N <- divide_column_by_character(NDG_N, "Judicial.Procedures.CODE", "\\+ ")
divide_column_by_character_piva <-function(dataframe, column_names, separator) {
  dataframe %>%
    mutate(across(all_of(column_names), ~ ifelse(is.na(.), "NA", .))) %>%
    rowwise() %>%
    separate_rows(all_of(column_names), sep = separator, convert = TRUE) %>%
    mutate_all(~str_trim(., 'left')) %>%
    mutate_all(~str_trim(., 'right'))
}



clean_cf.piva <- function(column) {
  column <- gsub(" ", "", column)
  column <- ifelse(str_length(column) == 10, paste0(0, column), column)
  return(column)
}
# Running example:
#ENTITIES$cf.piva <- clean_cf.piva(ENTITIES$cf.piva)




#prbly wrong
check_values_in_column <- function(df1, df2, column_name) {
  # Check if all values in df1's column are present in df2's column
  all_values_present <- all(df1[[column_name]] %in% df2[[column_name]])
  
  if (all_values_present) {
    cat("All values in", column_name, "are present in both data frames.\n")
  } else {
    cat("Not all values in", column_name, "are present in both data frames.\n")
  }
}
# Check if all the NDG from NDG are written in the Loans table
#check_values_in_column(NDG, Loans, "NDG")

dependence_function <- function(data, data_name) {
  column_names <- names(data)
    results_df <- data.frame(Column1 = character(), Column2 = character(), Result = character(), stringsAsFactors = FALSE)
  
  check_one_to_one <- function(data, col1, col2) {
    result <- data %>%
      group_by(!!sym(col1)) %>%
      summarise(UniqueCount = length(unique(!!sym(col2))))
    
    if (all(result$UniqueCount == 1)) {
      result_text <- paste("'", col1, "' determines '", col2, "'.", sep = "")
    } else {
      result_text <- paste("No functional dependency found between '", col1, "' and '", col2, "'.", sep = "")
    }
    
    return(data.frame(Column1 = col1, Column2 = col2, Result = result_text, stringsAsFactors = FALSE))
  }
  for (i in 1:length(column_names)) {
    col1 <- column_names[i]
    for (j in 1:length(column_names)) {
      if (i != j) {
        col2 <- column_names[j]
        result <- check_one_to_one(data, col1, col2)
        results_df <- rbind(results_df, result)
      }
    }
  }
  
  print(results_df)
    results_df <- as.data.frame(results_df)
    csv_filename <- "column_dependencies.csv"
  write.csv(results_df, file = csv_filename, row.names = FALSE)
  cat("Results saved to", csv_filename, "\n")
  column_names <- names(data)
    num_columns <- length(column_names)
  data_dependency_maytrix <- matrix(NA, nrow = num_columns, ncol = num_columns)
  
  # Create a function to check for one-to-one correspondence
  check_one_to_one <- function(data, col1, col2) {
    result <- data %>%
      group_by(!!sym(col1)) %>%
      summarise(UniqueCount = length(unique(!!sym(col2))))
    
    if (all(result$UniqueCount == 1)) {
      return(1)  # Column 1 determines Column 2
    } else {
      return(0)  # No functional dependency found
    }
  }
  
  # Fill the dependency matrix
  for (i in 1:num_columns) {
    for (j in 1:num_columns) {
      if (i == j) {
        data_dependency_maytrix[i, j] <- 0  # No need to compare a column to itself
      } else {
        col1 <- column_names[i]
        col2 <- column_names[j]
        data_dependency_maytrix[i, j] <- check_one_to_one(data, col1, col2)
      }
    }
  }
  
  row.names(data_dependency_maytrix) <- column_names
  colnames(data_dependency_maytrix) <- column_names
    cat("Dependency matrix for '", data_name, "':\n")
  print(data_dependency_maytrix)
    assign(paste(data_name, "_dependency_matrix", sep = ""), data_dependency_maytrix, envir = .GlobalEnv)
}
#EXAMPLE
#dependence_function(Loans, "Loans")

primary_key <- function(data) {
  unique_columns <- character(0)
  
  for (col_name in names(data)) {
    if (length(unique(data[[col_name]])) == nrow(data)) {
      unique_columns <- c(unique_columns, col_name)
    }
  }
  
  return(unique_columns)
}
#EXAMPLE
#primary_key(NDG)
